#' @title 
#' Fit linear regressions by group, and get different output options.
#' @description 
#' With this function it's possible to fit linear regressions by a grouping variable, and get a data frame
#' with each column as a coefficient and quality of fit variables, and other output options. Works with dplyr grouping functions.
#' @details 
#' With this function there's no more need to use the \code{do} function when fitting a linear regression in a pipe line.
#' It's also possible to easily make fit multiple regressions, specifying a grouping variable.
#' In addition to that, the default output sets each coefficient as a column, making it easy to call coefficients by name or position
#' when estimating values. 
#' 
#' It's possible to use the \code{output} argument to get a merged table if \code{output="merge"}, that binds
#' the original data frame and the fitted coefficients. 
#' If \code{output="merge_est"} we get a merged table as well, but with y estimated using the coefficients. If the fit is made using groups, this is taken into account, i.e. the estimation is made by group.
#' 
#' If \code{output="nest"}, a data frame with nested columns is provided. This can be used if the user desires to get a customized output.
#'
#' @param df A data frame.
#' @param model A linear regression model, with or without quotes. The variables mentioned in the model must exist in the provided data frame. X and Y sides of the model must be separated by "~".
#' @param .groups Optional argument. Quoted name(s) of grouping variables used to fit multiple regressions, one for each level of the provided variable(s). Default: \code{NA}.
#' @param output  Selects different output options. Can be either \code{"table"}, \code{"merge"}, \code{"merge_est"} and \code{"nest"}. See details for explanations for each option. Default: \code{"table"}.
#' @param est.name Name of the estimated y value. Used only if \code{est.name = TRUE}. Default: \code{"est"}. 
#' @param keep_model If \code{TRUE}, a column containing lm object(s) is kept in the output. Useful if the user desires to get more information on the regression. Default: \code{FALSE}.
#' @param rmoutliers If \code{TRUE}, outliers are filtered out using the IQR method. Default: \code{FALSE}.
#' @param fct_to_filter Name of a factor or character column to be used as a filter to remove levels. Default: \code{NA}.
#' @param rmlevels Levels of the fct_to_filter variable to be removed from the fit Default: \code{NA}.
#' @param boolean_filter Name of a Boolean column to be used as a filter to remove data. Default: \code{NA}.
#' @param onlyfiteddata If \code{TRUE}, the output data will be the same as the fitted (and possibly filtered) data. Default: \code{FALSE}.
#' @param del_boolean If \code{TRUE}, the Boolean column supplied will be deleted after use. Default: \code{FALSE}.
#' @return  A data frame. Different data frame options are available using the output argument.
#' 
#' @export
#' @examples 
#' library(forestmangr)
#' library(dplyr)
#' 
#' data("exfm19")
#' head(exfm19)
#' 
#' # Fit Schumacher and Hall model for volume estimation, and get
#' # coefficient, R2 and error values:
#' 
#' lm_table(exfm19, log(VWB) ~  log(DBH) + log(TH))   
#' 
#' # Fit SH model by group:
#' lm_table(exfm19, log(VWB) ~  log(DBH) + log(TH), "STRATA")
#' 
#' # This can also be done using dplyr::group_by:
#' exfm19 %>% 
#'   group_by(STRATA) %>% 
#'   lm_table(log(VWB) ~  log(DBH) + log(TH) )
#'   
#' # It's possible to merge the original data with the table containg the coefficients
#' # using the output parameter:
#' fit <- lm_table(exfm19, log(VWB) ~  log(DBH) + log(TH), "STRATA", output = "merge")
#' head(fit)
#' 
#' # It's possible to merge the original data with the table,
#' # and get the estimated values for this model:
#' fit <- lm_table(exfm19, log(VWB) ~  log(DBH) + log(TH),"STRATA",
#'  output = "merge_est", est.name = "VWB_EST") 
#' head(fit)
#'        
#' # It's possible to further customize the output,
#' # unnesting the nested variables provided when output is defined as "nest":
#' lm_table(exfm19, log(VWB) ~  log(DBH) + log(TH),"STRATA", output = "nest")
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

lm_table <- function(df, model, .groups = NA, output = "table", est.name = "est", keep_model = FALSE,rmoutliers = FALSE,fct_to_filter=NA,rmlevels=NA,boolean_filter=NA,onlyfiteddata=FALSE,del_boolean=FALSE){
  # ####
  dat<-Reg<-.<-est<-Coefs<-Qualid<-Res<-outliers_check<-fitdata<-NULL
  # Checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se model nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(model) ){  
    stop("model not set", call. = F) 
  }else if(is.character(model)){
    model <- stats::as.formula(model)
  }else if(!methods::is(model, "formula") ){
    stop("'model' must be a character or a formula containing a model", call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  # se nao for fornecido, criar tambem variavel com todos os nomes, para usar em nest
  if((missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||all(.groups=="") ) && !is.null(dplyr::groups(df))){
    .groups_syms <- rlang::syms(dplyr::groups(df))
    notgpvars <- names(df)[!names(df)%in% dplyr::groups(df) ]
  }else if(missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||all(.groups=="")){
    .groups_syms <- character()
    notgpvars <- names(df)
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(forestmangr::check_names(df,.groups)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,.groups, boolean=F), call.=F)
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    .groups_syms <- rlang::syms(.groups)
    notgpvars <- names(df)[!names(df)%in%.groups]
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('table', 'merge', 'merge_est', 'nest') ){ 
    stop("'output' must be equal to 'table', 'merge', 'merge_est', or 'nest' ", call. = F) 
  }
  
  # se keep_model nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! keep_model %in% c(TRUE, FALSE) ){ 
    stop("'keep_model' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(keep_model)!=1){
    stop("Length of 'keep_model' must be 1", call.=F)
  }
  

  
  # se del_boolean nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! del_boolean %in% c(TRUE, FALSE) ){ 
    stop("'del_boolean' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(del_boolean)!=1){
    stop("Length of 'del_boolean' must be 1", call.=F)
  }
   
  # ####
  
  #Extrair o y do model
  Y <- all.vars( stats::formula(model)[[2]] )
  
  # converte model para formula
  mod <- stats::formula(model)
  
  tidy_ <- function(x){
    estimate<-b<-NULL
    
    tibble::tibble(b=broom::tidy(x)$term, # criamos um data frame que tem apenas as colunas dos betas e seus valores
                   estimate=broom::tidy(x)$estimate ) %>% 
      dplyr::mutate(b = factor(0:(length(b)-1) , labels=0:(length(b)-1) ) ) %>% # mudamos os nomes dos coeficientes para bn
      tidyr::spread( b, estimate,sep="" )     # com spread deixamos a tabela horizontal, colocando cada coeficiente em uma coluna
    
  }
  
  glance_ <- function(x){ 
    
    tibble::tibble( 
      Rsqr      = broom::glance(x)$r.squared,
      Rsqr_adj  = broom::glance(x)$adj.r.squared, # criamos um data table que tem apenas as variaveis R quadrado ajustado e erro padrao
      Std.Error = broom::glance(x)$sigma ) }
  
  # Separar dados entre dado de ajuste, e dado original
  df <- df %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by(!!!.groups_syms ) %>%
    tidyr::nest( dat = tidyselect::any_of(notgpvars) ) %>% 
    dplyr::mutate(fitdata=dat) # fazer copia para ser filtrada, etc
  
  # remover outliers
  if(rmoutliers){
    df <- df %>% 
      dplyr::mutate(
        outliers_check=purrr::map(fitdata, ~outliersiqr(.x[[Y]])  ),
        fitdata = purrr::map2(fitdata, outliers_check,
                              ~dplyr::filter(.x,.data[[Y]] >= .y$lowlimit & .data[[Y]] <= .y$uplimit) ) ) %>% 
      dplyr::select(-outliers_check)
  }
  
  # Filtrar dados selecionados
  if( !all(is.na(rmlevels),is.na(fct_to_filter) )){
    df <- df %>% 
      dplyr::mutate(
        fitdata = purrr::map(fitdata,
                             ~dplyr::filter(.x,! .data[[fct_to_filter]] %in% rmlevels) ) )
  }
  
  # Se boolean_filter nao for character,ou nao for de tamanho 1, parar
  if(missing(boolean_filter) || is.null(boolean_filter) || is.na(boolean_filter) || boolean_filter == "" ){
    #df$boolean_filter <- TRUE
  }else if(!is.character( boolean_filter )){
    stop( "'boolean_filter' must be character", call.=F)
  }else if(length(boolean_filter)!=1){
    stop("Length of 'boolean_filter' must be 1", call.=F)
  }else{
    
    df <- df %>% 
      dplyr::mutate(
        fitdata = purrr::map(fitdata,
                             ~dplyr::filter(.x, .data[[boolean_filter]] ) ) )
    
  }
  
  
  if(onlyfiteddata){
    # se o usuario quiser visualizar apenas os dados utilizados
    # manter apenas os dados alterados e remover o original
    df <- df %>% 
      dplyr::mutate(dat=fitdata)
  }
  
  # Aqui e onde a regressao e realmente feita
  x <- df %>% 
    dplyr::mutate(Reg = purrr::map(fitdata, ~stats::lm(mod, data =., na.action = na.exclude ) ),
                  Coefs  = purrr::map(Reg, tidy_   ),
                  Qualid = purrr::map(Reg, glance_ ),
                  Res = purrr::map(Reg, stats::resid),
                  est = purrr::map2(Reg, dat, stats::predict) ) %>% 
    dplyr::ungroup()
  
  x$A <- NULL 
  
  # tirar o ln do est se o y do model tiver ln
  if( any( grepl("^LN|LOG", stats::formula(model)[2], ignore.case = T ) ) ){
    
    x <- x %>% dplyr::mutate(est = purrr::map(est, exp) ) 
    
  }
  
  #apagar a coluna criada la no lm_resid
  if(del_boolean){
    
    x <- x %>% 
      dplyr::mutate(
        dat = purrr::map(dat,
                             ~dplyr::select(.x, -.data[[boolean_filter]] ) ) )
    
  }
  
  
  
  if(output == "table"){
    
    # table ira resultar em uma tabela com os coeficientes e as variaveis de qualidade
    y <-  x %>% 
      tidyr::unnest(c(Coefs, Qualid) ) %>% 
      dplyr::select(-fitdata,-dat,-Res,-est)
    
    
  }else if(output == "merge"){
    
    # Merge ira unir os coeficientes aos dados originais
    y <- x %>% 
      tidyr::unnest(dat) %>% 
      tidyr::unnest(c(Coefs, Qualid) ) %>% 
      dplyr::select(-fitdata,-est, -Res)
    
  }else if(output %in% c("merge_est","est", "estimate")){
    
    #est ou estimate ira estimar a variavel y e uni-la aos dados originais
    y <- x %>% 
      tidyr::unnest(c(dat, est) ) %>% 
      dplyr::select(-fitdata,- dplyr::one_of( c("Coefs", "Qualid", "Res", "est" )), est )   # passar est para final da tabela
    
  }else if( output == "nest" ){
    
    # nest ira retornar uma tibble com varias list-columns
    y <- x
    
  }
  
  # Remover o model caso o usuario deseje
  if(keep_model==F & output != "nest"){
    y$Reg <- NULL
    y <- as.data.frame(y)
  }
  # Renomear est
  names(y)[names(y)=="est"] <- est.name
  
  return(y)
  
}
