#' @title 
#' Fit non-linear regressions by group, using LM algorithm and get different output options.
#' @description 
#' With this function it's possible to fit non-linear regressions using Levenberg-Marquardt or Gauss-Newton algorithms by a grouping variable, and get a data frame
#' with each column as a coefficient and quality of fit variables, and other output options. Works with dplyr grouping functions.
#' @details 
#' This function Levenberg-Marquardt algorithm as default for fitting non-linear regression models.
#' Also, with this function there no more need to use the \code{do} function when fitting a linear regression in a pipe line.
#' It's also possible to easily make fit multiple regressions, specifying a grouping variable.
#' In addition to that, the default output sets each coefficient as a column, making it easy to call coefficients by name or position
#' when estimating values. The Levenberg-Marquardt fit uses \code{\link[minpack.lm]{nlsLM}}.
#' 
#' @param df A data frame.
#' @param model A linear regression model, with or without quotes. The variables mentioned in the model must exist in the provided data frame. X and Y sides of the model must be separated by "~".
#' @param mod_start A vector or data frame, with start values for coefficients used in the model. This can be a data frame containing the same group variables used in the .groups argument, and the start values.
#' @param .groups Optional argument. Quoted name(s) of grouping variables used to fit multiple regressions, one for each level of the provided variable(s). Default \code{NA}.
#' @param output  Selects different output options. Can be either \code{"table"}, \code{"merge"}, \code{"merge_est"} and \code{"nest"}. See details for explanations for each option. Default: \code{"table"}.
#' @param est.name Name of the estimated y value. Used only if \code{est.name = TRUE}. Default: \code{"est"}. 
#' @param replace  If \code{TRUE}, models that don't converge on a grouped regression fit will be replaced by coefficients fitted using all data. Default: \code{FALSE}.
#' @param keep_model If \code{TRUE}, a column containing lm object(s) is kept in the output. Useful if the user desires to get more information on the regression.Default: \code{FALSE}.
#' @param global_start Optional argument. A vector or data frame, with start values for the global fit regression used when  \code{"replace"} is \code{TRUE}.
#' @param algorithm Algorithm to be used in the non-linear regression. It can be \code{"LM"} (Levenberg-Marquardt, more robust) or \code{"GN"} (Gauss-Newton, less robust, uses nls default algorithm). Default: \code{"LM"}.
#' @return  A data frame. Different data frame options are available using the output argument.
#'
#' @export
#' @examples 
#' library(forestmangr)
#' library(dplyr)
#' data("exfm14")
#' exfm14
#'
#' # Fit Chapman & Richards non-linear model for dominant Height:
#' nls_table(exfm14, dh ~ b0 * (1 - exp( -b1 * age )  )^b2, 
#'           mod_start = c( b0=23, b1=0.03, b2 = 1.3  ) )
#'
#' # Fit CR model by strata:
#' nls_table(exfm14,dh ~ b0 * (1 - exp( -b1 * age )  )^b2,
#'           mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),
#'           .groups = "strata") %>% 
#'           as.data.frame
#'           
#' # or, using group_by
#'
#' exfm14 %>% 
#' group_by(strata) %>% 
#' nls_table(dh ~ b0 * (1 - exp( -b1 * age )  )^b2,
#'           mod_start = c( b0=23, b1=0.03, b2 = 1.3  ) )
#'
#' # If there are multiple start values, for each strata, they can be supplied like so:
#' tab_coef <- data.frame(strata = c(1:20, 24,25,27,28,30,31,33,35,36,37), 
#'               rbind(
#'               data.frame(b0 = rep(23, 20), b1 = rep(0.03, 20), b2 = rep(1.3, 20) ), 
#'               data.frame(b0 = rep(23, 10), b1 = rep(0.03, 10), b2 = rep(.5, 10) )))
#' 
#' tab_coef
#' 
#' nls_table(exfm14, dh ~ b0 * (1 - exp( -b1 * age )  )^b2, 
#'           mod_start = tab_coef,
#'           .groups = "strata" )
#' # mod_start needs to be a data frame in this case.
#'
#' # It's possible to bind the coefficients to the original data,
#' # to estimate y. We'll also estimate bias and rmse for this estimation:
#' nls_table(exfm14,dh ~ b0 * (1 - exp( -b1 * age )  )^b2, 
#'           mod_start = tab_coef ,
#'           .groups = "strata", 
#'           replace = TRUE,
#'           output = "merge" ) %>% 
#'   mutate(
#'   dh_est = b0 * (1 - exp( -b1 * age )  )^b2,
#'   bias = bias_per(y = dh, yhat = dh_est),
#'   rmse = rmse_per(y = dh, yhat = dh_est) ) %>% 
#'   head(15)
#'
#' # This can also be done directly using "merge_est" as output:
#' nls_table(exfm14,dh ~ b0 * (1 - exp( -b1 * age )  )^b2, 
#'           mod_start = tab_coef ,
#'           .groups = "strata", 
#'           output = "merge_est", 
#'           est.name = "dh_est" ) %>% 
#'   mutate(
#'   bias = bias_per(y = dh, yhat = dh_est),
#'   rmse = rmse_per(y = dh, yhat = dh_est) ) %>% 
#'   head(15)
#'
#' # It's possible to further customize the output, using nested columns:
#' nls_table(exfm14,dh ~ b0 * (1 - exp( -b1 * age )  )^b2, 
#'           mod_start = tab_coef ,
#'           .groups = "strata",
#'           output = "nest" ) 
#'              
#' # It's possible to use Gauss-Newton's algorithm. In this case,
#' # some regressions will not converge.            
#'  exfm14 %>% 
#' group_by(strata) %>% 
#' nls_table(dh ~ b0 * (1 - exp( -b1 * age )  )^b2,
#'           mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),algorithm="GN" )
#'          
#' # If some regressions don't converge, it's possible to fill those NAs with
#' # regression coefficients from a general fit, using the entire data: 
#' nls_table(exfm14,dh ~ b0 * (1 - exp( -b1 * age )  )^b2, 
#'           mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),
#'           .groups = "strata",
#'           replace = TRUE,
#'           algorithm="GN" )
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}


nls_table <- function(df, model, mod_start, .groups = NA, output = "table", est.name = "est", replace = FALSE, keep_model = FALSE, global_start, algorithm="LM") {
  # ####
  dat<-Reg<-Coefs<-B<-.<-est<-data_n<-est_n<-data_na<-NULL
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
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('table', 'merge', 'merge_est', 'nest') ){ 
    stop("'output' must be equal to 'table', 'merge', 'merge_est', or 'nest' ", call. = F) 
  }
  
  if(  missing(mod_start) || any(mod_start == "") || any(is.null(mod_start)) ){  
    stop("mod_start not set. Please insert start up values for the model's parameters", call. = F) 
  }else if( is.vector(mod_start) ){
    
  }else if(is.vector(mod_start) & !is.numeric( mod_start )){
    stop( "'mod_start' must be a numeric vector or a dataframe", call.=F)
  }else if( is.data.frame(mod_start)){
    
  }else{
    stop("'mod_start' must be a numeric vector or a dataframe", call.=F)
  }
  
  # se replace nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! replace %in% c(TRUE, FALSE) ){ 
    stop("'replace' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(replace)!=1){
    stop("Length of 'replace' must be 1", call.=F)
  }
  
  mod <- stats::formula(model)
  
  # Se nenhum nome estiver no model, parar
  if(!any(names(df) %in% all.vars( mod ) ) ){
    
    stop("Variables not found. Check variable names inside model", call. = F)
    
    
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if((missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||all(.groups=="") ) && !is.null(dplyr::groups(df))){
    .groups_syms <- rlang::syms(dplyr::groups(df))
  }else if(missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||all(.groups=="")){
    .groups_syms <- character()
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
  }
  
  # Se algorithm nao for character,ou nao for de tamanho 1, parar
  if(!is.character( algorithm )){
    stop( "'algorithm' must be character", call.=F)
  }else if(length(algorithm)!=1){
    stop("Length of 'algorithm' must be 1", call.=F)
  }else if(! algorithm %in% c('GN', 'LM') ){ 
    stop("'algorithm' must be equal to 'GN' or 'LM' ", call. = F) 
  }
  
  # ####
  
  # Criar funcao customizada que retira os betas colocando cada um em uma coluna
  tidy_ <- function(x) {
    estimate<-b<-NULL
    
    tibble::tibble(b = broom::tidy(x)$term, estimate = broom::tidy(x)$estimate) %>% 
      dplyr::mutate(b = factor(b, labels = 0:(length(b) - 1))) %>% 
      tidyr::spread(b, estimate, sep = "")
  }
  
  # Criar funcao customizada que extrai os valores estimados e salva em um dataframe
  predict_ <- function(x,.groups){ 
    data.frame(est = stats::predict(x,.groups) )
  }
  
  # Criar funcao customizada que repete valores e salva em um dataframe
  rep_ <- function(x,n){
    
    data.frame(C = rep(x,n) )
    
  }
  if(algorithm=="LM"){
    nls <- minpack.lm::nlsLM
  }else if(algorithm=="GN"){
    nls <- stats::nls
  }
  # Transformar nls em uma funcao segura, que gera erros silenciosamente
  safe_nls <- purrr::safely(nls)
  
  # A seguir sera calculado a media dos coeficientes, que pdoera ser utilizada
  # e converte-se mod_start para vetor, caso ele seja uma lista
  if(is.data.frame(mod_start)){
    
    # calcular a media e converter para vetor
    mod_start_mean <-  mod_start[, - which(names(mod_start) %in% .groups ) ]
    mod_start_mean <- mod_start_mean[ , sapply(mod_start_mean , is.numeric) ]
    mod_start_mean <- mod_start_mean %>% dplyr::summarise_all(mean,na.rm=T) %>% unlist
    
  }else if(is.vector(mod_start)){
    
    mod_start_mean <- as.data.frame(t(mod_start)) %>% dplyr::summarise_all(mean,na.rm=T) %>% unlist
    
  }else if(is.list(mod_start)){
    # se mod_start for lista, converter para fator
    mod_start <- do.call(cbind,mod_start)
    # calcular a media e converter para vetor
    mod_start_mean <- as.data.frame(t(mod_start)) %>% dplyr::summarise_all(mean,na.rm=T) %>% unlist
  }else{
    stop("mod_start must be a data frame, a vector, or a list",call. = F)
  }
  
  # se verdadeiro, a regressao sera rodada para todos os dados
  # Seus coeficientes serao salvos no objeto aux
  # Se nao, aux sera um dataframe com uma coluna chamada "X1"
  if(replace == T){
    
    if(!missing(global_start)){
      # Substituir media geral se o start global for fornecido
      mod_start_mean <- global_start
      
    }
    
    aux <- df %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(B = "dummy") %>% 
      dplyr::group_by(!!rlang::sym("B")) %>% 
      tidyr::nest(.key="dat") %T>% #%>% 
      {options(warn=-1)} %>% 
      dplyr::mutate(Reg = purrr::map(dat, ~safe_nls( mod, ., mod_start_mean, na.action=na.exclude )[[1]]  )  #,
                    #  Coefs = purrr::map(Reg, tidy_)
      )  %T>% 
      {options(warn=0)}
    
    # Parar se a convergencia global nao acontecer
    if(is.null(aux$Reg[[1]])){stop("Convergence not met at global fit", call. = F)}
    
    aux <- aux %>% dplyr::mutate( Coefs = purrr::map(Reg, tidy_) )
    
    #return(aux)
    # betas auxiliares
    aux1 <- aux %>% 
      tidyr::unnest(Coefs, .drop = T) %>% 
      dplyr::select(-B) %>% 
      as.data.frame
    
    # Regressao auxiliar
    aux2 <- aux %>% 
      dplyr::pull(Reg) %>% 
      .[[1]]
    
    
    
  }else if(replace == F){
    
    # Aqui a classe de aux tem que coincidir com a classe da coluna em
    # ele sera inserido (por isso dataframe)
    aux1 <- data.frame(1)
    aux2 <- data.frame(1)
    
  }
  
  # possibly faz com que, quando se gere um erro,
  # seja possivel inserir um resultado para substitui-lo.
  
  # colocando aux como esse argumento em tidy,
  # faz-se que quando houver erro, ou seja, nao houver regressao,
  # sera utilizado aux1, que ira conter
  # os coeficientes ajustados para todos os dados ( caso replace seja verdadeiro)
  # ou um dataframe vazio ( caso replace seja falso)
  
  # No caso de nls, a regressao rodada para todos os dados sera inserida quando
  # a regressao por grupo falhar ( caso replace seja verdadeiro)
  # ou um dataframe vazio ( caso replace seja falso)
  
  # Predict ira gerar um dataframe 1x1 caso falhe. Isso sera utilizado como condicao futuramente,
  # caso replace seja FALSO. Caso replace seja verdadeiro, o erro nao ocorrera,
  # pois onde ele iria falhar, a regressao sera substituida (devido a pos_nls).
  
  pos_tidy <- purrr::possibly(tidy_ , aux1 )
  pos_nls <- purrr::possibly(nls, aux2)
  pos_predict <- purrr::possibly(predict_, data.frame(1) )
  
  
  # Se o chute inicial for um dataframe, unir os coeficientes aos dados
  if(is.data.frame(mod_start) ){
    
    # Pegar os nomes dos betas
    mod_start_names <- names( mod_start[, - which(names(mod_start) %in% .groups ) ] )
    
    # Não tem problema tentar colocar chutes iniciais que nao estejam no model,
    # nem tentar colocar nomes que nao existam.groups Por isso o model pode rodar com 2 coefs
    # ou 20
    
    suppressMessages(
      
      x <-   df %>%
        dplyr::ungroup() %>% 
        dplyr::full_join(mod_start) %>% 
        dplyr::group_by(!!!.groups_syms) %>% 
        tidyr::nest(.key="dat") %T>% #%>% 
        {options(warn=-1)} %>% 
        dplyr::mutate(Reg = purrr::map(dat,  ~pos_nls( mod, ., c( b0  = .[[ mod_start_names[01] ]][1], 
                                                                   b1  = .[[ mod_start_names[02] ]][1], 
                                                                   b2  = .[[ mod_start_names[03] ]][1], 
                                                                   b3  = .[[ mod_start_names[04] ]][1], 
                                                                   b4  = .[[ mod_start_names[05] ]][1], 
                                                                   b5  = .[[ mod_start_names[06] ]][1], 
                                                                   b6  = .[[ mod_start_names[07] ]][1], 
                                                                   b7  = .[[ mod_start_names[08] ]][1], 
                                                                   b8  = .[[ mod_start_names[09] ]][1], 
                                                                   b9  = .[[ mod_start_names[10] ]][1], 
                                                                   b10 = .[[ mod_start_names[11] ]][1], 
                                                                   b11 = .[[ mod_start_names[12] ]][1], 
                                                                   b12 = .[[ mod_start_names[13] ]][1], 
                                                                   b13 = .[[ mod_start_names[14] ]][1], 
                                                                   b14 = .[[ mod_start_names[15] ]][1], 
                                                                   b15 = .[[ mod_start_names[16] ]][1], 
                                                                   b16 = .[[ mod_start_names[17] ]][1], 
                                                                   b17 = .[[ mod_start_names[18] ]][1], 
                                                                   b18 = .[[ mod_start_names[19] ]][1], 
                                                                   b19 = .[[ mod_start_names[20] ]][1], 
                                                                   b20 = .[[ mod_start_names[21] ]][1]   ), na.action=na.exclude   ) ), 
                      Coefs = purrr::map(Reg, pos_tidy),
                      est = purrr::map2(Reg, dat, pos_predict) ) %T>% 
                      {options(warn=0)}
      
    ) #suppressMessages
  }else{  
    
    
    x <-   df %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(!!!.groups_syms) %>% 
      tidyr::nest(.key="dat") %T>% #%>% 
      {options(warn=-1)} %>% 
      dplyr::mutate(Reg = purrr::map(dat, ~pos_nls( mod, ., mod_start, na.action=na.exclude ) ),
                    Coefs = purrr::map(Reg, pos_tidy),
                    est = purrr::map2(Reg, dat, pos_predict) ) %T>% 
                    {options(warn=0)}
  }
  
  if(replace == F){
    
    x <- x %>% 
      dplyr::mutate( est_n =  purrr::map(est, nrow  ),
                     data_n =  purrr::map(dat, nrow  ),
                     data_na = purrr::map2(NA,data_n,rep_ )  ) %>% 
      tidyr::unnest(est_n,data_n) %>% 
      dplyr::mutate(est = ifelse(est_n == 1, data_na, est) ) %>% 
      dplyr::select(-est_n,-data_n,-data_na) #%>% 
    #tidyr::unnest(data,est) %>% 
    #select(-A)
    
  }
  
  
  # tirar o ln do est se o y do model tiver ln
  if( any( grepl("^LN|LOG", stats::formula(model)[2], ignore.case = T ) ) ){
    
    x <- x %>% dplyr::mutate(est = purrr::map(est, exp) )  
    
  }
  
  if (output == "table") {
    y <- tidyr::unnest(x, Coefs, .drop = F) %>% 
      dplyr::select(-dat, -est) 
    
  }
  else if (output == "merge") {
    
    # desninhar o dado e remover os chutes iniciais
    x <- x %>% tidyr::unnest(dat, .drop = F) 
    x[c("b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15","b16","b17","b18","b19","b20")] <- NULL
    
    # unir os coeficientes aos dados
    y <- x %>% 
      tidyr::unnest(Coefs) %>% 
      dplyr::select(-est)
  }
  else if(output %in% c("merge_est", "estimate")){
    
    #est ou estimate ira estimar a variavel y e uni-la aos dados originais
    y <- x %>% 
      tidyr::unnest(dat, est, .drop = F) %>% 
      dplyr::select(-est, est )   # passar est para final da tabela
    
    y[c("A","Coefs", "b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15","b16","b17","b18","b19","b20")] <- NULL
    
    
  }
  else if (output == "nest") {
    y <- x
  }
  
  # Remover variavel criada pelos erros (caso ocorram)
  y$X1 <- NULL
  # Remover o model caso o usuario deseje
  if(keep_model==F & output != "nest"){
    y$Reg <- NULL
    y <- as.data.frame(y)
    }
  # Renomear est
  names(y)[names(y)=="est"] <- est.name
  
  # Quando a regressao nao roda, a funcao nao gera erros,
  # devido as precaucoes tomadas previamente.
  # O dataframe gerado possui apenas 1 coluna quando isso ocorre.
  # Portanto, verifica-se se a regressao foi atingida ou nao com o if a seguir
  #if(length(y) <= 1 ){stop("Convergence not met. Try to use different start values",call.=F)}
  
  return(y)
  
}
