#' @title 
#' Simple Random Sampling
#' @description 
#' Function for processing forest inventory data using simple random sampling. 
#' @details 
#' This function allows the user to processes inventory data using simple random sampling for finite or infinite populations.
#' It's possible to run multiple sampling analysis using a factor variable indicated in the \code{.groups}() parameter.
#'
#' @param df a data frame.
#' @param Yi Quoted name of the volume variable, or other variable one desires to evaluate, in quotes.
#' @param plot_area Quoted name of the plot area variable, or a numeric vector with the plot area value. The plot area value must be in square meters.
#' @param total_area Quoted name of the total area variable, or a numeric vector with the total area value.The total area value must be in hectares.
#' @param .groups Optional argument. Quoted name(s) of additional grouping variable(s) that, if supplied, will be used to run multiple surveys, one for each level. 
#' If this argument is \code{NA}, the defined groups in the data frame will be used, if they exist. Default: \code{NA}.
#' @param age Optional parameter. Quoted name of the age variable. Calculates the average age supplied. \code{NA}.
#' @param alpha Numeric value for the significance value used in the t-student estimation. Default: \code{0.05}.
#' @param error Numeric value for the minimum admitted error value in the survey, in percentage. Default: \code{10}.
#' @param dec_places Numeric value for the number of decimal places to be used in the output tables. Default: \code{4}.
#' @param pop Character value for the type of population considered in the calculations. This can be either infinite (\code{"inf"}) or finite (\code{"fin"}). Default: \code{"inf"}.
#' @param tidy Boolean value that defines if the output tables should be tidied up or not. Default: \code{TRUE}.
#' @return A data frame with the sampling results.
#' 
#' @keywords Simple Random Sampling
#' @references 
#' Campos, J. C. C. and Leite, H. G. (2017) Mensuracao Florestal: Perguntas e Respostas. 5a. Vicosa: UFV.
#' 
#' Soares, C. P. B., Paula Neto, F. and Souza, A. L. (2012) Dendrometria e Inventario Florestal. 2nd ed. Vicosa: UFV.
#' 
#' @seealso other sampling functions: 
#'   \code{\link{strs}} for stratified random sampling, and
#'   \code{\link{ss_diffs}} for Systematic Sampling.
#' @export
#' @examples
#' library(forestmangr)
#' data("exfm2")
#' data("exfm3")
#' data("exfm4")
#' 
#' # The objective is to sample an area, with an error of 20%.
#' # First we run a pilot inventory, considering a 20% error and a finite population:
#' exfm3
#' 
#' sprs(exfm3, "VWB", "PLOT_AREA", "TOTAL_AREA", error = 20, pop = "fin")
#'
#' # With these results, in order to obtain the desired error, we'll need to sample new 
#' # plots, and run the definitive inventory. Again, we aim for a 20% error, and consider
#' # the population as finite:
#' exfm4
#' 
#' sprs(exfm4, "VWB", "PLOT_AREA", "TOTAL_AREA", error = 20, pop = "fin")
#'
#' # The desired error was met
#' 
#' # area values can be numeric
#' sprs(exfm4, "VWB", 3000, 46.8, error = 20, pop = "fin")
#' 
#' # Here we run a simple random sampling inventory for each forest subdivision,
#' # using the STRATA variable as a group variable:
#' exfm2
#' 
#' sprs(exfm2, "VWB", "PLOT_AREA", "STRATA_AREA",.groups = "STRATA" ,error = 20, pop = "fin")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

sprs <- function(df,Yi, plot_area, total_area, age=NA, .groups=NA, alpha = 0.05, error = 10, dec_places=4, pop="inf",tidy=TRUE){
  # ####
  n<-VC<-N<-t_rec<-Sy<-Abserror<-Y<-Yhat<-Total_Error<-VC<-NULL
  # checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop( "length and number of rows 'df' must be greater than 1", call.=F)
  }
  
  # se Yi nao for fornecido, nao for character, ou nao for um nome de variavel, parar
  if(  missing(Yi) || Yi == ""){  
    stop("Yi not set", call. = F) 
  }else if( !is.character(Yi) ){
    stop("'Yi' must be a character containing a variable name", call.=F)
  }else if(length(Yi)!=1){
    stop("length of 'Yi' must be 1", call.=F)
  }else if(forestmangr::check_names(df, Yi)==F){
    stop(forestmangr::check_names(df, Yi, boolean = F), call.=F)
  }
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao for um nome de variavel, parar
  if(  missing(plot_area) || plot_area == "" ){  
    stop("plot_area not set", call. = F) 
  }else if(is.numeric(plot_area) & length(plot_area)==1){
    df$plot_area <- plot_area
    plot_area <- "plot_area"
  }else if(!is.character(plot_area)){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot_area)!=1){
    stop("length of 'plot_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, plot_area)==F){
    stop(forestmangr::check_names(df, plot_area, boolean = F), call.=F)
  }
  
  # se total_area nao for fornecido, nao for numerico nem character,  ou nao for um nome de variavel, parar
  if(  missing(total_area) || total_area == "" ){  
    stop("total_area not set", call. = F) 
  }else if(is.numeric(total_area) & length(total_area)==1){
    df$total_area <- total_area
    total_area <- "total_area"
  }else if(!is.character(total_area)){
    stop("'total_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(total_area)!=1){
    stop("length of 'total_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, total_area)==F){
    stop(forestmangr::check_names(df, total_area, boolean = F), call.=F)
  }
  
  # se age nao for fornecido, for igual "", nulo, nao existir no dataframe, criar
  # variavel vazia
  # se existir e nao for character,  parar
  if(missing(age)||is.null(age)||is.na(age)||age==""){
    df$age <- NA
    age <- "age"
  }else if(!is.character(age)){
    stop("'age' must be a character containing a variable name", call.=F)
  }else if(length(age)!=1){
    stop("length of 'age' must be 1", call.=F)
  }else if(forestmangr::check_names(df, age)==F){
    stop(forestmangr::check_names(df, age, boolean = F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar error
  if(missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||any(.groups=="") ){
    .groups_syms <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call.=F)
  }else if(!length(.groups) %in% 1:10){
    stop("length of '.groups' must be between 1 and 10", call.=F)
  }else if(forestmangr::check_names(df,.groups)==F ){
    stop(forestmangr::check_names(df,.groups, boolean=F), call.=F)
  }else{
    .groups_syms <- rlang::syms(.groups)
  }
  
  # Se alpha nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( alpha )){
    stop( "'alpha' must be numeric",call.=F)
  }else if(length(alpha)!=1){
    stop("length of 'alpha' must be 1",call.=F)
  }else if(! alpha > 0 | ! alpha <= 0.30){
    stop("'alpha' must be a number between 0 and 0.30", call.=F)
  }
  
  # Se error nao for numerico, parar
  if(!is.numeric( error )){
    stop( "'error' must be numeric", call.=F )
  }else if(length(error)!=1){
    stop("length of 'error' must be 1",call.=F)
  }else if(!error > 0 | !error <= 20){
    stop("'error' must be a number between 0 and 20", call.=F)
  }
  
  # Se dec_places nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( dec_places )){
    stop( "'dec_places' must be numeric", call.=F)
  }else if(length(dec_places)!=1){
    stop("length of 'dec_places' must be 1",call.=F)
  }else if(! dec_places %in% seq(from=0,to=9,by=1) ){
    stop("'dec_places' must be a integer between 0 and 9", call.=F)
  }
  
  # Se pop nao for character ou for maior que 1, parar
  if(!is.character( pop )){
    stop( "'pop' must be character", call.=F)
  }else if(length(pop)!=1){
    stop( "length of 'pop' must be 1", call.=F)
  }else if( ! pop %in% c("fin", "inf" ) ){ 
    stop("'pop' must be equal to 'fin' or 'inf' ", call. = F) 
  }
  
  # se tidy nao for igual a TRUE ou FALSE, parar
  if( is.null(tidy) || ! tidy %in% c(TRUE, FALSE) ){ 
    stop("tidy must be equal to TRUE or FALSE", call. = F) 
  }else if(length(tidy)!=1){
    stop( "length of 'tidy' must be 1", call.=F)
  }
  
  # Transformar os objetos em simbolos, para que o dplyr entenda
  # e procure o nome das variaveis dentro dos objetos
  Yi_sym <- rlang::sym(Yi)
  plot_area_sym <- rlang::sym(plot_area)
  total_area_sym <- rlang::sym(total_area)
  age_sym <- rlang::sym(age)
  
  # ####
  
  x_ <-df %>%
    dplyr::na_if(0) %>%
    dplyr::group_by(!!!.groups_syms,.add=T) %>%
    dplyr::summarise(
      age        = mean(!!age_sym,na.rm=T), # usa-se media pois os valores estao repetidos
      n            = dplyr::n() , # número de amostras
      N            = mean(!!total_area_sym,na.rm=T) / ( mean(!!plot_area_sym,na.rm=T)/10000 ), 
      VC           = stats::sd(!!Yi_sym,na.rm=T) / mean(!!Yi_sym,na.rm=T) * 100, # Calculo do coeficiente de variacao
      t            = stats::qt(alpha/2, df = n-1, lower.tail = FALSE) ,
      t_rec        = ifelse(pop=="inf", 
                            stats::qt(alpha/2, df = ceiling( t^2 * VC^2 / error^2) - 1, lower.tail = FALSE)  ,
                            stats::qt(alpha/2, df = ceiling( t^2 * VC^2 / ( error^2 +(t^2 * VC^2 / N) ) ) - 1, lower.tail = FALSE) ) ,
      n_recalc     = ifelse(pop=="inf",
                            ceiling( t_rec ^2 * VC^2 / error^2 ) ,
                            ceiling( t_rec ^2 * VC^2 / ( error^2 +(t_rec^2 * VC^2 / N) ) ) ),
      S2           = stats::var(!!Yi_sym,na.rm=T), #Variancia
      sd           = stats::sd(!!Yi_sym,na.rm=T), # desvio padrao
      Y            = mean(!!Yi_sym, na.rm=T), # Media do volume
      Sy           = ifelse(pop=="inf", 
                            sqrt( stats::var(!!Yi_sym,na.rm=T)/n ), 
                            sqrt( stats::var(!!Yi_sym,na.rm=T)/n  * (1 - (n/N)) ) ),
      Abserror      = Sy * t , # Erro Absoluto
      Percerror     = Abserror / Y * 100 , # Erro Percentual
      Yhat         = Y * N, # Media estimada para area total
      Total_Error   = Abserror * N, # Erro EStimado Para area Total
      CI_Inf       = Y - Abserror, # Intervalo de confianca inferior
      CI_Sup       = Y + Abserror, # Intervalo de confianca superior
      CI_ha_Inf    = (Y - Abserror)*10000/mean(!!plot_area_sym,na.rm=T), # Intervalo de confianca por ha inferior
      CI_ha_Sup    = (Y + Abserror)*10000/mean(!!plot_area_sym,na.rm=T), # Intervalo de confianca por ha superior
      CI_Total_inf = Yhat - Total_Error, # Intervalo de confianca total inferior
      CI_Total_Sup = Yhat + Total_Error) %>% # Intervalo de confianca total superior
    dplyr::na_if(0) %>% # substitui 0 por NA
    rm_empty_col %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    forestmangr::round_df(dec_places)
  
  
  x <- x_ %>% 
    plyr::rename(c( "age"          = "Age"                  , 
                    "n"            = "Total number of sampled plots (n)",
                    "N"            = "Number of maximum plots (N)", 
                    "t"            = "t-student"                      ,
                    "t_rec"        = "recalculated t-student", 
                    "n_recalc"     = "Number of samples regarding the admited error",
                    "S2"           = "Variance (S2)",
                    "sd"           = "Standard deviation (s)",
                    "VC"           = "Variance Quoeficient (VC)", 
                    "Y"            = "Mean (Y)"                ,
                    "Sy"           = "Standard error of the mean (Sy)",
                    "Abserror"     = "Absolute Error" ,
                    "Percerror"    = "Relative Error (%)",
                    "Yhat"         = "Estimated Total Value (Yhat)", 
                    "Total_Error"  = "Total Error",
                    "CI_Inf"       = "Inferior Confidence Interval (m3)" ,
                    "CI_Sup"       = "Superior Confidence Interval (m3)",
                    "CI_ha_Inf"    = "Inferior Confidence Interval (m3/ha)" ,
                    "CI_ha_Sup"    = "Superior Confidence Interval (m3/ha)",
                    "CI_Total_inf" = "inferior Total Confidence Interval (m3)",
                    "CI_Total_Sup" = "Superior Total Confidence Interval (m3)"),
                 warn_missing = F) # nao gera erro mesmo quando se renomeia variaveis inexistentes
  
  if(tidy==F)
  {
    return(x_)
  } 
  else if(tidy==T & length(.groups_syms)==0 )
  {
    #x <- data.frame(Variables = names(x), Values = t(x) )
    #rownames(x) <- NULL
    # ou
    x <- tibble::rownames_to_column(data.frame("Values"=t(x)) , "Variables" ) 
    
    return(as.data.frame(x))
  }
  else
  {
    # Primeiro cria-se um vetor que contem os nomes de todas as variaveis criadas anteriormente
    # exceto as variaveis de grupo
    all_but_group_vars <- rlang::syms(names(x)[! names(x) %in% .groups ])
    
    # Aqui identifica-se a ultima variavel de grupo colocada pelo usuario.
    # Esta sera usada para espalhar os dados por coluna. Ou seja,
    # Cada nivel deste fator vai virar uma coluna dos dados
    last_group_var <- rlang::sym(.groups[length(.groups)])
    
    y <- x %>%
      tidyr::gather("Variables","value", !!!all_but_group_vars, factor_key=T ) %>% #juntar todo mundo menos as variaveis de grupo
      dplyr::arrange(!!! .groups_syms ) %>%  # organiza os dados (meio desnecessario, mas ok)
      tidyr::spread(!!last_group_var,"value",sep="") %>%  # Colocar cada talhao(por exemplo) em um coluna, espalhando-o pela tabela de forma horizontal
      dplyr::ungroup() # 'desgrupificar' o dado
    
    
    return(as.data.frame(y))
    
  }
  
}
