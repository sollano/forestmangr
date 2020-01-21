#' @title 
#' Stratified Random Sampling
#' @description 
#' Function for processing forest inventory data using stratified random sampling. 
#' @details 
#' This function allows the user to processes inventory data using stratified random sampling for n forest subdivisions (strata),
#' for finite or infinite populations. It's possible to run multiple sampling analysis using a factor variable indicated in the
#' \code{.groups}() parameter.
#'
#' @param df a data frame.
#' @param Yi Quoted name of the volume variable, or other variable one desires to evaluate, in quotes.
#' @param plot_area Quoted name of the plot area variable, or a numeric vector with the plot area value. The plot area value must be in square meters.
#' @param strata_area Quoted name of the strata area variable, or a numeric vector with the plot strata values. If there are more than 1 area values, it's possible to use a vector with all area values, like so:\code{c(14.4, 16.4, 14.2)}. The strata area values must be in hectares.
#' @param strata Quoted name of the subdivision variable(s), also known as strata. If this argument is not supplied, the defined groups in the data frame will be used, if they exist.
#' @param .groups Optional argument. Quoted name(s) of additional grouping variable(s) that, if supplied, will be used to run multiple surveys, one for each level. 
#' If this argument is \code{NA}, the defined groups in the data frame will be used, if they exist. Default: \code{NA}.
#' @param age Optional parameter. Quoted name of the age variable. Calculates the average age supplied. \code{NA}.
#' @param alpha Numeric value for the significance value used in the t-student estimation. Default: \code{0.05}.
#' @param error Numeric value for the minimum admitted error value in the survey, in percentage. Default: \code{10}.
#' @param dec_places Numeric value for the number of decimal places to be used in the output tables. Default: \code{4}.
#' @param pop Character value for the type of population considered in the calculations. This can be either infinite (\code{"inf"}) or finite (\code{"fin"}). Default: \code{"inf"}.
#' @param tidy Boolean value that defines if the output tables should be tidied up or not. Default: \code{TRUE}.
#' @return A list containing two data frames, one with informations for each strata, and one with the stratified sampling results.
#' 
#' @keywords Stratified Random Sampling
#' @references 
#' Campos, J. C. C. and Leite, H. G. (2017) Mensuracao Florestal: Perguntas e Respostas. 5a. Vicosa: UFV.
#' 
#' Soares, C. P. B., Paula Neto, F. and Souza, A. L. (2012) Dendrometria e Inventario Florestal. 2nd ed. Vicosa: UFV.
#' 
#' @seealso other sampling functions: 
#'   \code{\link{sprs}} for Simple Random Sampling, and
#'   \code{\link{ss_diffs}} for Systematic Sampling.
#' @export
#' @examples
#' library(forestmangr)
#' data("exfm1")
#' data("exfm2")
#' data("exfm6")
#' 
#' # The objective is to sample an area, with an error of 5%.
#' # First we run a pilot inventory, considering a 5% error and a finite population:
#' exfm1
#' 
#' strs(exfm1, "VWB", "PLOT_AREA", "STRATA_AREA", strata = "STRATA", error = 5, pop = "fin")
#'
#' # With these results, in order to meet the desired error of 5%, we'll need to sample 24 more plots,
#' # 4 in stratum 1, 8 in stratum 2, and 12 in stratum 3.
#' # After sampling the necessary plots, we now run a definitive inventory,
#' # considering an 5% error and a finite population:
#' exfm2
#' 
#' strs(exfm2, "VWB", "PLOT_AREA", "STRATA_AREA", strata = "STRATA", error = 5, pop = "fin")
#'
#' # The desired error value was met.
#' 
#' # Area values can be numeric:
#' strs(exfm2, "VWB", 1000, c(14.4, 16.4,14.2), strata = "STRATA", error = 5, pop = "fin")
#'
#' # Optional variable age, and one stratified sampled inventory for each map:
#' exfm6
#' 
#' strs(exfm6, "VWB", "PLOT_AREA", "STRATA_AREA", strata ="STRATA", .groups = "MAP", age = "AGE")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

strs <- function(df, Yi, plot_area, strata_area, strata, .groups=NA, age=NA, alpha = 0.05, error = 10, dec_places = 4, pop="inf", tidy=TRUE ){
  # ####
  Nj<-N<-Pj<-Yj<-Pj_Sj2<-Pj_Sj<-Pj_Yj<-EPj_Sj<-Y<-nj<-EPj_Sj2<-t_rec<-n_recalc<-nj_optimal<-Sy<-Abserror<-AREA_PC<-Yhat<-Total_Error<-NULL
  # Checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se Yi nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(Yi) || Yi == "" ){  
    stop("Yi not set", call. = F) 
  }else if( !is.character(Yi) ){
    stop("'Yi' must be a character containing a variable name", call.=F)
  }else if(length(Yi)!=1){
    stop("Length of 'Yi' must be 1", call.=F)
  }else if(forestmangr::check_names(df, Yi)==F){
    stop(forestmangr::check_names(df, Yi, boolean=F), call.=F)
  }
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot_area) ){  
    stop("plot_area not set", call. = F) 
  }else if( is.null(plot_area) || is.na(plot_area) || plot_area == "" ){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(plot_area) & length(plot_area)==1){
    df $ plot_area <- plot_area
    plot_area <- "plot_area"
  }else if(!is.character(plot_area)){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot_area)!=1){
    stop("Length of 'plot_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, plot_area)==F){
    stop(forestmangr::check_names(df, plot_area, boolean = F), call.=F)
  }
  
  # se strata_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(strata_area) ){  
    stop("strata_area not set", call. = F) 
  }else if( any(is.null(strata_area)) || any(is.na(strata_area)) || all(strata_area == "") ){
    stop("'strata_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(strata_area) & length(strata_area)==1){
    df $ strata_area <- strata_area
    strata_area <- "strata_area"
  }else if(is.numeric(strata_area) & length(strata_area)>1){
    
    # Se area do estrato for fornecida numericamente, e for maior que uma area,
    # temos que criar uma tabela com os nomes dos talhoes e suas areas, e juntar com os dados originais.
    estrato_name <- strata
    estratos <- levels(factor(df[[estrato_name]]))
    
    if(!all.equal(length(estratos), length(strata_area))){stop("numero de estratos e numero de areas de estrato nao coincidem")}
    
    
    tab_estratos <- data.frame( estratos, strata_area)
    
    strata_area <- "strata_area"
    
    names(tab_estratos) <- c(estrato_name, "strata_area")
    
    df[[estrato_name]] <- as.factor(df[[estrato_name]] )
    df <- dplyr::left_join(df, tab_estratos, by = estrato_name)
    
  }else if(!is.character(strata_area)){
    stop("'strata_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(strata_area)!=1){
    stop("Length of 'strata_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, strata_area)==F){
    stop(forestmangr::check_names(df, strata_area, boolean = F), call.=F)
  }
  
  # se age nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(age) || is.null(age) || is.na(age) || age == "" ){
    df$age <- NA
    age <- "age"
  }else if(!is.character(age)){
    stop("'age' must be a character containing a variable name", call.=F)
  }else if(length(age)!=1){
    stop("Length of 'age' must be 1", call.=F)
  }else if(forestmangr::check_names(df, age)==F){
    stop(forestmangr::check_names(df, age, boolean=F), call.=F)
  }
  
  # Se strata nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(strata) && is.null(dplyr::groups(df)) ){
    stop("strata not set. strata must be set if data doesn't have any groups", call. = F)
  }else if(missing(strata) && !is.null(dplyr::groups(df)) ){
    strata_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(strata)){
    stop("strata must be a character", call. = F)
  }else if(! length(strata)%in% 1:10){
    stop("Length of 'strata' must be between 1 and 10", call.=F) 
  }else if(forestmangr::check_names(df,strata)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,strata, boolean=F), call.=F) 
  }else{
    strata_syms <- rlang::syms(strata) 
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||any(.groups=="") ){
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
  
  # Se alpha nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( alpha )){
    stop( "'alpha' must be numeric",call.=F)
  }else if(length(alpha)!=1){
    stop("length of 'alpha' must be 1",call.=F)
  }else if(! alpha > 0 | ! alpha <= 0.30){
    stop("'alpha' must be a number between 0 and 0.30", call.=F)
  }
  
  # Se erro nao for numerico, parar
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
  
  # ####
  
  Yi_sym <- rlang::sym(Yi)
  plot_area_sym <- rlang::sym(plot_area)
  strata_area_sym <- rlang::sym(strata_area)
  age_sym <- rlang::sym(age)
  
  # Calcula-se o N separado, para caso se tenha diferentes tamanhos de area por talhao
  aux <- df %>%
    dplyr::na_if(0) %>%
    dplyr::group_by( !!!.groups_syms, !!!strata_syms ) %>%
    dplyr::summarise( Nj = mean(!!strata_area_sym) / (mean(!!plot_area_sym)/10000) ) %>%
    dplyr::summarise(N  = sum(Nj) )
  
  # Se tiver apenas linha, ou seja, se tiver apenas um talhao, cbind
  if(nrow(aux) == 1) {
    
    x_ <- cbind(as.data.frame(df),N = aux$N)
    
  }else{
    # se tiver mais de uma linha, ou seja, varios talhoes,
    # unir as areas aos dados originais utilizando join
    x_ <- dplyr::left_join(as.data.frame(df),aux, by = .groups )
    
  }
  
  x_ <- x_ %>% 
    dplyr::mutate(Nj = (!!strata_area_sym ) / ( (!!plot_area_sym)/10000 ) ) %>%
    dplyr::group_by( !!!.groups_syms, !!!strata_syms) %>%
    dplyr::summarise(
      AGE  = mean(!!age_sym),
      AREA_PC= mean(!!plot_area_sym),
      nj     = dplyr::n() ,
      n      = nrow(x_),
      Nj     = mean(Nj),
      N      = mean(N),
      Pj     = Nj/N,
      Eyj    = sum(!!Yi_sym),
      Eyj2   = sum((!!Yi_sym)^2),
      Yj     = mean(!!Yi_sym, na.rm=T),
      Pj_Sj2 = Pj * stats::var(!!Yi_sym, na.rm=T),
      Pj_Sj  = Pj * stats::sd(!!Yi_sym, na.rm=T),
      Pj_Yj  = Pj * Yj ) %>%
    # ungroup %>%
    dplyr::mutate(
      EPj_Sj2  =   sum(Pj_Sj2), 
      EPj_Sj   =   sum(Pj_Sj), 
      VC       = EPj_Sj / sum(Pj_Yj) * 100, # Coeficiente de variancia
      Y        =   sum(Pj_Yj), # media estratificada (ponderada)     
      t        = stats::qt(alpha/2, df = sum(nj)-1, lower.tail = FALSE),     # a seguir, o t sera calculado utilizando o n calculado, de forma direta
      t_rec    = ifelse(pop=="inf",
          stats::qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / (error*Y/100)^2 )-1, lower.tail = FALSE),
          stats::qt(alpha/2, df = ceiling( t^2 * EPj_Sj^2 / ( (error*Y/100)^2 + (t^2 * EPj_Sj2 / N )  ) )-1, lower.tail = FALSE)
                   ),
      n_recalc = ifelse(pop=="inf",
            ceiling( t_rec^2 * EPj_Sj^2 / (error*Y/100)^2 ),
            ceiling( t_rec^2 * EPj_Sj^2 / ( (error*Y/100)^2 + (t_rec^2 * EPj_Sj2 / N )  ) ) 
                   ), # agora fazemos o recalculo do n, utilizando o t correto
      nj_optimal = ceiling(n_recalc*Pj_Sj/EPj_Sj), # por estrato utilizando o metodo de Neyman
      n_optimal  = sum(nj_optimal), # n calculado total
      Yhatj    = Nj * Yj )  %>% # producao total por estrato
    dplyr::na_if(0) %>% # substitui 0 por NA
    rm_empty_col  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    
  
  x <- x_ %>% 
    plyr::rename(
      c("AGE"        = "Age",
        "AREA_PC"    = "Plot Area",
        "nj"         = "Number of sampled plots per stratum (nj)" ,
        "n"          = "Total number of sampled plots (n)",
        "Nj"         = "Number of maximum plots per stratum (Nj)",
        "N"          = "Number of maximum plots (N)", 
        "Pj"         = "Nj/N Ratio (Pj)",
        "Eyj"        = "Stratum sum (Eyj)", 
        "Eyj2"       = "Stratum quadratic sum (Eyj2)", 
        "Yj"         = "Mean of Yi per stratum (Yj)", 
        "Pj_Sj2"     = "PjSj2", 
        "Pj_Sj"      = "PjSj", 
        "Pj_Yj"      = "PjYj",
       # "EPj_Sj2"    = "Stratified Variance",
       # "EPj_Sj"     = "Stratified Standard Deviation", 
      #  "VC"         = "Variance Quoeficient (VC)", 
     #   "Y"          = "Stratified mean (Y)",
     #   "t"          = "t-student", 
     #   "t_rec"      = "recalculated t-student", 
        "n_recalc"   = "Number of samples regarding the admited error",
        "nj_optimal" = "Optimal number of samples per stratum (nj optimal)", 
        "n_optimal"  = "Optimal number of samples (n optimal)", 
        "Yhatj"      = "Total value of Y per stratum (Yhatj)"  ),
      warn_missing = F) %>% 
    dplyr::select(-EPj_Sj2,-EPj_Sj,-VC,-Y,-t,-t_rec) %>% #remover variaveis comuns aos estratos
    forestmangr::round_df(dec_places)  
  
  
  y_ <- x_ %>%
    dplyr::summarise(
      t     = mean(t),
      t_rec = mean(t_rec),
      Sy           = ifelse(pop=="inf",
            sqrt(sum(Pj_Sj)^2 / sum(nj) ),
            sqrt(sum(Pj_Sj) ^2 / sum(nj) - (mean(EPj_Sj2) / mean(N) )  )
                     ), # Erro-padrao da media
      EPj_Sj2  =   sum(Pj_Sj2),  # Variancia estratificada
      EPj_Sj   =   sum(Pj_Sj),  # desvio padrao estratificado
      VC       = EPj_Sj / sum(Pj_Yj) * 100, # Coeficiente de variancia
      Y            = sum(Pj_Yj), # media de Yi estratificada (ponderada) 
      Abserror      = Sy * t_rec, # Erro Absoluto
      Percerror     = Abserror / Y * 100, # Erro percentual
      Yhat         = sum(Nj) * Y, # Volume Total
      Total_Error   = Abserror * sum(Nj), # Erro Total
      CI_Inf       = Y - Abserror, # Intervalo de confianca inferior
      CI_Sup       = Y + Abserror, # Intervalo de confianca superior
      CI_ha_Inf    = (Y - Abserror)*10000/mean(AREA_PC,na.rm=T), # Intervalo de confianca por ha inferior
      CI_ha_Sup    = (Y + Abserror)*10000/mean(AREA_PC,na.rm=T), # Intervalo de confianca por ha superior
      CI_Total_inf = Yhat - Total_Error, # Intervalo de confianca total inferior
      CI_Total_Sup = Yhat + Total_Error    ) %>% # Intervalo de confianca total superior)
    forestmangr::round_df(dec_places)  
  
  
  y <- y_ %>% 
    plyr::rename(
      c("t"            = "t-student",
        "t_rec"      = "recalculated t-student",
        "EPj_Sj2"    = "Stratified Variance",
        "EPj_Sj"     = "Stratified Standard Deviation", 
        "VC"         = "Variance Quoeficient (VC)", 
        "Y"            = "Stratified Mean (Y)",
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
      warn_missing = F)
  
  x_ <- forestmangr::round_df(x_, dec_places)  
  
  
  if(tidy==F){
    
    z <- list(Tabela1 = as.data.frame(x_), Tabela2 = as.data.frame(y_))
    
    return(z)
    
  }else{
    
    vec1 <- rlang::syms(names(x)[! names(x) %in% c(.groups, strata) ])
    vec2 <- rlang::sym(strata)
   # vec3 <- rlang::syms(.groups[.groups!=vec2])
    vec3 <- .groups
    vec4 <- rlang::syms(names(y)[! names(y) %in% vec3 ]) 
    vec5 <- as.character(vec3[length(vec3)])
    vec6 <- vec3[as.character(vec3)!=as.character(vec5)]
    
    x <-  x %>%
      tidyr::gather("Variables","value", !!!vec1, factor_key=T) %>% 
      dplyr::arrange(!!!.groups_syms) %>% 
      tidyr::spread(!!vec2,"value",sep=" ")
    
    if(length(.groups)!=1 ){
      
      y <- y %>%
        tidyr::gather("Variables","value", !!!vec4, factor_key=T) %>% 
        dplyr::arrange(!!!vec3) %>% 
        tidyr::spread(vec5,"value") 
      
    } else{
      
      y <- y %>%
        tidyr::gather("Variables","value", !!!vec4, factor_key=T)
      
      
    }
    
    z <- list(Table1 = as.data.frame(x), Table2 = as.data.frame(y))
    
    return(z) 
    
  }
  
  
  
}
