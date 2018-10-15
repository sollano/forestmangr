#' @title 
#' Divide data into diameter classes, and get number of observations
#' @description 
#' This function can be used to divide data into diameter classes,
#' get the number of observations, number of observations per ha,
#' and check number of species individuals, volume and G in each diameter class.
#' It's also possible to spread the diameter classes as columns.
#'
#' @param df A data frame.
#' @param dbh Quoted name of the diameter at breast hight variable, in cm.
#' @param plot Optional parameter.Quoted name of the plot variable. used to differentiate the plots trees, and calculate the number of sampled plots. Default \code{NA}.
#' @param plot_area Optional parameter. Quoted name of the plot area variable, or a numeric vector with the plot area value. The plot area value must be in square meters. Default \code{NA}.
#' @param ci Numeric value for the class interval used to classify the data. Default: \code{5}.
#' @param dbhmin Numeric value for minimum diameter value to be considered in the classifications. dbh values smaller than this will be disconsidered from the classification. Default: \code{5}.
#' @param species Optional parameter. Quoted name of the scientific names variable, or any variable used to differentiate the different species found in data. If supplied, will be used to classify the species in the diameter data. Default \code{NA}.
#' @param volume Optional parameter. Quoted name of the volume variable. If supplied, will be used classify the volume variable in the different diameter classes. Also, if \code{cc_to_column} is \code{TRUE}, the center of class columns will be filled with volume values, instead of number of individuals. Default \code{NA}.
#' @param NI_label Label used for Species not identified. This parameter works along with species. The level supplied here will not be considered in the classification. Default \code{"NI"}.
#' @param cc_to_column If \code{TRUE}, will spread the center class column as multiple columns, one for each class. The value that fills these columns, by default is the number of individuals found in each class, but this can be changed by using other arguments. Default \code{FALSE}.
#' @param G_to_cc If \code{TRUE}, and \code{cc_to_column} is also \code{TRUE}, will fill the center of class columns with basal area values, instead of number of individuals. Default \code{FALSE}.
#' @param cctc_ha  If \code{TRUE}, will calculate values per hectare for number of individuals per class, basal area per class and volume per class (if supplied). These values will also be used to fill the center of class columns, if cc_to_column is \code{TRUE}. Default \code{TRUE}.
#' @param keep_unused_classes Some diameter classes may end up empty, depending on the maximum value of diameter and the class interval used. If this is \code{TRUE}, those classes will not be removed from the final data frame. Default \code{FALSE}.
#' @return A data frame containing the supplied data divided into diameter classes.
#'
#' @export
#' @examples 
#' library(forestmangr)
#' data("exfm20")
#' head(exfm20)
#' 
#' # n
#' # Number of individuals per ha per diameter class
#' diameter_class(df = exfm20, dbh = "dbh", ci = 10, dbhmin = 10, volume = "vol") 
#' 
#' # Number of individuals per ha per diameter class per species
#' diameter_class(exfm20,"dbh", "transect", 10000, 10, 10, "scientific.name") 
#'
#' # Number of individuals per ha per diameter class, with each diameter class as a column
#' diameter_class(exfm20,"dbh", "transect", 10000, 10, 10, "scientific.name", cc_to_column=TRUE) 
#'
#' # G
#' # Basal area per ha per diameter class, with each diameter class as a column
#' diameter_class(exfm20,"dbh", "transect",10000,10,10,"scientific.name",
#' cc_to_column=TRUE,G_to_cc=FALSE) 
#'
#'
#' # Volume
#' # Volume per ha per diameter class
#' diameter_class(exfm20,"dbh", "transect", 10000, 10, 10, "scientific.name",volume = "vol") 
#'
#' # Volume per ha per diameter class, with each diameter class as a column
#' diameter_class(exfm20,"dbh","transect",10000,10,10,"scientific.name","vol",cc_to_column=TRUE) 
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#'
diameter_class <- function(df, dbh, plot=NA, plot_area, ci = 5, dbhmin = 5, species=NA, volume=NA, NI_label="NI", cc_to_column=FALSE, G_to_cc=FALSE, cctc_ha=TRUE, keep_unused_classes=FALSE){
  # checagem de variaveis ####

  # ci precisa ser numerico e de tamanho 1
  if(!is.numeric(ci) || length(ci)!=1){
    stop("ci must be a single number", call. = F)
  }
  
  # dbhmin precisa ser numerico e de tamanho 1
  if(!is.numeric(dbhmin) || length(dbhmin)!=1){
    stop("dbhmin must be a single number", call. = F)
  }
  
  
  # se plot nao for fornecida, for NULL, NA ou "", transformar em 1
  if(missing(plot) || is.null(plot) || is.na(plot) ||  plot==""){
    plot <- 1
  }
  
  # se plot_area nao for fornecida, for NULL, NA ou "", transformar em 10000
  if(missing(plot_area) || is.null(plot_area) || is.na(plot_area) ||  plot_area==""){
    plot_area <- 10000
  }
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se dbh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dbh) || dbh == ""){  
    stop("dbh not set", call. = F) 
  }else if( !is.character(dbh) ){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(check_names(df, dbh)==F){
    stop(check_names(df, dbh, boolean=F), call.=F)
  }
  
  
  
  # se vol nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(volume) || is.null(volume) || is.na(volume) || volume==""){  
    df$vol <- 23
    volume_sym <- rlang::sym("vol")
    
  }else if( !is.character(volume) ){
    stop("'volume' must be a character containing a variable name", call.=F)
  }else if(length(volume)!=1){
    stop("Length of 'volume' must be 1", call.=F)
  }else if(check_names(df, volume)==F){
    stop(check_names(df, volume, boolean=F), call.=F)
  }else{
    volume_sym <- rlang::sym(volume)
    
  }
  
  # Se NI_label nao for character,ou nao for de tamanho 1, parar
  if(!is.character( NI_label )){
    stop( "'NI_label' must be character", call.=F)
  }else if(length(NI_label)!=1){
    stop("Length of 'NI_label' must be 1", call.=F)
  }
  
  # Se species nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(species)||is.null(species)||is.na(species)||species==F||species==""){
    species_sym <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(species)){
    stop("species must be a character", call. = F)
  }else if(! length(species)%in% 1:10){ 
    stop("Length of 'species' must be between 1 and 10", call.=F)
  }else if(check_names(df,species)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(check_names(df,species, boolean=F), call.=F) 
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    
    #app
    if(is.null(NI_label)||NI_label==""){NI_label <- "NI"}
    
    df <- df[! df[[species]] %in% NI_label, ] 
    species_sym <- rlang::sym(species) 
  }
  
  
  # se plot nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot) ){  
    plot <- 1
    nplot <- plot
  }else if( is.null(plot) || is.na(plot) || plot == "" ){
    stop("'plot' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(plot) & length(plot)==1){
    
    nplot <- plot
    
  }else if(!is.character(plot)){
    stop("'plot' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot)!=1){
    stop("Length of 'plot' must be 1", call.=F)
  }else if(check_names(df, plot)==F){
    stop(check_names(df, plot, boolean = F), call.=F)
  }else{
    plot_sym <- rlang::sym(plot)
    nplot <- df %>% 
      dplyr::pull(!!plot_sym) %>%
      as.factor %>% 
      nlevels
  }
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot_area) ){  
    plot_area <- 10000
    plot_area_num <- plot_area
  }else if( is.null(plot_area) || is.na(plot_area) || plot_area == "" ){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(plot_area) & length(plot_area)==1){
    
    plot_area_num <- plot_area
    
  }else if(!is.character(plot_area)){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot_area)!=1){
    stop("Length of 'plot_area' must be 1", call.=F)
  }else if(check_names(df, plot_area)==F){
    stop(check_names(df, plot_area, boolean = F), call.=F)
  }else{
    plot_area_sym <- rlang::sym(plot_area)
    
    plot_area_num <- df %>% 
      dplyr::group_by( !!!plot_sym ) %>% 
      dplyr::summarise(AREA = mean(!!plot_area_sym), na.rm=TRUE) %>% 
      dplyr::summarise(A = mean(AREA, na.rm=TRUE)) %>% 
      dplyr::pull(A) 
    
  }
  
  # Parar se o usuario pedir o resultado por ha sem fornecer as variaveis necessarias
  if(cctc_ha==TRUE & cc_to_column==TRUE &  plot == 1 & plot_area == 10000 ){stop("plot and plot_area must be provided if cctc_ha=TRUE",call. = F)}
  
  # Permitir se e nse como entrada de nomes de variaveis
  dbh_sym <- rlang::sym(dbh)
  
  # ####
  
  df_final <- df %>% 
    dplyr::filter(!is.na( !!dbh_sym ) ) %>% # remover NA
    dplyr::mutate(
      CC = ceiling(( !!dbh_sym )/ci) * ci - ci/2, # Calcular Centro de classe
      g = pi * (!!dbh_sym)^2 / 40000   ) %>%  # Calcular area seccional
    dplyr::group_by(!!!species_sym, CC ) %>% # Agrupar e calcular o numero de individuos, e n de individuos por ha
    dplyr::summarise(
      NumIndv=n(),
      IndvHA = round( NumIndv / (plot_area_num/10000 * nplot ),  1 ),
      G = sum(g),
      G_ha = sum(g) / (plot_area_num/10000 * nplot ),
      volume = sum( !!volume_sym, na.rm = T  ),
      volume_ha = sum( !!volume_sym, na.rm = T) / (plot_area_num/10000 * nplot )     ) %>% 
    dplyr::mutate(RD =  round(NumIndv/sum(NumIndv) * 100, 4) ) %>% # Calcular densidade relativa
    dplyr::arrange( CC ) %>% 
    dplyr::filter(CC >= dbhmin) %>% # Remover classes menores que o dbh minimo
    dplyr::ungroup() %>% 
    as.data.frame()
  
  if(keep_unused_classes){
    df_final <- df_final  %>% 
      dplyr::full_join( data.frame(
        CC=seq( min(.$CC),max(.$CC), by=ci)), 
        by=c( "CC" )
      ) %>% 
      dplyr::arrange(CC)
  }
  
  # Remover volume caso nao tenha fornecido pelo usuario
  if(is.na(volume) || is.null(volume) || volume==""){
    
    df_final$volume <- NULL
    df_final$volume_ha <- NULL
  }
  
  # se plot ou plot_area nao for fornecida, for NULL, NA ou "", remover variaveis extrapoladas ha
  #if(missing(plot) || is.null(plot) || is.na(plot) ||  plot=="" || missing(plot_area) || is.null(plot_area) || is.na(plot_area) ||  plot_area=="" ){
  if(plot == 1 & plot_area == 10000){
    df_final$IndvHA <- NULL
    df_final$G_ha <- NULL
    df_final$volume_ha <- NULL
  }
  
  # Se o usuario quiser o centro de classe na coluna e nao tiver fornecido volume,
  # popular o centro de classe com o numero de individuos
  if(cc_to_column==T &&  G_to_cc==T ){
    
    if(cctc_ha==T){df_final$G_f <- df_final$G_ha}else if(cctc_ha==F){df_final$G_f <- df_final$G}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      dplyr::select(!!species_sym,CC,G_f) %>% 
      tidyr::spread(CC,G_f, fill = 0) %>% 
      dplyr::mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && (!missing(species)||!is.null(species)||!is.na(species)||species!=F||species!="") && (missing(volume) || is.null(volume) || is.na(volume) || volume=="") ){
    
    if(cctc_ha==T){df_final$NI <- df_final$IndvHA}else if(cctc_ha==F){df_final$NI <- df_final$NumIndv}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      dplyr::select(!!species_sym,CC,NI) %>% 
      tidyr::spread(CC,NI, fill = 0 ) %>% 
      dplyr::mutate(Total = rowSums(.[  ,  sapply(., is.numeric)  ]  ) ) %>% 
      as.data.frame %>% 
      round_df(4)
    
    df_final[df_final==0] <- ""
    # Se o usuario quiser o centro de classe na coluna e tiver fornecido volume,
    # popular o centro de classe com o volume
  }else if(cc_to_column==T && (!missing(species)||!is.null(species)||!is.na(species)||species!=F||species!="") && (!missing(volume) || !is.null(volume) || !is.na(volume) || volume!="" ) ){
    
    if(cctc_ha==T){df_final$VOL<- df_final$volume_ha}else if(cctc_ha==F){df_final$VOL<- df_final$volume}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    df_final <- df_final %>% 
      dplyr::select(!!species_sym,CC,VOL) %>% 
      tidyr::spread(CC,VOL, fill = 0) %>% 
      dplyr::mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && (missing(species)||is.null(species)||is.na(species)||species==F||species=="") ){
    
    stop("species column must be provided if cc_to_column is true ", call. = F)
    
  }
  
  return(df_final)
  
}
