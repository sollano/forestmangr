#' @title 
#' Summarize forest inventory data
#' @description 
#' Get informations about forest inventory plots, like number of individuals,
#' mean DBH, q, height, basal area, volume, etc.
#' 
#' @param df A data frame.
#' @param plot Quoted name of the plot variable. used to differentiate the data's plots. If this argument is missing, the defined groups in the data frame will be used, If there are no groups in the data, the function will fail.
#' @param plot_area Quoted name of the plot area variable, or a numeric vector with the plot area value. The plot area value must be in square meters. 
#' @param dbh Optional parameter. Quoted name of the diameter at breast height variable. If supplied, will be used to calculate the mean diameter per plot, quadratic diameter (q), basal area and basal area per hectare. Default \code{NA}.
#' @param th Optional parameter. Quoted name of the total height variable. If supplied, will be used to calculate the mean total height, and the dominant height variable, if the \code{dh} is \code{NA}. Default \code{NA}.
#' @param .groups Optional argument. Quoted name(s) of grouping variables that can be added to differentiate subdivisions of the data. Default: \code{NA}.
#' @param total_area Optional argument. Quoted name of the total area variable, or a numeric vector with the total area value. The total area value must be in hectares. Default: \code{NA}.
#' @param vwb Optional parameter. Quoted name of the volume with bark variable. If supplied, will be used to calculate the total vwb per plot, and vwb per hectare per plot. Default \code{NA}.
#' @param vwob Optional parameter. Quoted name of the volume without bark variable. If supplied, will be used to calculate the total vwob per plot, and vwob per hectare per plot. Default \code{NA}.
#' @param dh Optional parameter. Quoted name of the dominant height variable. If supplied, will be used to calculate the mean dominant height per plot. If not, the \code{ht} variable supplied will be used to calculate the average of the top two trees of each plot, and use that as dh. Default: \code{NA}.
#' @param age Optional parameter. Quoted name of the age variable. If supplied, will be used to calculate the average age per plot. Default: \code{NA}.
#' @param dec_places Numeric value for the number of decimal places to be used in the output tables. Default: \code{4}.
#' @return A data frame with informations per plot.
#'
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data("exfm21")
#' head(exfm21)
#' 
#' # Obligatory arguments. Basic informations about the plot.
#' plot_summarise(exfm21, "PLOT", 810)
#' 
#' # Area values can be numeric, or a variable name
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA")
#' 
#' # With DBH supplied, we get the mean diameter, quadratic diameter,
#' # basal area and basal area per hectare:
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH")
#' 
#' # With TH supplied, we get the mean total height and dominant height
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH", "TH_EST")
#' 
#' # With strata supplied, we divide the data into 2 strata
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA")
#' 
#' # The strata area can also be supplied
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA")
#' 
#' # With VWB supplied, we get the total vwb, and vwb per hectare
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA",
#'  "VWB")
#'
#' # With VWOB supplied, we get the total vwob, and vwob per hectare
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA",
#'  "VWB", "VWOB")
#' 
#' # If the data already has a dominant height variable, it can also be supplied here
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA",
#'  "VWB", "VWOB", "DH")
#' 
#' # With the AGE variable supplied, we get the average age of each plot
#' plot_summarise(exfm21, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA",
#'  "VWB", "VWOB", "DH", "AGE")
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' 
plot_summarise <- function(df, plot, plot_area, dbh, th, .groups, total_area, vwb, vwob, dh, age, dec_places = 4) {
  # ####
  CSA<-Indv<-G<-NULL
  # checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se dbh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dbh) || is.null(dbh) || is.na(dbh) || dbh == ""  ){  
    df $ dbh <- NA
    dbh <- "dbh"
  }else if( !is.character(dbh) ){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dbh)==F){
    stop(forestmangr::check_names(df, dbh, boolean=F), call.=F)
  }
  
  # se th nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(th) || is.null(th) || is.na(th) || th == "" ){
    df $ th <- NA
    th <- "th"
  }else if(!is.character(th)){
    stop("'th' must be a character containing a variable name", call.=F)
  }else if(length(th)!=1){
    stop("Length of 'th' must be 1", call.=F)
  }else if(forestmangr::check_names(df, th)==F){
    stop(forestmangr::check_names(df, th, boolean=F), call.=F)
  }
  
  # se vwb nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(vwb) || is.null(vwb) || is.na(vwb) || vwb == "" ){
    df $ vwb <- NA
    vwb <- "vwb"
  }else if(!is.character(vwb)){
    stop("'vwb' must be a character containing a variable name", call.=F)
  }else if(length(vwb)!=1){
    stop("Length of 'vwb' must be 1", call.=F)
  }else if(forestmangr::check_names(df, vwb)==F){
    stop(forestmangr::check_names(df, vwb, boolean=F), call.=F)
  }
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot_area) || is.null(plot_area) || is.na(plot_area) || plot_area == "" ){  
    stop("plot_area not set", call. = F) 
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
  
  # se total_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, criar variavel vazia
  # Se for fornecida verificar se e numerica ou nome de variavel
  if(  missing(total_area) || is.null(total_area) || is.na(total_area) || total_area == "" ){ 
    df $ total_area <- NA
    total_area <- "total_area" 
  }else if(is.numeric(total_area) & length(total_area)==1){
    df $ total_area <- total_area
    total_area <- "total_area"
  }else if(!is.character(total_area)){
    stop("'total_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(total_area)!=1){
    stop("Length of 'total_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, total_area)==F){
    stop(forestmangr::check_names(df, total_area, boolean = F), call.=F)
  }
  
  # se age nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(age) || is.null(age) || is.na(age) || age == "" ){
    df $ age <- NA
    age <- "age"
  }else if(!is.character(age)){
    stop("'age' must be a character containing a variable name", call.=F)
  }else if(length(age)!=1){
    stop("Length of 'age' must be 1", call.=F)
  }else if(forestmangr::check_names(df, age)==F){
    stop(forestmangr::check_names(df, age, boolean=F), call.=F)
  }
  
  # se vwob nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(vwob) || is.null(vwob) || is.na(vwob) || vwob == "" ){
    df $ vwob <- NA
    vwob <- "vwob"
  }else if(!is.character(vwob)){
    stop("'vwob' must be a character containing a variable name", call.=F)
  }else if(length(vwob)!=1){
    stop("Length of 'vwob' must be 1", call.=F)
  }else if(forestmangr::check_names(df, vwob)==F){
    stop(forestmangr::check_names(df, vwob, boolean=F), call.=F)
  }
  
  # Se plot nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(plot) && is.null(dplyr::groups(df)) ){
    stop("plot not set. plot must be set if data doesn't have any groups", call. = F)
  }else if(missing(plot) && !is.null(dplyr::groups(df)) ){
    plot_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(plot)){
    stop("plot must be a character", call. = F)
  }else if(! length(plot)%in% 1:10){
    stop("Length of 'plot' must be between 1 and 10", call.=F) 
  }else if(forestmangr::check_names(df,plot)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,plot, boolean=F), call.=F) 
  }else{
    plot_syms <- rlang::syms(plot) 
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
  
  # Se dec_places nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( dec_places )){
    stop( "'dec_places' must be numeric", call.=F)
  }else if(length(dec_places)!=1){
    stop("Length of 'dec_places' must be 1", call.=F)
  }else if(! dec_places %in%  seq(from=0,to=9,by=1) ){
    stop("'dec_places' must be a number between 0 and 9", call.=F)
  }
  
  dbh_name <- dbh
  th_name <- th
  plot_area_name <- plot_area
  total_area_name <- total_area
  vwb_name <- vwb
  vwb_ha_name <- paste(vwb,"ha",sep="_")
  vwob_name <- vwob
  vwob_ha_name <- paste(vwob,"ha",sep="_")
  age_name <- age
  
  
  dbh_sym <- rlang::sym( dbh )
  th_sym <- rlang::sym( th )
  vwb_sym <- rlang::sym( vwb )
  plot_area_sym <- rlang::sym( plot_area )
  total_area_sym <- rlang::sym( total_area )
  age_sym <- rlang::sym( age )
  vwob_sym <- rlang::sym( vwob )
  
  # ####
  
  if(missing(dh) || dh=="" || is.null(dh) || is.na(dh) ){ # se a altura dominante nao for fornecida
    # se ja existir uma variavel chamada "DH", deletar
    if(  "DH" %in% names(df) ){ df$DH <- NULL }
    # estimar altura dominante
    df <- forestmangr::dom_height(df = df, th = th,plot = plot, .groups = .groups, merge_data = T)
    
    dh_name <- "DH"
    dh_sym <- rlang::sym( "DH" )
    
    # caso contrario, so criar os symbolos
  } else{ 
    
    dh_name <- dh
    dh_sym <- rlang::sym( dh )
    
    }
  # novo nome = nome antigo
  
  df %>% 
    dplyr::group_by(!!!.groups_syms, !!!plot_syms, .add=T) %>% 
    dplyr::mutate(CSA = pi * (!!dbh_sym)^2 / 40000 ) %>% 
    dplyr::summarise(
      !!age_name        := round( mean(as.numeric( (!!age_sym) ), na.rm=T) ),
      !!total_area_name := mean( !!total_area_sym, na.rm=T), 
      !!plot_area_name  := mean( !!plot_area_sym, na.rm=T),
      !!dbh_name        := mean(!!dbh_sym, na.rm=T),
      q                 = sqrt(mean(CSA, na.rm=T) * 40000 / pi),
      !!th_name         := mean(!!th_sym, na.rm=T),
      !!dh_name         := mean(!!dh_sym),
      Indv              = dplyr::n(),
      Indvha            = Indv* 10000/(!!plot_area_sym),
      G                 = sum(CSA, na.rm=T),
      G_ha              = G * 10000/(!!plot_area_sym),
      !!vwb_name        := sum(!!vwb_sym, na.rm=T),
      !!vwb_ha_name     := (!!rlang::sym(vwb_name)) * 10000/ (!!plot_area_sym),
      !!vwob_name       := sum(!!vwob_sym, na.rm=T),
      !!vwob_ha_name    := (!!rlang::sym(vwob_name)) * 10000/ (!!plot_area_sym)  ) %>% #sumarise 
    na_to_0() %>% # substitui 0 por NA
    rm_empty_col %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    forestmangr::round_df(dec_places)
}
