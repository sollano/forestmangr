#' @title
#' Classify a forest for selective cutting using the Meyer BDq method
#' @description 
#' This function can be used to plan and execute selective cuttings of a native
#' forest, without damaging the forest's natural structure.
#' 
#' @param df A data frame.
#' @param plot Quoted name of the plot variable. used to differentiate the plot's trees, and calculate the number of sampled plots.
#' @param dbh Quoted name of the diameter at breast height variable, in cm.
#' @param plot_area Quoted name of the plot area variable, or a numeric vector with the plot area value. The plot area value must be in square meters.
#' @param class_interval Numeric value for the class interval used to classify the data. Default: \code{5}.
#' @param dbh_min Numeric value for minimum diameter value to be considered in the classifications. dbh values smaller than this will be dismissed from the classification. Default: \code{5}.
#' @param licourt_index Numeric value for the start licourt index used. Default: \code{2}.
#' @param output Character value for the desired output. Can be either \code{"table"} for the classified data table, \code{"model"} to get a lm object with the linear model fitted, \code{"coefs"} to get a vector with the Meyer coefficients, or \code{"full"}, to get a list with all restuls. Default: \code{"table"}.
#' @return a data frame, a lm object, a vector or a list, according to the output argument.
#' 
#' @references 
#' Souza, A. L. and Soares, C. P. B. (2013) Florestas Nativas: estrutura, dinamica e manejo. Vicosa: UFV.
#' 
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data("exfm20")
#' exfm20
#' 
#' # To get the table with the regulated forest:
#' bdq_meyer(exfm20, "transect", "dbh", 1000)
#' 
#' # Use different class interval values to get different results:
#' bdq_meyer(exfm20, "transect", "dbh", 1000, class_interval = 10)
#' 
#' # It's possible to get different outputs:
#' bdq_meyer(exfm20, "transect", "dbh", 1000, output="model")
#' bdq_meyer(exfm20, "transect", "dbh", 1000, output="coefs")
#' bdq_meyer(exfm20, "transect", "dbh", 1000, output="full")
#'
#' 
#' @author Eric Bastos Gorgens \email{e.gorgens@@gmail.com}
#'
bdq_meyer <- function(df, plot, dbh, plot_area, class_interval = 5, dbh_min = 5, licourt_index = 2, output="table"){
  # #####
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se plot nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(plot) ){  
    stop("plot not set", call. = F) 
  }else if( !is.character(plot) ){
    stop("'plot' must be a character containing a variable name", call.=F)
  }else if(length(plot)!=1){
    stop("Length of 'plot' must be 1", call.=F)
  }else if(forestmangr::check_names(df, plot)==F){
    stop(forestmangr::check_names(df, plot, boolean=F), call.=F)
  }
  
  # se dbh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dbh) ){  
    stop("dbh not set", call. = F) 
  }else if( !is.character(dbh) ){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dbh)==F){
    stop(forestmangr::check_names(df, dbh, boolean=F), call.=F)
  }
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot_area) ){  
    stop("plot_area not set", call. = F) 
  }else if( is.null(plot_area) || is.na(plot_area) || plot_area == "" ){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(plot_area) & length(plot_area)==1){
    
    AREA.PLOT = plot_area
    
  }else if(!is.character(plot_area)){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot_area)!=1){
    stop("Length of 'plot_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, plot_area)==F){
    stop(forestmangr::check_names(df, plot_area, boolean = F), call.=F)
  }else if(is.character(plot_area)){
    
    AREA.PLOT = mean(df[,plot_area],na.rm = T )
    
  }
  
  # Se class_interval nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( class_interval )){
    stop( "'class_interval' must be numeric", call.=F)
  }else if(length(class_interval)!=1){
    stop("length of 'class_interval' must be 1", call.=F)
  }
  
  # Se dbh_min nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( dbh_min )){
    stop( "'dbh_min' must be numeric", call.=F)
  }else if(length(dbh_min)!=1){
    stop("length of 'dbh_min' must be 1", call.=F)
  }
  
  # Se licourt_index nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( licourt_index )){
    stop( "'licourt_index' must be numeric", call.=F)
  }else if(length(licourt_index)!=1){
    stop("length of 'licourt_index' must be 1", call.=F)
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('table', 'model', 'coefs', 'full') ){ 
  stop("'output' must be equal to 'table', 'model', 'coefs' or 'full' ", call. = F) 
  }
  # ####
  df <- as.data.frame(df)
  INTERVALO.CLASSE = class_interval
  DBH = dbh
  DBH.MIN = dbh_min
  PLOTS = plot
  LICOURT = licourt_index
  
  # Remover NA
  df = df[!is.na(df[DBH]),]
  
  # Calcula número de parcelas na area de estudo
  nplots = length(unique(df[,PLOTS]))
  
  # Estrutura diametrica
  
  df[,"Class"] = trunc(df[,DBH] /  INTERVALO.CLASSE)
  df[, "Class_Center"] = df[,"Class"] * INTERVALO.CLASSE + (INTERVALO.CLASSE / 2)
  
  freq = data.frame(table(df[,"Class"]))
  DD = data.frame(Class = as.numeric(as.character(freq[,1])) ) # correcao fator para numerico
  DD$Class_Center = DD$Class * INTERVALO.CLASSE - (INTERVALO.CLASSE / 2)
  DD$NumIndv = freq[,2]
  # Alterei aqui para a area poder ser inserida em m2
  DD$IndvHectare = round(DD$NumIndv / ((AREA.PLOT/10000) * nplots), 1)
  DD = DD[DD$Class_Center >= DBH.MIN,]
  DD = DD[DD$IndvHectare > 0,]
  rm(freq)
  
  # Meyer
  meyer = stats::lm(log(DD$IndvHectare) ~ DD$Class_Center)
  DD$Meyer = round(exp(stats::predict(meyer)), 0)
  
  # # Mervart
  # mervart = stats::lm(log(DD$IndvHectare) ~ log(DD$Class_Center))
  # DD$Mervart = round(exp(stats::predict(mervart)), 0)
  
  # Licourt atual
  q = 0
  for (i in seq(1, length(DD$Class_Center)-1,1)){
    q[i] = DD$IndvHectare[i] / DD$IndvHectare[i+1]
  }
  q[length(DD$Class_Center)] = NA
  DD$q = round(q, 1)
  rm(q)
  
  # DBq base meyer
  
  # Calcula b1 do modelo de Meyer
  b1 = round(log(LICOURT)/(- INTERVALO.CLASSE), 6)
  
  # Calcula b0 do modelo de Meyer
  temp.b0 = DD$Class_Center^2 * exp(b1 * DD$Class_Center)
  sum.temp.b0 = sum(temp.b0)
  areaBasal = (DD$Class_Center^2 * pi / 40000) * (DD$IndvHectare)
  b0 = log(40000 * sum(areaBasal) / (pi * sum.temp.b0))
  rm(temp.b0, sum.temp.b0, areaBasal)
  
  # Calcula a distribuicao diametrica balanceada com base no modelo de Meyer
  DD$MeyerBalan = round(exp(b0 + b1 * DD$Class_Center), 0)
  
  result = list(DD, meyer, c(b0, b1))
  
  if(output=="table"){
    return(DD)
  }else if(output=="model"){
    return(meyer)
  }else if(output=="coefs"){
    return(c(b0, b1))
  }else if(output=="full"){
    list(table=DD, model=meyer,coefs=c(b0, b1))
  }
}
