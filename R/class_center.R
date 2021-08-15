#' @title 
#' Classify a given variable and get center of class
#' @description 
#' This function can be used to divide the data into classes, based on
#' minimum value and class interval of a given variable,
#' and create a column with the center of each class.
#'
#' @param df A data frame.
#' @param y Quoted name of a variable, or a vector to be classified.
#' @param ci Numeric value for the class interval used to classify the data. Default: \code{3}.
#' @param ymin Numeric value for minimum value value to be considered in the classifications. dbh values smaller than this will be dismissed from the classification. Default: \code{5}.
#' @return if \code{df} is supplied, a data frame containing the supplied data with a new column for the center of classes; if \code{df} is missing, a vetor with the center of class.
#'
#' @export
#' @examples 
#' library(forestmangr)
#' library(dplyr)
#' data("exfm20")
#' head(exfm20)
#' 
#' # n
#' # Number of individuals per ha per diameter class
#' class_center(df = exfm20, y = "dbh", ci = 10, ymin = 10) 
#' 
#' exfm20 %>% 
#' mutate(CC = class_center(y = dbh, ci = 10, ymin = 10))
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#'
class_center <- function(df, y, ci = 3, ymin = 5){
  
  # ####
  
  if(missing(df) & !missing(y)){
    
    if(!is.numeric(y)) stop("'y' must be numeric if df is missing",call. = FALSE)
    
    return( ymin + (ci/2) * (1 + trunc((y-ymin)/ci) * 2)   )
    
  }else if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se y nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(y) ){  
    stop("y not set", call. = F) 
  }else if( !is.character(y) ){
    stop("'y' must be a character containing a variable name", call.=F)
  }else if(length(y)!=1){
    stop("Length of 'y' must be 1", call.=F)
  }else if(forestmangr::check_names(df, y)==F){
    stop(forestmangr::check_names(df, y, boolean=F), call.=F)
  }
  
  # Se ic nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( ci )){
    stop( "'ci' must be numerci", call.=F)
  }else if(length(ci)!=1){
    stop("length of 'ci' must be 1", call.=F)
  }else if(! ci > 0 | ! ci <= max(df[[y]])){
    stop("'ci' must be a number between 0 and max y", call.=F)
  }
  
  # Se ymin nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( ymin )){
    stop( "'ymin' must be numeric", call.=F)
  }else if(length(ymin)!=1){
    stop("length of 'ymin' must be 1", call.=F)
  }else if(! ymin > 0 | ! ymin <= max(df[[y]])){
    stop("'ymin' must be a number between 0 and max y", call.=F)
  }
  # ####
  
  y_sym <- rlang::sym(y)
  
  df %>% 
    dplyr::mutate(CC = ymin + (ci/2) * (1 + trunc( ((!!y_sym) - ymin) /ci) * 2) )
  
}