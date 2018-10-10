#' @title 
#' Round all numeric variables of a dataframe to a given digit
#' @description 
#' This function allows the user to round all numeric values of a dataframe,
#' directly, even if the dataframe contains non-numeric variables
#' (which would throw an error in the \link[base]{round} function).
#' @param df A dataframe.
#' @param digits Numeric vector for the desired number of digits.
#' @param rf Type of round to be used. It can either be \code{"ceiling"}, \code{"floor"}, \code{"trunc"}, \code{"signif"}, or \code{"round"}. Default \code{"round"}.
#' 
#' @return A dataframe, with all the numeric variables rounded up to the number given to \code{digits}.
#' 
#' @export 
#' 
#' @examples 
#' library(forestmangr)
#' 
#' # Round all numeric variables
#' round_df(iris) 
#' 
#' # Round all numeric variables using the floor function
#' round_df(iris, rf="floor") 
#' 
#' # Do not run
#' # trying this with the base::round function throws an error:
#' 
#' # round(iris)
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#'
round_df <- function(df, digits, rf="round") {
  
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }
  
  if(missing(digits)){
    digits <- 0
  }else if(!is.numeric( digits )){
    stop( "'digits' must be numeric", call.=F)
  }else if(length(digits)!=1){
    stop("length of 'digits' must be 1", call.=F)
  }else if(! digits > -10 | ! digits <= 10){
    stop("'digits' must be a number between -10 and 10", call.=F)
  }
  
  if(!is.character( rf )){
    stop( "'rf' must be character", call.=F)
  }else if(length(rf)!=1){
    stop("Length of 'rf' must be 1", call.=F)
  }else if(! rf %in% c('round', 'ceiling', 'floor', 'trunc', 'signif') ){ 
  stop("'rf' must be equal to 'round', 'ceiling', 'trunc', 'floor' or 'signif' ", call. = F) 
  }
  
  numeric_columns <- sapply(df, class) == 'numeric'
  
  if(rf=="round"){
  
  df[numeric_columns] <-  round(df[numeric_columns], digits)
  
  }else if(rf=="ceiling"){
    
    df[numeric_columns] <-  ceiling(df[numeric_columns])
    
  }else if(rf=="floor"){
    
    df[numeric_columns] <-  floor(df[numeric_columns])
    
  }else if(rf=="trunc"){
    
    df[numeric_columns] <-  ceiling(df[numeric_columns])
    
  }else if(rf=="signif"){
    
    df[numeric_columns] <-  signif(df[numeric_columns], digits)
    
  }
  
  df
}
