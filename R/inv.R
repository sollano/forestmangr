#' @title 
#' Calculate the inverse of a number
#' @description 
#' This function returns the inverse of a numeric vector.
#' @details 
#' This function is manly used when fitting statistical models.
#' If one of the variables in a model is an inverse of a vector, the \code{lm}
#' function does not properly compute the variable, if \code{1/vector} is inserted directly
#' in the model, leading to the need of creating a separate variable. This function allows the user to get the inverse
#' of a given numeric vector inside the model, without the need to create a new variable.
#'  
#' @param x A numeric vector
#' 
#' @return a numeric vector containing the inverse of \code{x}.
#' 
#' @export
#' 
#' @examples
#' library(forestmangr)
#' data("exfm15")
#' exfm15
#' 
#' # Get the inverse of a vector
#' inv(iris$Petal.Length)
#' 
#' # Fit a model that contains the inverse of a variable, without the need to
#' # create a new variable for the inverse:
#' lm(log(TH) ~ inv(DBH), exfm15 )
#' # or 
#' lm_table(exfm15, log(TH) ~ inv(DBH) )
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}


inv <- function(x){ 1/x }