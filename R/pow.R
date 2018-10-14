#' @title 
#' Raise a numeric vector to a given power
#' @description 
#' This function returns a numeric vector raised to a given power.
#' @details 
#' This function is manly used when fitting statistical models.
#' If one of the variables in a model is a variable raised to a given power, the \code{lm}
#' function does not properly compute the variable, if \code{vector^power} is inserted directly
#' in the model, leading to the need of creating a separate variable. This function allows the user to get the power
#' of a given numeric vector to \code{y} inside the model, without the need to create a new variable.
#'  
#' @param x A numeric vector
#' @param y A numeric value for the power \code{x} should be raised to.
#' 
#' @return a numeric vector containing \code{x} to the power of \code{y}.
#' 
#' @export
#' 
#' @examples
#' library(forestmangr)
#' data("exfm15")
#' head(exfm15)
#' 
#' # Raise a numeric vector to the power of 2:
#' pow(iris$Petal.Length, 2)
#' 
#' # Fit a model that contains the dbh squared, without the need to create a new variable:
#' lm(log(TH) ~ DBH + pow(DBH,2), exfm15 )
#' # or 
#' lm_table(exfm15, log(TH) ~ DBH + pow(DBH,2) )
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

pow <- function(x,y){ x^y }