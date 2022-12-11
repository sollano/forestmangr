#' @title 
#' Convert NA to zero on numeric variables
#' @description 
#' This function Convert NA to zero on numeric variables outside of mutate.
#' this is used because of dplyr 1.1.0.
#'  
#' @param df A data frame
#' 
#' @return a data frame
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
#' exfm15 %>% na_to_zero()
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

na_to_0 <- function(df) {
  dplyr::mutate(df, dplyr::across(dplyr::where(is.numeric), ~ dplyr::na_if(.x, 0)))
}