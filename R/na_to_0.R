#' @title 
#' Convert NA to zero on numeric variables
#' @description 
#' This function Convert NA to zero on numeric variables outside of mutate.
#' this is used due to changes on dplyr 1.1.0.
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
#' # Turn NA values to zero only on numeric values
#' exfm15 %>% na_to_0()
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

na_to_0 <- function(df) {
  dplyr::mutate(df, dplyr::across(tidyselect::where(is.numeric), ~ tidyr::replace_na(.x, 0)))
}
