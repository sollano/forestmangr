#' @title 
#' Remove empty columns
#' @description 
#' This function removes columns filled with \code{NA} or \code{0} from a dataframe.
#' @details 
#' This function is mainly used inside other functions, to remove optional variables
#' when they are not opted in.
#' 
#' @param x A dataframe
#' 
#' @return a dataframe.
#' 
#' @export
#' 
#' @examples
#' library(forestmangr)
#' library(dplyr)
#' data("exfm15")
#' head(exfm15)
#' 
#' exfm15 %>% 
#' mutate(emptycol=NA) %>% 
#' rm_empty_col
#' 
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
rm_empty_col <- function(x){
  dplyr::select_if(x, Negate(function(y){all(y==0|is.na(y))}))
  }
