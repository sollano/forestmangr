#' @export

outliersiqr <- function(x){
  
  x <- dplyr::na_if(x,0)
  
  q25      <- stats::quantile(x, probs=c(.25), na.rm = TRUE)
  q75      <- stats::quantile(x, probs=c(.75), na.rm = TRUE)
  iqr      <- stats::IQR(x, na.rm = TRUE)
  uplimit  <- q75+1*iqr
  lowlimit <- q25-1*iqr
  
  y <- dplyr::tibble(lowlimit,uplimit)  
  
  return(y)
}
