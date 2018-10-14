#' @title 
#' Bias of an estimator in percentage
#' @description 
#' Function for calculating the bias of an estimator. 
#' @details 
#' Function for calculating the bias of an estimator, given the observed values, and the estimated values.
#'
#' @param df a dataframe.
#' @param y Quoted name of the variable representing the observed values in the dataframe. If a dataframe is not provided, \code{y} can also be a numeric vector.
#' @param yhat Quoted name of the variable representing the estimated values in the dataframe. If a dataframe is not provided, \code{yhat} can also be a numeric vector.
#' @return 
#' Numeric vector with the bias value, in percentage.
#' 
#' @keywords bias
#' 
#' @seealso other statistics to evaluate estimators: 
#'   \code{\link{rmse_per}} for the Root mean square error of an estimator
#' @export
#' @examples 
#' library(forestmangr)
#' data(exfm11)
#' head(exfm11)
#'
#' # Bias of an estimator, given the dataframe and quoted variable names:
#' bias_per(exfm11, "TH", "TH_EST3")
#'
#' # Bias of an estimator, given the vectors for observed and estimated values:
#' bias_per(y = exfm11$TH, yhat = exfm11$TH_EST3)
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

bias_per <- function(df, y, yhat){
  # Checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,tratar como vetores
  if(  missing(df) ){  
    return(sum(y - yhat)/sum(y) * 100)
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
  # se yhat nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(yhat) ){  
    stop("yhat not set", call. = F) 
  }else if( !is.character(yhat) ){
    stop("'yhat' must be a character containing a variable name", call.=F)
  }else if(length(yhat)!=1){
    stop("Length of 'yhat' must be 1", call.=F)
  }else if(forestmangr::check_names(df, yhat)==F){
    stop(forestmangr::check_names(df, yhat, boolean=F), call.=F)
  }
# ####  
  y_sym <- rlang::sym(y)
  yhat_sym <- rlang::sym(yhat)
  
  y_vec <- df %>% dplyr::pull(!!y_sym)
  yhat_vec <- df %>% dplyr::pull(!!yhat_sym)
  
  sum(y_vec - yhat_vec)/sum(y_vec) * 100
  
}
