#' @title 
#' Calculate the equivalent diameter
#' @description 
#' Calculate the equivalent diameter of a character variable with multiple diameter variables
#' stored, separated by a symbol.
#' 
#' @param df A dataframe.
#' @param dbh Quoted name of the diameter at breast height variable. Used to filter out trees with no diameter measerument.
#' @param sep Separator used in the dbh variable to separate the values. Can be either: \code{";"}, \code{","} \code{"-"} , \code{" "} or \code{"+"}. Default: \code{"+"}.
#' @param dbh_name Character object for the new variable name. Default: \code{FALSE}.
#' @param dec_comma If \code{TRUE}, the decimals will be considred as a comma, if \code{FALSE}, decimals will be condidered as dot. Default: \code{}FALSE.
#' @param remove_old If \code{TRUE}, the old dbh variable will me removed from the dataframe. Default: \code{TRUE}.
#' @return A dataframe with an added equivalent diameter variable.
#' 
#' @export
#' 
#' @examples
#' 
#' library(forestmangr)
#' data("exfm22")
#' data("exfm23")
#' 
#' exfm22 
#' # This data has a column with dbh values separated by a "+" sign, and
#' # a dot as decimal, so we can just calculate the equivalent diameter directly:
#' eq_dbh(df = exfm22, dbh = "dbh", sep = "+", dbh_name = "eq_dbh")
#' 
#' exfm23
#' # In this data, decimals are separated by a comma. So we use the argument dec_comma = TRUE:
#' eq_dbh(df = exfm23, dbh = "dbh", sep = "+", dbh_name = "eq_dbh", dec_comma = TRUE)
#' 
#' # It's possible to keep the old variable, using remove_old=FALSE:
#' eq_dbh(df = exfm23, dbh = "dbh", sep = "+", dbh_name = "eq_dbh", dec_comma = TRUE, remove_old=FALSE)
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' 
eq_dbh <- function(df, dbh, sep="+", dbh_name = "dbh", dec_comma=FALSE,remove_old=TRUE){
  # Checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
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
  
  # Se sep nao for character,ou nao for de tamanho 1, parar
  if(!is.character( sep )){
    stop( "'sep' must be character", call.=F)
  }else if(length(sep)!=1){
    stop("Length of 'sep' must be 1", call.=F)
  }else if(! sep %in% c(';', ',', '-','+',' ') ){ 
  stop("'sep' must be equal to ';', ',', '-', '+',' ' ", call. = F) 
  }
  
  # Se dbh_name nao for character,ou nao for de tamanho 1, parar
  if(!is.character( dbh_name )){
    stop( "'dbh_name' must be character", call.=F)
  }else if(length(dbh_name)!=1){
    stop("Length of 'dbh_name' must be 1", call.=F)
  }
  
  # se dec_comma nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! dec_comma %in% c(TRUE, FALSE) ){ 
    stop("'dec_comma' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(dec_comma)!=1){
    stop("Length of 'dec_comma' must be 1", call.=F)
  }
  
  # se remove_old nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! remove_old %in% c(TRUE, FALSE) ){ 
    stop("'remove_old' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(remove_old)!=1){
    stop("Length of 'remove_old' must be 1", call.=F)
  }
  
  dbh_sym <- rlang::sym(dbh)
  
  # ####
  
  # Saves the variable as a separate vector
  dbh_old <- df %>% dplyr::pull(!!dbh_sym)
  
  # Splits the strings by the separator ("+")
  list_split <- strsplit(dbh_old,  sep, fixed=T)
  
  # If comma is used as dec, replaced it with "."
  if(dec_comma){list_split <- sapply(list_split, gsub, pattern=",",replacement= ".")}
  
  # Convert vectors from list to numeric
  list_split <- sapply(list_split, as.numeric)
  
  # Get the  index of the vector with most observations.
  # This will be the number of columns created in our dataframe,
  # one for each observation of diameter
  n.obs <- sapply(list_split, length) 
  seq.max <- seq_len(max(n.obs))
  
  
  #"[" will take the seq.max value of element of df_split, like
  # list_split[[1]][5]. If list_split[[1]] does not have 5 elements,
  # the others will be filled with NA. This assures that all rows have 5 elements,
  # and makes it possible to convert it to a proper dataframe
  df_split <- as.data.frame( t(sapply(list_split, "[", i = seq.max))  )
  df_split
  
  # Delete the old variable, if the user desires
  if(remove_old){df[dbh] <- NULL}
  
  # Calculate the equivalent diameter, summing all columns of df_split,
  # and add it as a new column to df
  df[[dbh_name]] <- sqrt(rowSums(df_split^2, na.rm=T) )
  
  return(df)  
  
}
