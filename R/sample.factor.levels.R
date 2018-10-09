#' @export

sample.factor.levels <- function(df,factor, n, rep = F){
  # Checagem de variaveis ####
  
  # Definir pipe para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se factor nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(factor) ){  
    stop("factor not set", call. = F) 
  }else if( !is.character(factor) ){
    stop("'factor' must be a character containing a variable name", call.=F)
  }else if(length(factor)!=1){
    stop("Length of 'factor' must be 1", call.=F)
  }else if(forestr::check_names(df, factor)==F){
    stop(forestr::check_names(df, factor, boolean=F), call.=F)
  }
  
  # Se n nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( n )){
    stop( "'n' must be numeric", call.=F)
  }else if(length(n)!=1){
    stop("length of 'n' must be 1", call.=F)
  }else if(! n > 0 | ! n <= nlevels(as.factor(df[[factor]]))){
    stop("'n' must be a number between 0 and nlevels of factor", call.=F)
  }
  
  # se rep nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! rep %in% c(TRUE, FALSE) ){ 
    stop("'rep' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(rep)!=1){
    stop("Length of 'rep' must be 1", call.=F)
  }
  
  factor_sym <- rlang::sym(factor)
  
  # ####
  
  df %>%
    dplyr::group_by(!!factor_sym) %>%
    dplyr::summarise(a = n()) %>%
    dplyr::sample_n(n, replace = rep) %>%
    dplyr::select(!!factor_sym) %>%
    .[[1]]
  
}
