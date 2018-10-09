fund_dap <- function(df, dap, .sep){
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
  
  # se dap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dap) ){  
    stop("dap not set", call. = F) 
  }else if( !is.character(dap) ){
    stop("'dap' must be a character containing a variable name", call.=F)
  }else if(length(dap)!=1){
    stop("Length of 'dap' must be 1", call.=F)
  }else if(forestr::check_names(df, dap)==F){
    stop(forestr::check_names(df, dap, boolean=F), call.=F)
  }
  
  # Se sep nao for character,ou nao for de tamanho 1, parar
  if(!is.character( sep )){
    stop( "'sep' must be character", call.=F)
  }else if(length(sep)!=1){
    stop("Length of 'sep' must be 1", call.=F)
  }else if(! sep %in% c(';', ',', '-',' ') ){ 
  stop("'sep' must be equal to ';', ',', '-',' ' ", call. = F) 
  }
  
  dap_sym <- rlang::sym(dap)
  
  # ####
  
  dap_old <- df %>% dplyr::pull(!!dap_sym)
  
  list_split <- strsplit(dap_old,  .sep, fixed=T)
  list_split <- sapply(list_split, gsub, pattern=",",replacement= ".")
  list_split <- sapply(list_split, as.numeric)
  
  n.obs <- sapply(list_split, length) 
  seq.max <- seq_len(max(n.obs))
  
  df_split <- as.data.frame( t(sapply(list_split, "[", i = seq.max))  )
  df_split
  
  df$dap <- sqrt(rowSums(df_split^2, na.rm=T) )
  
  return(df)  
  
}
