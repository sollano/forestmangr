class_center <- function(df, y, ci = 3, ymin = 5){
  
  # ####
  
  if(missing(df) & !missing(y)){
    
    if(!is.numeric(y)) stop("'y' must be numeric if df is missing",call. = FALSE)
    
    return( ymin + (ci/2) * (1 + trunc((y-ymin)/ci) * 2)   )
    
  }else if(  missing(df) ){  
    stop("df not set", call. = F) 
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
  
  # Se ic nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( ci )){
    stop( "'ci' must be numerci", call.=F)
  }else if(length(ci)!=1){
    stop("length of 'ci' must be 1", call.=F)
  }else if(! ci > 0 | ! ci <= max(df[[y]])){
    stop("'ci' must be a number between 0 and max y", call.=F)
  }
  
  # Se ymin nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( ymin )){
    stop( "'ymin' must be numeric", call.=F)
  }else if(length(ymin)!=1){
    stop("length of 'ymin' must be 1", call.=F)
  }else if(! ymin > 0 | ! ymin <= max(df[[y]])){
    stop("'ymin' must be a number between 0 and max y", call.=F)
  }
  # ####
  
  y_sym <- rlang::sym(y)
  
  df %>% 
    dplyr::mutate(CC = ymin + (ci/2) * (1 + trunc( ((!!y_sym) - ymin) /ci) * 2) )
  
}