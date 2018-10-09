#' @export

struct_form <- function(df, Id, HD, B, V, S, .groups){
  # checagem de variaveis ####
  
  # Definir pipe do dplyr, para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se struct_form_df nao for igual a TRUE ou FALSE
  if(!   struct_form_df %in% c(TRUE, FALSE) ){ 
    stop("struct_form_df must be equal to TRUE or FALSE", call. = F) 
  }
  
    # se Id nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(Id) ){  
      stop("Id not set", call. = F) 
    }else if( !is.character(Id) ){
      stop("'Id' must be a character containing a variable name", call.=F)
    }else if(length(Id)!=1){
      stop("Length of 'Id' must be 1", call.=F)
    }else if(forestr::check_names(df, Id)==F){
      stop(forestr::check_names(df, Id, boolean=F), call.=F)
    }
    
    # se HD nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(HD) ){  
      stop("HD not set", call. = F) 
    }else if( !is.character(HD) ){
      stop("'HD' must be a character containing a variable name", call.=F)
    }else if(length(HD)!=1){
      stop("Length of 'HD' must be 1", call.=F)
    }else if(forestr::check_names(df, HD)==F){
      stop(forestr::check_names(df, HD, boolean=F), call.=F)
    }
    
    # se B nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(B) ){  
      stop("B not set", call. = F) 
    }else if( !is.character(B) ){
      stop("'B' must be a character containing a variable name", call.=F)
    }else if(length(B)!=1){
      stop("Length of 'B' must be 1", call.=F)
    }else if(forestr::check_names(df, B)==F){
      stop(forestr::check_names(df, B, boolean=F), call.=F)
    }
    
    # se V nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(V) ){  
      stop("V not set", call. = F) 
    }else if( !is.character(V) ){
      stop("'V' must be a character containing a variable name", call.=F)
    }else if(length(V)!=1){
      stop("Length of 'V' must be 1", call.=F)
    }else if(forestr::check_names(df, V)==F){
      stop(forestr::check_names(df, V, boolean=F), call.=F)
    }
    
    # se S nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(S) ){  
      stop("S not set", call. = F) 
    }else if( !is.character(S) ){
      stop("'S' must be a character containing a variable name", call.=F)
    }else if(length(S)!=1){
      stop("Length of 'S' must be 1", call.=F)
    }else if(forestr::check_names(df, S)==F){
      stop(forestr::check_names(df, S, boolean=F), call.=F)
    }
    # se Id nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(Id) ){  
      stop("Id not set", call. = F) 
    }else if( !is.character(Id) ){
      stop("'Id' must be a character containing a variable name", call.=F)
    }else if(length(Id)!=1){
      stop("Length of 'Id' must be 1", call.=F)
    }else if(forestr::check_names(df, Id)==F){
      stop(forestr::check_names(df, Id, boolean=F), call.=F)
    }
    
    # se HD nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(HD) ){  
      stop("HD not set", call. = F) 
    }else if( !is.character(HD) ){
      stop("'HD' must be a character containing a variable name", call.=F)
    }else if(length(HD)!=1){
      stop("Length of 'HD' must be 1", call.=F)
    }else if(forestr::check_names(df, HD)==F){
      stop(forestr::check_names(df, HD, boolean=F), call.=F)
    }
    
    # se B nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(B) ){  
      stop("B not set", call. = F) 
    }else if( !is.character(B) ){
      stop("'B' must be a character containing a variable name", call.=F)
    }else if(length(B)!=1){
      stop("Length of 'B' must be 1", call.=F)
    }else if(forestr::check_names(df, B)==F){
      stop(forestr::check_names(df, B, boolean=F), call.=F)
    }
    
    # se V nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(V) ){  
      stop("V not set", call. = F) 
    }else if( !is.character(V) ){
      stop("'V' must be a character containing a variable name", call.=F)
    }else if(length(V)!=1){
      stop("Length of 'V' must be 1", call.=F)
    }else if(forestr::check_names(df, V)==F){
      stop(forestr::check_names(df, V, boolean=F), call.=F)
    }
    
    # se S nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(S) ){  
      stop("S not set", call. = F) 
    }else if( !is.character(S) ){
      stop("'S' must be a character containing a variable name", call.=F)
    }else if(length(S)!=1){
      stop("Length of 'S' must be 1", call.=F)
    }else if(forestr::check_names(df, S)==F){
      stop(forestr::check_names(df, S, boolean=F), call.=F)
    }
    
    # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
    if(missing(.groups) && is.null(dplyr::groups(df))){
      stop(".groups must be set if data doesn't have any groups", call. = F)
    }else if(missing(.groups) && !is.null(dplyr::groups(df))){
      .groups_syms <- rlang::syms(dplyr::groups(df))
    }else if(!is.character(.groups)){
      stop(".groups must be a character", call. = F)
    }else if(! length(.groups)%in% 1:10){
      stop("Length of '.groups' must be between 1 and 10", call.=F)
    }else if(forestr::check_names(df,.groups)==F){
      # Parar se algum nome nao existir, e avisar qual nome nao existe
      stop(forestr::check_names(df,.groups, boolean=F), call.=F) 
      # se os grupos forem fornecidos e forem nomes dos dados
      # Transformar o objeto em simbolo, para que dplyr entenda
      # e procure o nome das variaveis dentro dos objetos
    }else{
      .groups_syms <- rlang::syms(.groups) 
    }
    
Id_sym <- rlang::sym(Id)
HD_sym <- rlang::sym(HD)
B_sym <- rlang::sym(B)
V_sym <- rlang::sym(V)
S_sym <- rlang::sym(S)
    
suppressMessages(
  
  df %>% 
    dplyr::group_by( !!!.groups_syms, add=T ) %>% 
    dplyr::transmute(
      I1 = !!Id_sym, I2  = dplyr::lead(!!Id_sym), 
      HD = !!HD_sym, HD2 = dplyr::lead(!!HD_sym), 
      B1 = !!B_sym,  B2  = dplyr::lead(!!B_sym), 
      V1 = !!V_sym,  V2  = dplyr::lead(!!V_sym),
      S  = !!S_sym   ) %>% 
    stats::na.omit() %>% 
    dplyr::mutate(
      Y1 = log(B2)          ,
      X1 = log(B1) * (I1/I2),
      X2 = 1 - I1/I2        ,
      X3 = (1 - I1/I2) * S  ,
      Y2 = log(V2)          ,
      X4 = 1 / I2           ,
      X5 = S
    ) %>% 
    dplyr::ungroup()
  
  
)  
  
}

