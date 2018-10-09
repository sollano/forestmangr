#' @export

hubersc <- function(df, di, comp_secao, es, .groups, di_mm_to_cm=FALSE, es_mm_to_cm=FALSE   ){
  # Checagem de variaveis ####
  
  # Definir pipe para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) || is.null(df) || is.na(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se di nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(di) ){  
    stop("di not set", call. = F) 
  }else if( !is.character(di) ){
    stop("'di' must be a character containing a variable name", call.=F)
  }else if(length(di)!=1){
    stop("Length of 'di' must be 1", call.=F)
  }else if(forestr::check_names(df, di)==F){
    stop(forestr::check_names(df, di, boolean=F), call.=F)
  }
  
  # se comp_secao nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(comp_secao) ){  
    stop("comp_secao not set", call. = F) 
  }else if( !is.character(comp_secao) ){
    stop("'comp_secao' must be a character containing a variable name", call.=F)
  }else if(length(comp_secao)!=1){
    stop("Length of 'comp_secao' must be 1", call.=F)
  }else if(forestr::check_names(df, comp_secao)==F){
    stop(forestr::check_names(df, comp_secao, boolean=F), call.=F)
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
    .groups_syms <- .groups
  }
  
  # se di_mm_to_cm nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! di_mm_to_cm %in% c(TRUE, FALSE) ){ 
    stop("'di_mm_to_cm' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(di_mm_to_cm)!=1){
    stop("Length of 'di_mm_to_cm' must be 1", call.=F)
  }
  
  # se es_mm_to_cm nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! es_mm_to_cm %in% c(TRUE, FALSE) ){ 
    stop("'es_mm_to_cm' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(es_mm_to_cm)!=1){
    stop("Length of 'es_mm_to_cm' must be 1", call.=F)
  }
  
  # Converter diametro da secao de milimetro para centimetro
    if(di_mm_to_cm){
    df[[di]] <- df[[di]]/10
  }
  
  # Converter espessura da casca de milimetro para centimetro
  if(es_mm_to_cm){
    df[[es]] <- df[[es]]/10
  }
  # ####
  
  di_sym <- rlang::sym(di)
  comp_secao_sym <- rlang::sym(comp_secao)
  es_sym <- rlang::sym(es)
  
  df %>% 
    dplyr::group_by(!!!.groups_syms) %>% 
    dplyr::mutate(
      di_sc =   (!!di_sym)-2*(!!es_sym),
      AS_SC =  (di_sc^2* pi) / 40000, 
      VSC = AS_SC * (!!comp_secao_sym) ) %>% 
    dplyr::ungroup()
}
