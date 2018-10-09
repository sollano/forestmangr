#' @export
hdjoin <- function(df, HT, DAP, OBS, dom, .groups){
  # checagem de variaveis ####
  
  # Definir pipe do dplyr, para facilitar
  `%>%` <- dplyr::`%>%`
  
  df[["HD"]] <- NULL
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se HT nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(HT) ){  
    stop("HT not set", call. = F) 
  }else if( !is.character(HT) ){
    stop("'HT' must be a character containing a variable name", call.=F)
  }else if(length(HT)!=1){
    stop("Length of 'HT' must be 1", call.=F)
  }else if(forestr::check_names(df, HT)==F){
    stop(forestr::check_names(df, HT, boolean=F), call.=F)
  }
  
  # se DAP nao for fornecido, for igual "", nulo ou NA, nao fazer nada
  # se existir e nao for character,  parar
  if(missing(DAP) || is.null(DAP) || is.na(DAP) || DAP == "" ){
    
    
  }else if(!is.character(DAP)){
    stop("'DAP' must be a character containing a variable name", call.=F)
  }else if(length(DAP)!=1){
    stop("Length of 'DAP' must be 1", call.=F)
  }else if(forestr::check_names(df, DAP)==F){
    stop(forestr::check_names(df, DAP, boolean=F), call.=F)
  }
  
  # se OBS nao for fornecido, for igual "", nulo ou NA, fazer nada
  # se existir e nao for character,  parar
  if(missing(OBS) || is.null(OBS) || is.na(OBS) || OBS == "" ){
    
    
  }else if(!is.character(OBS)){
    stop("'OBS' must be a character containing a variable name", call.=F)
  }else if(length(OBS)!=1){
    stop("Length of 'OBS' must be 1", call.=F)
  }else if(forestr::check_names(df, OBS)==F){
    stop(forestr::check_names(df, OBS, boolean=F), call.=F)
  }
  
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
    .groups_syms <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
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
  # ####
  
  HT_sym <- rlang::sym( HT )
  
  if(missing(OBS) || is.null(OBS) || is.na(OBS) || OBS=="" || missing(DAP) || is.null(DAP) || is.na(DAP) || DAP == "" ){
    
    if( missing(.groups) ){
      suppressMessages(   # remove mensagens do dplyr
        df %>% 
          dplyr::top_n(2) %>% # seleciona as duas maiores arvores
          dplyr::select(!!HT_sym) %>% 
          dplyr::summarise(HD = mean(!!HT_sym, na.rm=T) ) %>% 
          cbind(df) # como nao ha .groups, usamos cbind
      )    }else{
        suppressMessages(
          
          df %>% 
            dplyr::group_by(!!!.groups_syms) %>% 
            dplyr::select(!!HT_sym) %>% 
            dplyr::top_n(2) %>% 
            dplyr::summarise(HD = mean(!!HT_sym, na.rm=T) ) %>% 
            dplyr::full_join(df) # como ha .groups, usamos join
          
        )
      }
    
    
  }else{
    
    DAP_sym <- rlang::sym( DAP )
    OBS_sym <- rlang::sym( OBS )
    
    x <- df %>%
      dplyr::group_by(!!!.groups_syms) %>%
      dplyr::filter( 
        !is.na(!!HT_sym) , # filtra alturas nao medidas
        !is.na(!!DAP_sym), # filtra arvores nao medidas
        (!!OBS_sym) == dom # filtra arvores dominantes
      ) %>%
      dplyr::summarise(HD = mean(!!HT_sym, na.rm = T) ) %>%
      dplyr::ungroup()
    
    
    df %>%
      dplyr::filter( !is.na(!!DAP_sym) ) %>%
      dplyr::left_join(x, by = .groups)
    
    
  }
  
}
