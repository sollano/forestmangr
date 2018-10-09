#' @export

cub_summary <- function(df, dap, ht, vcc, vsc, .groups){
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
  
  # se vcc nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(vcc) ){  
    stop("vcc not set", call. = F) 
  }else if( !is.character(vcc) ){
    stop("'vcc' must be a character containing a variable name", call.=F)
  }else if(length(vcc)!=1){
    stop("Length of 'vcc' must be 1", call.=F)
  }else if(forestr::check_names(df, vcc)==F){
    stop(forestr::check_names(df, vcc, boolean=F), call.=F)
  }
  
  # se vsc nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(vsc) ){  
    stop("vsc not set", call. = F) 
  }else if( !is.character(vsc) ){
    stop("'vsc' must be a character containing a variable name", call.=F)
  }else if(length(vsc)!=1){
    stop("Length of 'vsc' must be 1", call.=F)
  }else if(forestr::check_names(df, vsc)==F){
    stop(forestr::check_names(df, vsc, boolean=F), call.=F)
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
  
  dap_name <- dap
  ht_name <- ht
  vcc_name <- vcc
  vsc_name <- vsc
  
  # funcao para transformar strings em symmbolos que o dplyr entende
  dap_sym <- rlang::sym(dap) 
  ht_sym  <- rlang::sym(ht)
  vcc_sym <- rlang::sym(vcc)
  vsc_sym <- rlang::sym(vsc)
  
  # ####
  
  # !! diz para o dplyr que voce esta lidando com simbolos ou strings
  
  # := e utilizado quando o nome da variavel nova dentro do pipe esta dentro de um objeto
  
   df %>%                                     # define data frame utilizado
    dplyr::na_if(0) %>%                              # Transforma zeros em NA
    dplyr::group_by( !!!.groups_syms )  %>%         # definicao da chave
    dplyr::summarize(                                # Funcao que compila os df
      !!dap_name := mean(!!dap_sym, na.rm = TRUE), # Media de DAP
      !!ht_name  := mean(!!ht_sym,  na.rm = TRUE), # media de HT
      AS          = pi * (!!rlang::sym(dap_name))^2 / 40000       , # Area Seccional
      !!vcc_name := sum(!!vcc_sym,  na.rm = TRUE), # Soma de volume com casca
      !!vsc_name := sum(!!vsc_sym,  na.rm = TRUE), # Soma de volume sem casca
      PORC_CASCA  = (( (!!rlang::sym(vcc_name)) - (!!rlang::sym(vsc_name)) )/ (!!rlang::sym(vcc_name))  )*100    , # Porcentagem da casca
      #  VCIL     = AS *  (!!rlang::sym(ht_name)) ,
      FFCC        = (!!rlang::sym(vcc_name)) / (AS * (!!rlang::sym(ht_name)) )   , # Fator de forma com casca
      FFSC        = (!!rlang::sym(vsc_name)) / (AS * (!!rlang::sym(ht_name)) )   ) %>%     # Fator de forma sem casca
     dplyr::mutate_at(                                # Funcao que cria novas variaveis utilizando as variaveis
      vars(FFCC, FFSC),                   # especificadas por vars
      funs(medio = mean)    ) %>%             # Fator de forma medio
     dplyr::na_if(0) %>%                              # Se vsc nao for informado, variaveis que o utilizam serao 0, portanto, deve-se converte-las para NA, para depois remove-las
     dplyr::select_if(Negate(anyNA))                  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
  
}
