#' @export

arv_summary <- function(df, arvore, dap, .groups, area_parcela, area_total, ht, vcc, vsc){
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
  
  # se arvore nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(arvore) || arvore == "" ){  
    stop("arvore not set", call. = F) 
  }else if( !is.character(arvore) ){
    stop("'arvore' must be a character containing a variable name", call.=F)
  }else if(length(arvore)!=1){
    stop("Length of 'arvore' must be 1", call.=F)
  }else if(forestr::check_names(df, arvore)==F){
    stop(forestr::check_names(df, arvore, boolean=F), call.=F)
  }
  
  # se dap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dap) || dap == "" ){  
    stop("dap not set", call. = F) 
  }else if( !is.character(dap) ){
    stop("'dap' must be a character containing a variable name", call.=F)
  }else if(length(dap)!=1){
    stop("Length of 'dap' must be 1", call.=F)
  }else if(forestr::check_names(df, dap)==F){
    stop(forestr::check_names(df, dap, boolean=F), call.=F)
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
  
  # se area_parcela nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, criar variavel vazia
  # se for fornecido numero ou noem de variavel, reagir de acordo
  if(  missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) || area_parcela == "" ){  
    df $ area_parcela <- NA
    area_parcela <- "area_parcela"
  }else if(is.numeric(area_parcela) & length(area_parcela)==1){
    df $ area_parcela <- area_parcela
    area_parcela <- "area_parcela"
  }else if(!is.character(area_parcela)){
    stop("'area_parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(area_parcela)!=1){
    stop("Length of 'area_parcela' must be 1", call.=F)
  }else if(forestr::check_names(df, area_parcela)==F){
    stop(forestr::check_names(df, area_parcela, boolean = F), call.=F)
  }
  
  # se area_total nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, criar variavel vazia
  # se for fornecido numero ou noem de variavel, reagir de acordo
  if(  missing(area_total) || is.null(area_total) || is.na(area_total) || area_total == "" ){  
    df $ area_total <- NA
    area_total <- "area_total"
  }else if(is.numeric(area_total) & length(area_total)==1){
    df $ area_total <- area_total
    area_total <- "area_total"
  }else if(!is.character(area_total)){
    stop("'area_total' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(area_total)!=1){
    stop("Length of 'area_total' must be 1", call.=F)
  }else if(forestr::check_names(df, area_total)==F){
    stop(forestr::check_names(df, area_total, boolean = F), call.=F)
  }
  
  # se ht nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(ht) || is.null(ht) || is.na(ht) || ht == "" ){
    df $ ht <- NA
    ht <- "ht"
  }else if(!is.character(ht)){
    stop("'ht' must be a character containing a variable name", call.=F)
  }else if(length(ht)!=1){
    stop("Length of 'ht' must be 1", call.=F)
  }else if(forestr::check_names(df, ht)==F){
    stop(forestr::check_names(df, ht, boolean=F), call.=F)
  }
  
  # se vcc nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(vcc) || is.null(vcc) || is.na(vcc) || vcc == "" ){
    df $ vcc <- NA
    vcc <- "vcc"
  }else if(!is.character(vcc)){
    stop("'vcc' must be a character containing a variable name", call.=F)
  }else if(length(vcc)!=1){
    stop("Length of 'vcc' must be 1", call.=F)
  }else if(forestr::check_names(df, vcc)==F){
    stop(forestr::check_names(df, vcc, boolean=F), call.=F)
  }
  
  # se vsc nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(vsc) || is.null(vsc) || is.na(vsc) || vsc == "" ){
    df $ vsc <- NA
    vsc <- "vsc"
  }else if(!is.character(vsc)){
    stop("'vsc' must be a character containing a variable name", call.=F)
  }else if(length(vsc)!=1){
    stop("Length of 'vsc' must be 1", call.=F)
  }else if(forestr::check_names(df, vsc)==F){
    stop(forestr::check_names(df, vsc, boolean=F), call.=F)
  }
  
  dap_name <- dap
  ht_name <- ht
  arvore_name <- arvore
  area_parcela_name <- area_parcela
  area_total_name <- area_total
  vcc_name <- vcc
  vsc_name <- vsc
  
  dap_sym          <- rlang::sym(dap)
  ht_sym           <- rlang::sym(ht)
  arvore_sym       <- rlang::sym(arvore)
  area_parcela_sym <- rlang::sym(area_parcela)
  area_total_sym   <- rlang::sym(area_total)
  vcc_sym          <- rlang::sym(vcc)
  vsc_sym          <- rlang::sym(vsc)
  
  # ####
  
  df %>% 
    dplyr::group_by(!!!.groups_syms, !!arvore_sym) %>% 
    dplyr::summarise(
      !!area_parcela_name := mean((!!area_parcela_sym), na.rm = TRUE),
      !!area_total_name   := mean((!!area_total_sym), na.rm = TRUE),
      !!dap_name          := sqrt( sum( (!!dap_sym)^2, na.rm=T) ),
      !!ht_name           := mean((!!ht_sym), na.rm = TRUE),
      !!vcc_name          := mean((!!vcc_sym), na.rm = TRUE),
      !!vsc_name          := mean((!!vsc_sym), na.rm = TRUE) ) %>% 
    dplyr::na_if(0) %>% 
    as.data.frame() %>% 
    dplyr::select_if( function(x) !all(is.nan(x)) ) %>% # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NaN)
    dplyr::ungroup()
  
}
