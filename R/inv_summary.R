#' @export
inv_summary <- function(df, DAP, HT, VCC, area_parcela, .groups, area_total,idade,VSC,Hd, casas_decimais = 4) {
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
  
  # se DAP nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(DAP) || DAP == "" ){  
    stop("DAP not set", call. = F) 
  }else if( !is.character(DAP) ){
    stop("'DAP' must be a character containing a variable name", call.=F)
  }else if(length(DAP)!=1){
    stop("Length of 'DAP' must be 1", call.=F)
  }else if(forestr::check_names(df, DAP)==F){
    stop(forestr::check_names(df, DAP, boolean=F), call.=F)
  }
  

  # se HT nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(HT) || is.null(HT) || is.na(HT) || HT == "" ){
    df $ HT <- NA
    HT <- "HT"
  }else if(!is.character(HT)){
    stop("'HT' must be a character containing a variable name", call.=F)
  }else if(length(HT)!=1){
    stop("Length of 'HT' must be 1", call.=F)
  }else if(forestr::check_names(df, HT)==F){
    stop(forestr::check_names(df, HT, boolean=F), call.=F)
  }
  
  # se VCC nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(VCC) || VCC == ""){  
    stop("VCC not set", call. = F) 
  }else if( !is.character(VCC) ){
    stop("'VCC' must be a character containing a variable name", call.=F)
  }else if(length(VCC)!=1){
    stop("Length of 'VCC' must be 1", call.=F)
  }else if(forestr::check_names(df, VCC)==F){
    stop(forestr::check_names(df, VCC, boolean=F), call.=F)
  }
  
  # se area_parcela nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) || area_parcela == "" ){  
    stop("area_parcela not set", call. = F) 
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
  # Se for fornecida verificar se e numerica ou nome de variavel
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
  
  # se idade nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(idade) || is.null(idade) || is.na(idade) || idade == "" ){
    df $ idade <- NA
    idade <- "idade"
  }else if(!is.character(idade)){
    stop("'idade' must be a character containing a variable name", call.=F)
  }else if(length(idade)!=1){
    stop("Length of 'idade' must be 1", call.=F)
  }else if(forestr::check_names(df, idade)==F){
    stop(forestr::check_names(df, idade, boolean=F), call.=F)
  }
  
  # se VSC nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(VSC) || is.null(VSC) || is.na(VSC) || VSC == "" ){
    df $ VSC <- NA
    VSC <- "VSC"
  }else if(!is.character(VSC)){
    stop("'VSC' must be a character containing a variable name", call.=F)
  }else if(length(VSC)!=1){
    stop("Length of 'VSC' must be 1", call.=F)
  }else if(forestr::check_names(df, VSC)==F){
    stop(forestr::check_names(df, VSC, boolean=F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
    .groups_syms <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(forestr::check_names(df,.groups)==F ){
    # Parar se algum nome nao existir, e avisar qual nome nao existe 
    stop(forestr::check_names(df,.groups, boolean=F), call.=F )
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    .groups_syms <- rlang::syms(.groups) 
  }
  
  # Se casas_decimais nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( casas_decimais )){
    stop( "'casas_decimais' must be numeric", call.=F)
  }else if(length(casas_decimais)!=1){
    stop("Length of 'casas_decimais' must be 1", call.=F)
  }else if(! casas_decimais %in%  seq(from=0,to=9,by=1) ){
    stop("'casas_decimais' must be a number between 0 and 9", call.=F)
  }
  
  DAP_name <- DAP
  HT_name <- HT
  area_parcela_name <- area_parcela
  area_total_name <- area_total
  VCC_name <- VCC
  VSC_name <- VSC
  idade_name <- idade
  
  
  DAP_sym <- rlang::sym( DAP )
  HT_sym <- rlang::sym( HT )
  VCC_sym <- rlang::sym( VCC )
  area_parcela_sym <- rlang::sym( area_parcela )
  area_total_sym <- rlang::sym( area_total )
  idade_sym <- rlang::sym( idade )
  VSC_sym <- rlang::sym( VSC )
  
  # ####
  
  if(missing(Hd) || Hd=="" || is.null(Hd) || is.na(Hd) ){ # se a altura dominante nao for fornecida
    
    # se ja existir uma variavel chamada "HD", deletar
    if(  "HD" %in% names(df) ){ df$HD <- NULL }
    
    # estimar altura dominante
    x <- forestr::hdjoin(df = df, HT = HT,.groups= .groups)
    
    # caso contrario, renomear "Hd" para "HD"
  } else{ 
    Hd_sym <- rlang::sym( Hd )
    x <- df %>% dplyr::rename(HD = !!Hd_sym) }
  # novo nome = nome antigo
  
  x %>% 
    dplyr::group_by(!!!.groups_syms,add=T) %>% 
    dplyr::mutate(AS = pi * (!!DAP_sym)^2 / 40000 ) %>% 
    dplyr::summarise(
      !!idade_name        := round( mean(as.numeric( (!!idade_sym) ), na.rm=T) ),
      !!area_total_name   := mean( !!area_total_sym, na.rm=T), 
      !!area_parcela_name := mean( !!area_parcela_sym, na.rm=T),
      !!DAP_name          := mean(!!DAP_sym, na.rm=T),
      q            = sqrt(mean(AS, na.rm=T) * 40000 / pi),
      !!HT_name    := mean(!!HT_sym, na.rm=T),
      HD           = mean(HD),
      Indv         = n(),
      IndvHA       = Indv* 10000/(!!area_parcela_sym),
      G            = sum(AS, na.rm=T),
      G_HA         = G * 10000/(!!area_parcela_sym),
      VCC          = sum(!!VCC_sym, na.rm=T),
      VCC_HA       = VCC * 10000/ (!!area_parcela_sym),
      VSC          = sum(!!VSC_sym, na.rm=T),
      VSC_HA       = VSC * 10000/ (!!area_parcela_sym)  ) %>% #sumarise 
    dplyr::na_if(0) %>% # substitui 0 por NA
    dplyr::select_if(Negate(anyNA)) %>%  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    forestr::round_df(casas_decimais)
}
