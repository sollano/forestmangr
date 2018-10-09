#' @export

classe_diametro <- function(df, dap, parcela, area_parcela, ic = 5, dapmin = 5, especies=NA, volume=NA, rotulo.NI="NI", cc_to_column=F, G_to_cc=F, cctc_ha=T, keep_unused_classes=F){
  # checagem de variaveis ####
  
  # Definir pipe do dplyr, para facilitar
  `%>%` <- dplyr::`%>%`
  
  
  # ic precisa ser numerico e de tamanho 1
  if(!is.numeric(ic) || length(ic)!=1){
    stop("ic must be a single number", call. = F)
  }
  
  # dapmin precisa ser numerico e de tamanho 1
  if(!is.numeric(dapmin) || length(dapmin)!=1){
    stop("dapmin must be a single number", call. = F)
  }
  
  
  # se parcela nao for fornecida, for NULL, NA ou "", transformar em 1
  if(missing(parcela) || is.null(parcela) || is.na(parcela) ||  parcela==""){
    parcela <- 1
  }
  
  # se area_parcela nao for fornecida, for NULL, NA ou "", transformar em 10000
  if(missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) ||  area_parcela==""){
    area_parcela <- 10000
  }
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se dap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dap) || dap == ""){  
    stop("dap not set", call. = F) 
  }else if( !is.character(dap) ){
    stop("'dap' must be a character containing a variable name", call.=F)
  }else if(length(dap)!=1){
    stop("Length of 'dap' must be 1", call.=F)
  }else if(check_names(df, dap)==F){
    stop(check_names(df, dap, boolean=F), call.=F)
  }
  
  
  
  # se vol nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(volume) || is.null(volume) || is.na(volume) || volume==""){  
    df$vol <- 23
    volume_sym <- rlang::sym("vol")
    
  }else if( !is.character(volume) ){
    stop("'volume' must be a character containing a variable name", call.=F)
  }else if(length(volume)!=1){
    stop("Length of 'volume' must be 1", call.=F)
  }else if(check_names(df, volume)==F){
    stop(check_names(df, volume, boolean=F), call.=F)
  }else{
    volume_sym <- rlang::sym(volume)
    
  }
  
  # Se especies nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(especies)||is.null(especies)||is.na(especies)||especies==F||especies==""){
    especies_sym <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(especies)){
    stop("especies must be a character", call. = F)
  }else if(! length(especies)%in% 1:10){ 
    stop("Length of 'especies' must be between 1 and 10", call.=F)
  }else if(check_names(df,especies)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(check_names(df,especies, boolean=F), call.=F) 
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    
    #app
    if(is.null(rotulo.NI)||rotulo.NI==""){rotulo.NI <- "NI"}
    
    df <- df[! df[[especies]] %in% rotulo.NI, ] 
    especies_sym <- rlang::sym(especies) 
  }
  
  
  # se parcela nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(parcela) ){  
    parcela <- 1
    npar <- parcela
  }else if( is.null(parcela) || is.na(parcela) || parcela == "" ){
    stop("'parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(parcela) & length(parcela)==1){
    
    npar <- parcela
    
  }else if(!is.character(parcela)){
    stop("'parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(parcela)!=1){
    stop("Length of 'parcela' must be 1", call.=F)
  }else if(check_names(df, parcela)==F){
    stop(check_names(df, parcela, boolean = F), call.=F)
  }else{
    parcela_sym <- rlang::sym(parcela)
    npar <- df %>% 
      dplyr::pull(!!parcela_sym) %>%
      as.factor %>% 
      nlevels
  }
  
  # se area_parcela nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(area_parcela) ){  
    area_parcela <- 10000
    area_parcela_num <- area_parcela
  }else if( is.null(area_parcela) || is.na(area_parcela) || area_parcela == "" ){
    stop("'area_parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(area_parcela) & length(area_parcela)==1){
    
    area_parcela_num <- area_parcela
    
  }else if(!is.character(area_parcela)){
    stop("'area_parcela' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(area_parcela)!=1){
    stop("Length of 'area_parcela' must be 1", call.=F)
  }else if(check_names(df, area_parcela)==F){
    stop(check_names(df, area_parcela, boolean = F), call.=F)
  }else{
    area_parcela_sym <- rlang::sym(area_parcela)
    
    area_parcela_num <- df %>% 
      dplyr::group_by( !!!parcela_sym ) %>% 
      dplyr::summarise(AREA = mean(!!area_parcela_sym), na.rm=T) %>% 
      dplyr::summarise(A = mean(AREA, na.rm=T)) %>% 
      dplyr::pull(A) 
    
  }
  
  # Parar se o usuario pedir o resultado por ha sem fornecer as variaveis necessarias
  if(cctc_ha==T & cc_to_column==T &  parcela == 1 & area_parcela == 10000 ){stop("Parcela and area_parcela must be provided if cctc_ha=TRUE",call. = F)}
  
  # Permitir se e nse como entrada de nomes de variaveis
  dap_sym <- rlang::sym(dap)
  
  # ####
  
  df_final <- df %>% 
    dplyr::filter(!is.na( !!dap_sym ) ) %>% # remover NA
    dplyr::mutate(
      CC = ceiling(( !!dap_sym )/ic) * ic - ic/2, # Calcular Centro de classe
      g = pi * (!!dap_sym)^2 / 40000   ) %>%  # Calcular area seccional
    dplyr::group_by(!!!especies_sym, CC ) %>% # Agrupar e calcular o numero de individuos, e n de individuos por ha
    dplyr::summarise(
      NumIndv=n(),
      IndvHA = round( NumIndv / (area_parcela_num/10000 * npar ),  1 ),
      G = sum(g),
      G_ha = sum(g) / (area_parcela_num/10000 * npar ),
      volume = sum( !!volume_sym, na.rm = T  ),
      volume_ha = sum( !!volume_sym, na.rm = T) / (area_parcela_num/10000 * npar )     ) %>% 
    dplyr::mutate(DR =  round(NumIndv/sum(NumIndv) * 100, 4) ) %>% # Calcular densidade relativa
    dplyr::arrange( CC ) %>% 
    dplyr::filter(CC >= dapmin) %>% # Remover classes menores que o dap minimo
    dplyr::ungroup() %>% 
    as.data.frame()
  
  if(keep_unused_classes){
    df_final <- df_final  %>% 
      dplyr::full_join( data.frame(
        CC=seq( min(.$CC),max(.$CC), by=ic)), 
        by=c( "CC" )
      ) %>% 
      dplyr::arrange(CC)
  }
  
  # Remover volume caso nao tenha fornecido pelo usuario
  if(is.na(volume) || is.null(volume) || volume==""){
    
    df_final$volume <- NULL
    df_final$volume_ha <- NULL
  }
  
  # se parcela ou area_parcela nao for fornecida, for NULL, NA ou "", remover variaveis extrapoladas ha
  #if(missing(parcela) || is.null(parcela) || is.na(parcela) ||  parcela=="" || missing(area_parcela) || is.null(area_parcela) || is.na(area_parcela) ||  area_parcela=="" ){
  if(parcela == 1 & area_parcela == 10000){
    df_final$IndvHA <- NULL
    df_final$G_ha <- NULL
    df_final$volume_ha <- NULL
  }
  
  # Se o usuario quiser o centro de classe na coluna e nao tiver fornecido volume,
  # popular o centro de classe com o numero de individuos
  if(cc_to_column==T &&  G_to_cc==T ){
    
    if(cctc_ha==T){df_final$G_f <- df_final$G_ha}else if(cctc_ha==F){df_final$G_f <- df_final$G}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      dplyr::select(!!especies_sym,CC,G_f) %>% 
      tidyr::spread(CC,G_f, fill = 0) %>% 
      dplyr::mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && (!missing(especies)||!is.null(especies)||!is.na(especies)||especies!=F||especies!="") && (missing(volume) || is.null(volume) || is.na(volume) || volume=="") ){
    
    if(cctc_ha==T){df_final$NI <- df_final$IndvHA}else if(cctc_ha==F){df_final$NI <- df_final$NumIndv}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      dplyr::select(!!especies_sym,CC,NI) %>% 
      tidyr::spread(CC,NI, fill = 0 ) %>% 
      dplyr::mutate(Total = rowSums(.[  ,  sapply(., is.numeric)  ]  ) ) %>% 
      as.data.frame %>% 
      round_df(4)
    
    df_final[df_final==0] <- ""
    # Se o usuario quiser o centro de classe na coluna e tiver fornecido volume,
    # popular o centro de classe com o volume
  }else if(cc_to_column==T && (!missing(especies)||!is.null(especies)||!is.na(especies)||especies!=F||especies!="") && (!missing(volume) || !is.null(volume) || !is.na(volume) || volume!="" ) ){
    
    if(cctc_ha==T){df_final$VOL<- df_final$volume_ha}else if(cctc_ha==F){df_final$VOL<- df_final$volume}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    df_final <- df_final %>% 
      dplyr::select(!!especies_sym,CC,VOL) %>% 
      tidyr::spread(CC,VOL, fill = 0) %>% 
      dplyr::mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && (missing(especies)||is.null(especies)||is.na(especies)||especies==F||especies=="") ){
    
    stop("Especies column must be provided if cc_to_column is true ", call. = F)
    
  }
  
  return(df_final)
  
}
