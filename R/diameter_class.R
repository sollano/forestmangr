#' @export

diameter_class <- function(df, dbh, plot, plot_area, ci = 5, dbhmin = 5, species=NA, volume=NA, NI_label="NI", cc_to_column=F, G_to_cc=F, cctc_ha=T, keep_unused_classes=F){
  # checagem de variaveis ####

  # ci precisa ser numerico e de tamanho 1
  if(!is.numeric(ci) || length(ci)!=1){
    stop("ci must be a single number", call. = F)
  }
  
  # dbhmin precisa ser numerico e de tamanho 1
  if(!is.numeric(dbhmin) || length(dbhmin)!=1){
    stop("dbhmin must be a single number", call. = F)
  }
  
  
  # se plot nao for fornecida, for NULL, NA ou "", transformar em 1
  if(missing(plot) || is.null(plot) || is.na(plot) ||  plot==""){
    plot <- 1
  }
  
  # se plot_area nao for fornecida, for NULL, NA ou "", transformar em 10000
  if(missing(plot_area) || is.null(plot_area) || is.na(plot_area) ||  plot_area==""){
    plot_area <- 10000
  }
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se dbh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dbh) || dbh == ""){  
    stop("dbh not set", call. = F) 
  }else if( !is.character(dbh) ){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(check_names(df, dbh)==F){
    stop(check_names(df, dbh, boolean=F), call.=F)
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
  
  # Se species nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(species)||is.null(species)||is.na(species)||species==F||species==""){
    species_sym <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(species)){
    stop("species must be a character", call. = F)
  }else if(! length(species)%in% 1:10){ 
    stop("Length of 'species' must be between 1 and 10", call.=F)
  }else if(check_names(df,species)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(check_names(df,species, boolean=F), call.=F) 
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    
    #app
    if(is.null(NI_label)||NI_label==""){NI_label <- "NI"}
    
    df <- df[! df[[species]] %in% NI_label, ] 
    species_sym <- rlang::sym(species) 
  }
  
  
  # se plot nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot) ){  
    plot <- 1
    nplot <- plot
  }else if( is.null(plot) || is.na(plot) || plot == "" ){
    stop("'plot' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(plot) & length(plot)==1){
    
    nplot <- plot
    
  }else if(!is.character(plot)){
    stop("'plot' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot)!=1){
    stop("Length of 'plot' must be 1", call.=F)
  }else if(check_names(df, plot)==F){
    stop(check_names(df, plot, boolean = F), call.=F)
  }else{
    plot_sym <- rlang::sym(plot)
    nplot <- df %>% 
      dplyr::pull(!!plot_sym) %>%
      as.factor %>% 
      nlevels
  }
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot_area) ){  
    plot_area <- 10000
    plot_area_num <- plot_area
  }else if( is.null(plot_area) || is.na(plot_area) || plot_area == "" ){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(plot_area) & length(plot_area)==1){
    
    plot_area_num <- plot_area
    
  }else if(!is.character(plot_area)){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot_area)!=1){
    stop("Length of 'plot_area' must be 1", call.=F)
  }else if(check_names(df, plot_area)==F){
    stop(check_names(df, plot_area, boolean = F), call.=F)
  }else{
    plot_area_sym <- rlang::sym(plot_area)
    
    plot_area_num <- df %>% 
      dplyr::group_by( !!!plot_sym ) %>% 
      dplyr::summarise(AREA = mean(!!plot_area_sym), na.rm=T) %>% 
      dplyr::summarise(A = mean(AREA, na.rm=T)) %>% 
      dplyr::pull(A) 
    
  }
  
  # Parar se o usuario pedir o resultado por ha sem fornecer as variaveis necessarias
  if(cctc_ha==T & cc_to_column==T &  plot == 1 & plot_area == 10000 ){stop("plot and plot_area must be provided if cctc_ha=TRUE",call. = F)}
  
  # Permitir se e nse como entrada de nomes de variaveis
  dbh_sym <- rlang::sym(dbh)
  
  # ####
  
  df_final <- df %>% 
    dplyr::filter(!is.na( !!dbh_sym ) ) %>% # remover NA
    dplyr::mutate(
      CC = ceiling(( !!dbh_sym )/ci) * ci - ci/2, # Calcular Centro de classe
      g = pi * (!!dbh_sym)^2 / 40000   ) %>%  # Calcular area seccional
    dplyr::group_by(!!!species_sym, CC ) %>% # Agrupar e calcular o numero de individuos, e n de individuos por ha
    dplyr::summarise(
      NumIndv=n(),
      IndvHA = round( NumIndv / (plot_area_num/10000 * nplot ),  1 ),
      G = sum(g),
      G_ha = sum(g) / (plot_area_num/10000 * nplot ),
      volume = sum( !!volume_sym, na.rm = T  ),
      volume_ha = sum( !!volume_sym, na.rm = T) / (plot_area_num/10000 * nplot )     ) %>% 
    dplyr::mutate(DR =  round(NumIndv/sum(NumIndv) * 100, 4) ) %>% # Calcular densidade relativa
    dplyr::arrange( CC ) %>% 
    dplyr::filter(CC >= dbhmin) %>% # Remover classes menores que o dbh minimo
    dplyr::ungroup() %>% 
    as.data.frame()
  
  if(keep_unused_classes){
    df_final <- df_final  %>% 
      dplyr::full_join( data.frame(
        CC=seq( min(.$CC),max(.$CC), by=ci)), 
        by=c( "CC" )
      ) %>% 
      dplyr::arrange(CC)
  }
  
  # Remover volume caso nao tenha fornecido pelo usuario
  if(is.na(volume) || is.null(volume) || volume==""){
    
    df_final$volume <- NULL
    df_final$volume_ha <- NULL
  }
  
  # se plot ou plot_area nao for fornecida, for NULL, NA ou "", remover variaveis extrapoladas ha
  #if(missing(plot) || is.null(plot) || is.na(plot) ||  plot=="" || missing(plot_area) || is.null(plot_area) || is.na(plot_area) ||  plot_area=="" ){
  if(plot == 1 & plot_area == 10000){
    df_final$IndvHA <- NULL
    df_final$G_ha <- NULL
    df_final$volume_ha <- NULL
  }
  
  # Se o usuario quiser o centro de classe na coluna e nao tiver fornecido volume,
  # popular o centro de classe com o numero de individuos
  if(cc_to_column==T &&  G_to_cc==T ){
    
    if(cctc_ha==T){df_final$G_f <- df_final$G_ha}else if(cctc_ha==F){df_final$G_f <- df_final$G}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      dplyr::select(!!species_sym,CC,G_f) %>% 
      tidyr::spread(CC,G_f, fill = 0) %>% 
      dplyr::mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && (!missing(species)||!is.null(species)||!is.na(species)||species!=F||species!="") && (missing(volume) || is.null(volume) || is.na(volume) || volume=="") ){
    
    if(cctc_ha==T){df_final$NI <- df_final$IndvHA}else if(cctc_ha==F){df_final$NI <- df_final$NumIndv}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    
    df_final <- df_final %>% 
      dplyr::select(!!species_sym,CC,NI) %>% 
      tidyr::spread(CC,NI, fill = 0 ) %>% 
      dplyr::mutate(Total = rowSums(.[  ,  sapply(., is.numeric)  ]  ) ) %>% 
      as.data.frame %>% 
      round_df(4)
    
    df_final[df_final==0] <- ""
    # Se o usuario quiser o centro de classe na coluna e tiver fornecido volume,
    # popular o centro de classe com o volume
  }else if(cc_to_column==T && (!missing(species)||!is.null(species)||!is.na(species)||species!=F||species!="") && (!missing(volume) || !is.null(volume) || !is.na(volume) || volume!="" ) ){
    
    if(cctc_ha==T){df_final$VOL<- df_final$volume_ha}else if(cctc_ha==F){df_final$VOL<- df_final$volume}else(stop("cctc_ha must be TRUE or FALSE",call. = F))
    df_final <- df_final %>% 
      dplyr::select(!!species_sym,CC,VOL) %>% 
      tidyr::spread(CC,VOL, fill = 0) %>% 
      dplyr::mutate(Total=rowSums(.[, sapply(., is.numeric)]) ) %>% 
      as.data.frame %>% 
      round_df(4)
    df_final[df_final==0] <- ""
    
  }else if(cc_to_column==T && (missing(species)||is.null(species)||is.na(species)||species==F||species=="") ){
    
    stop("species column must be provided if cc_to_column is true ", call. = F)
    
  }
  
  return(df_final)
  
}
