#' @export

library(forestmangr)
data("exfm18")
head(exfm18)

tree_summarise(exfm18, "DBH",tree="Tree", .groups=c("Plot", "Species") )


tree_summarise <- function(df,  dbh, tree, th=NULL, vwb=NULL, vwob=NULL, plot_area=NULL, total_area=NULL, .groups=NULL){
  # Checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se dbh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dbh) || dbh == "" ){  
    stop("dbh not set", call. = F) 
  }else if( !is.character(dbh) ){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dbh)==F){
    stop(forestmangr::check_names(df, dbh, boolean=F), call.=F)
  }
  
  # Se tree nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(tree) && is.null(dplyr::groups(df)) ){
    stop("tree not set. tree must be set if data doesn't have any groups", call. = F)
  }else if(missing(tree) && !is.null(dplyr::groups(df)) ){
    tree_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(tree)){
    stop("tree must be a character", call. = F)
  }else if(! length(tree)%in% 1:10){
    stop("Length of 'tree' must be between 1 and 10", call.=F) 
  }else if(forestmangr::check_names(df,tree)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,tree, boolean=F), call.=F) 
  }else{
    tree_syms <- rlang::syms(tree) 
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
    .groups_syms <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){ 
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(forestmangr::check_names(df,.groups)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,.groups, boolean=F), call.=F) 
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    .groups_syms <- rlang::syms(.groups) 
  }
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, criar variavel vazia
  # se for fornecido numero ou noem de variavel, reagir de acordo
  if(  missing(plot_area) || is.null(plot_area) || is.na(plot_area) || plot_area == "" ){  
    df $ plot_area <- NA
    plot_area <- "plot_area"
  }else if(is.numeric(plot_area) & length(plot_area)==1){
    df $ plot_area <- plot_area
    plot_area <- "plot_area"
  }else if(!is.character(plot_area)){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot_area)!=1){
    stop("Length of 'plot_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, plot_area)==F){
    stop(forestmangr::check_names(df, plot_area, boolean = F), call.=F)
  }
  
  # se total_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, criar variavel vazia
  # se for fornecido numero ou noem de variavel, reagir de acordo
  if(  missing(total_area) || is.null(total_area) || is.na(total_area) || total_area == "" ){  
    df $ total_area <- NA
    total_area <- "total_area"
  }else if(is.numeric(total_area) & length(total_area)==1){
    df $ total_area <- total_area
    total_area <- "total_area"
  }else if(!is.character(total_area)){
    stop("'total_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(total_area)!=1){
    stop("Length of 'total_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, total_area)==F){
    stop(forestmangr::check_names(df, total_area, boolean = F), call.=F)
  }
  
  # se th nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(th) || is.null(th) || is.na(th) || th == "" ){
    df $ th <- NA
    th <- "th"
  }else if(!is.character(th)){
    stop("'th' must be a character containing a variable name", call.=F)
  }else if(length(th)!=1){
    stop("Length of 'th' must be 1", call.=F)
  }else if(forestmangr::check_names(df, th)==F){
    stop(forestmangr::check_names(df, th, boolean=F), call.=F)
  }
  
  # se vwb nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(vwb) || is.null(vwb) || is.na(vwb) || vwb == "" ){
    df $ vwb <- NA
    vwb <- "vwb"
  }else if(!is.character(vwb)){
    stop("'vwb' must be a character containing a variable name", call.=F)
  }else if(length(vwb)!=1){
    stop("Length of 'vwb' must be 1", call.=F)
  }else if(forestmangr::check_names(df, vwb)==F){
    stop(forestmangr::check_names(df, vwb, boolean=F), call.=F)
  }
  
  # se vwob nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(vwob) || is.null(vwob) || is.na(vwob) || vwob == "" ){
    df $ vwob <- NA
    vwob <- "vwob"
  }else if(!is.character(vwob)){
    stop("'vwob' must be a character containing a variable name", call.=F)
  }else if(length(vwob)!=1){
    stop("Length of 'vwob' must be 1", call.=F)
  }else if(forestmangr::check_names(df, vwob)==F){
    stop(forestmangr::check_names(df, vwob, boolean=F), call.=F)
  }
  
  dbh_name <- dbh
  th_name <- th
  tree_name <- tree
  plot_area_name <- plot_area
  total_area_name <- total_area
  vwb_name <- vwb
  vwob_name <- vwob
  
  dbh_sym          <- rlang::sym(dbh)
  th_sym           <- rlang::sym(th)
  tree_sym         <- rlang::sym(tree)
  plot_area        <- rlang::sym(plot_area)
  total_area_sym   <- rlang::sym(total_area)
  vwb_sym          <- rlang::sym(vwb)
  vwob_sym          <- rlang::sym(vwob)
  
  # ####
  
  df %>% 
    dplyr::group_by(!!!.groups_syms, !!!tree_sym, add=T) %>% 
    dplyr::summarise(
      !!plot_area_name := mean((!!plot_area), na.rm = TRUE),
      !!total_area_name   := mean((!!total_area_sym), na.rm = TRUE),
      !!dbh_name          := sqrt( sum( (!!dbh_sym)^2, na.rm=T) ),
      !!th_name           := mean((!!th_sym), na.rm = TRUE),
      !!vwb_name          := mean((!!vwb_sym), na.rm = TRUE),
      !!vwob_name          := mean((!!vwob_sym), na.rm = TRUE) ) %>% 
    dplyr::na_if(0) %>% 
    as.data.frame() %>% 
    dplyr::select_if( function(x) !all(is.nan(x)) ) %>% # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NaN)
    dplyr::ungroup()
  
}
