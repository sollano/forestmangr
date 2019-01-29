#' @title
#' Calculate the equivalent diameter of trees with more than one trunk
#' @description 
#' This function uses takes the square root of the diameters squared sum,
#' in order to estimate the equivalent diameter of trees. Other supplied variables
#' are summed up, or averaged, depending on the variable.
#'
#' @param df A data frame.
#' @param dbh Quoted name of the diameter at breast height variable.
#' @param tree Quoted name of the tree variable. used to differentiate the trees' sections. If this argument is missing, the defined groups in the data frame will be used. If there are no groups in the data, the function will fail.
#' @param .groups Optional argument. Quoted name(s) of grouping variables that can be added to differentiate subdivisions of the data. Default: \code{NA}.
#' @param vwb Optional argument. Quoted name of the volume with bark variable, in cubic meters. Default: \code{NA}.
#' @param vwob Optional argument. Quoted name of the volume without bark variable, in cubic meters. Default: \code{NA}.
#' @return A data frame with the the equivalent diameter calculated.
#' 
#' @references 
#' Soares, C. P. B., Paula Neto, F. and Souza, A. L. (2012) Dendrometria e Inventario Florestal. 2nd ed. Vicosa: UFV.
#'
#' @export
#' 
#' @examples
#' 
#' library(forestmangr)
#' data("exfm18")
#' exfm18
#' 
#' # Calculate the equivalent diameter of trees with more than one trunk:
#' eq_diam <- tree_summarise(exfm18, "DBH",tree="Tree", .groups=c("Plot", "Species") )
#' head(eq_diam, 10)
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' 
tree_summarise <- function(df,  dbh, tree, .groups=NA, vwb=NA, vwob=NA){
  # Checagem de variaveis ####
  nn <- NULL
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
  if(missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||any(.groups=="") ){
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
  
  # se vwob nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
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
  
  dbh_name  <- dbh
  tree_name <- tree
  vwb_name  <- vwb
  vwob_name <- vwob
  
  dbh_sym  <- rlang::sym(dbh)
  vwb_sym  <- rlang::sym(vwb)
  vwob_sym <- rlang::sym(vwob)
  
  # ####
  
  x <- df %>% 
    dplyr::group_by(!!!.groups_syms, !!!tree_syms, add=T) %>% 
    dplyr::summarise(
      !!dbh_name := sqrt( sum( (!!dbh_sym)^2, na.rm=T) ),
      !!vwb_name := sum( (!!vwb_sym), na.rm=T),
      !!vwob_name := sum( (!!vwob_sym), na.rm=T) ) %>% 
    dplyr::na_if(0) %>% 
    as.data.frame() %>% 
    dplyr::select_if(~!all(is.na(.))) %>% # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
    dplyr::ungroup()
  
  # Remove data from other boles, keep only the first one
  y <- df %>% 
    dplyr::group_by(!!!.groups_syms, !!!tree_syms, add=T) %>% 
    dplyr::mutate(nn=seq(1,dplyr::n())) %>% 
    dplyr::filter(nn==1) %>% 
    dplyr::select(-nn, -(!!dbh_sym), -(!!vwb_sym), -(!!vwob_sym) ) %>% 
    dplyr::ungroup()
  
  # Bind new dbh with original data and return
  if(missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||any(.groups=="") ){
    return(as.data.frame(dplyr::left_join(y,x, by=tree)) ) 
  }else{
    return(as.data.frame(dplyr::left_join(y,x, by=c(.groups,tree) )) )
  }
  
}
