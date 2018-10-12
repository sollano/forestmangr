#' @title 
#' Calculate the Dominant Height of forest inventory data plots
#' @description 
#' This function is used to get a dataframe with Dominant height values for each plot
#' in an forest inventory data.
#' 
#' @param df A dataframe.
#' @param th Quoted name of the total height variable.
#' @param dbh Quoted name of the diameter at breast height variable. Used to filter out trees with no diameter measerument.
#' @param plot Quoted name of the plot variable. used to differentiate the data's plots. If this argument is missing, the defined groups in the dataframe will be used, If there are no groups in the data, the function will fail.
#' @param obs Quoted name of the observations variable. This will be used to tell which trees are dominant, i.e. it's the variable that tells the type of tree; if it is normal, dominant, suppressed, etc. If this argument is not supplied, the function will calculate the average value of 2 trees with bigger height values in each plot, and use that as the dominant value.
#' @param dom Character value for the dominant tree code used in the observations variable variable supplied  in the \code{obs} argument. This is used alongside the \code{obs} argument to differentiate dominant trees from the others.
#' @param .groups Optional argument. Quoted name(s) of grouping variables that can be added to differenciate subdivisions of the data. Default: \code{NA}.
#' @param merge_data If \code{TRUE}, will merge the original dataframe with the dominant hight table. Default: \code{FALSE}.
#' @param dh_name Character value for the name of the dominant height variable created. Default: \code{"DH"}
#' @return A dataframe with the the dominant height values by plot.
#' 
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data(exfm9)
#' head(exfm9)
#'
#' # Let's say we need to get the dominant height (DH) values for a forest inventory data.
#' # If we don't have a variable that tells which trees are dominant, it's ok. We can
#' # still estimate DH using this function. It will average the top 2 trees of each plot:
#' dom_height(df=exfm9,th="TH",dbh="DBH",plot="PLOT")
#' 
#' # Of course, if we do have a variable that differentiate the dominant trees, it's
#' # best we use it. For that we use the obs argument, and the dom argument.
#' # In ths data, the OBS variable is used to tell the type of tree.
#' # Let's check the levels in our OBS variable, to see which one is associated 
#' # with dominant trees.
#' 
#' levels(as.factor(exfm9$OBS))
#' 
#' # So, the "D" level must be the one that tells which trees are dominant. Let's use it:#' 
#' dom_height(df=exfm9,th="TH",dbh="DBH",plot="PLOT",obs="OBS",dom="D")
#' 
#' # If there are subdivitions of the data, like different strata, they can be inlcuded in the
#' # .groups argument: 
#' dom_height(df=exfm9,th="TH",dbh="DBH",plot="PLOT",obs="OBS",dom="D",.groups="STRATA")
#' 
#' # It's possible to automatically bind the dominant heights table to the original data, 
#' # using the merge_data argument:
#' 
#' dom_height(df=exfm9,th="TH",dbh="DBH",plot="PLOT",obs="OBS",dom="D",.groups="STRATA", merge_data=T)
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' 
dom_height <- function(df, th, dbh, plot, obs, dom, .groups, merge_data=F,dh_name="DH"){
  # checagem de variaveis ####
  
  df[["DH"]] <- NULL
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se th nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(th) ){  
    stop("th not set", call. = F) 
  }else if( !is.character(th) ){
    stop("'th' must be a character containing a variable name", call.=F)
  }else if(length(th)!=1){
    stop("Length of 'th' must be 1", call.=F)
  }else if(forestmangr::check_names(df, th)==F){
    stop(forestmangr::check_names(df, th, boolean=F), call.=F)
  }
  
  # se dbh nao for fornecido, for igual "", nulo ou NA, nao fazer nada
  # se existir e nao for character,  parar
  if(missing(dbh) || is.null(dbh) || is.na(dbh) || dbh == "" ){
    
    
  }else if(!is.character(dbh)){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dbh)==F){
    stop(forestmangr::check_names(df, dbh, boolean=F), call.=F)
  }
  
  # se obs nao for fornecido, for igual "", nulo ou NA, fazer nada
  # se existir e nao for character,  parar
  if(missing(obs) || is.null(obs) || is.na(obs) || obs == "" ){
    
    
  }else if(!is.character(obs)){
    stop("'obs' must be a character containing a variable name", call.=F)
  }else if(length(obs)!=1){
    stop("Length of 'obs' must be 1", call.=F)
  }else if(forestmangr::check_names(df, obs)==F){
    stop(forestmangr::check_names(df, obs, boolean=F), call.=F)
  }
  
  
  # Se plot nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(plot) && is.null(dplyr::groups(df)) ){
    stop("plot not set. plot must be set if data doesn't have any groups", call. = F)
  }else if(missing(plot) && !is.null(dplyr::groups(df)) ){
    plot_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(plot)){
    stop("plot must be a character", call. = F)
  }else if(! length(plot)%in% 1:10){
    stop("Length of 'plot' must be between 1 and 10", call.=F) 
  }else if(forestmangr::check_names(df,plot)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,plot, boolean=F), call.=F) 
  }else{
    plot_syms <- rlang::syms(plot) 
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
  
  # se merge_data nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! merge_data %in% c(TRUE, FALSE) ){ 
    stop("'merge_data' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(merge_data)!=1){
    stop("Length of 'merge_data' must be 1", call.=F)
  }
  
  
  th_sym <- rlang::sym( th )
  
  # ####
  
  if(missing(obs) || is.null(obs) || is.na(obs) || obs=="" || missing(dbh) || is.null(dbh) || is.na(dbh) || dbh == "" ){
    
    suppressMessages(
    dhtable <- df %>% 
      dplyr::group_by(!!!.groups_syms, !!!plot_syms, add=T) %>% 
      dplyr::select(!!th_sym) %>% 
      dplyr::top_n(2) %>% 
      dplyr::summarise(DH = mean(!!th_sym) )
  )  
    
  } else{
    
    dbh_sym <- rlang::sym( dbh )
    obs_sym <- rlang::sym( obs )
    
    dhtable <- df %>%
      dplyr::group_by(!!!.groups_syms, !!!plot_syms, add=T) %>% 
      dplyr::filter( 
        !is.na(!!th_sym),
        !is.na(!!dbh_sym),
        (!!obs_sym) == dom ) %>%
      dplyr::summarise(DH = mean(!!th_sym, na.rm=T) ) %>%
      dplyr::ungroup()
    
  }
  
  if(merge_data){
    
    dhtable <- suppressMessages(dplyr::left_join(df, dhtable))
    
  }
  
  # Renomear dom
  names(dhtable)[names(dhtable)=="DH"] <- dh_name
  
  return(dhtable)
  
}
