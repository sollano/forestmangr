#' @title 
#' Calculate the volume without bark of trees using the Smalian method
#' @description
#' Function used to calculate the volume without bark of trees using the Smalian method.
#' This function has integration with dplyr, so it can be used inside a pipe, along with the
#' \code{group_by} function.
#' 
#' @param df A dataframe.
#' @param di Quoted name of the section diameter  variable, in centimeters.
#' @param hi Quoted name of the section height  variable, in meters
#' @param bt Quoted name of the bark thickness variable, in centimeters.
#' @param tree Quoted name of the tree variable. used to differentiate the trees' sections. If this argument is \code{NULL}, the defined groups in the dataframe will be used. Default: \code{NULL}.
#' @param .groups Optional argument. Quoted name(s) of additional grouping variables that can be added to differenciate subdivisions of the data. 
#' If this argument is \code{NULL}, the defined groups in the dataframe will be used. Default: \code{NULL}.
#' @param di_mm_to_cm Boolean argument that, if \code{TRUE}, converts the di argument from milimiters to centimeters. Default: \code{FALSE}.
#' @param hi_cm_to_m Boolean argument that, if \code{TRUE}, converts the hi argument from centimeters to meters. Default: \code{FALSE}.
#' @param nt_mm_to_cm Boolean argument that, if \code{TRUE}, converts the bt argument from milimiters to centimeters. Default: \code{FALSE}.
#' @return Dataframe with volume values by section.
#' 
#' @references 
#' Campos, J. C. C. and Leite, H. G. (2017) Mensuração Florestal: Perguntas e Respostas. 5a. Viçosa: UFV.
#'
#' @seealso Complementary functions:
#'   \code{\link{smalianwb}}, For calculation of volume with bark using Smalian's method,
#'   \code{\link{huberwb}}, for calculation of volume with bark using Huber's method,
#'   \code{\link{huberwob}}, for calculation of volume without bark Huber's method.
#'   
#' @export
#' @examples
#' library(forestmangr)
#' data("ex7_mfr")
#' 
#' head(ex7_mfr)
#' 
#' # Calculate the volume without bark using Smalian's method:
#' smalianwob(ex7_mfr,"di_wb", "hi", "bark_t", "TREE")
#' 
#' # Using pipes:
#' library(dplyr)
#' 
#'  ex7_mfr %>% 
#'  group_by(TREE) %>% 
#'  smalianwob("di_wb", "hi", "bark_t")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' 
smalianwob <- function(df, di, hi, bt, tree, .groups=NULL, di_mm_to_cm=FALSE, hi_cm_to_m=FALSE, bt_mm_to_cm=FALSE ){
  # Checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
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
  }else if(forestmangr::check_names(df, di)==F){
    stop(forestmangr::check_names(df, di, boolean=F), call.=F)
  }
  
  # se hi nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(hi) ){  
    stop("hi not set", call. = F) 
  }else if( !is.character(hi) ){
    stop("'hi' must be a character containing a variable name", call.=F)
  }else if(length(hi)!=1){
    stop("Length of 'hi' must be 1", call.=F)
  }else if(forestmangr::check_names(df, hi)==F){
    stop(forestmangr::check_names(df, hi, boolean=F), call.=F)
  }
  
  # se bt nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(bt) ){  
    stop("bt not set", call. = F) 
  }else if( !is.character(bt) ){
    stop("'bt' must be a character containing a variable name", call.=F)
  }else if(length(bt)!=1){
    stop("Length of 'bt' must be 1", call.=F)
  }else if(forestmangr::check_names(df, bt)==F){
    stop(forestmangr::check_names(df, bt, boolean=F), call.=F)
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
  # se di_mm_to_cm nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! di_mm_to_cm %in% c(TRUE, FALSE) ){ 
    stop("'di_mm_to_cm' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(di_mm_to_cm)!=1){
    stop("Length of 'di_mm_to_cm' must be 1", call.=F) 
  }
  
  # se hi_cm_to_m nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! hi_cm_to_m %in% c(TRUE, FALSE) ){  
    stop("'hi_cm_to_m' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(hi_cm_to_m)!=1){
    stop("Length of 'hi_cm_to_m' must be 1", call.=F)
  }
   
  # Converter diametro da secao de milimetro para centimetro
  if(di_mm_to_cm){
    
    df[[di]] <- df[[di]]/10
    
  }
  
  # Converter altura da secao de centrometro para metro
  if(hi_cm_to_m){
    df[[hi]] <- df[[hi]]/100
  }

  # Converter espessura da casca de milimetro para centimetro
  if(bt_mm_to_cm){
    df[[bt]] <- df[[bt]]/10
  }
  
  di_sym <- rlang::sym(di)
  hi_sym <- rlang::sym(hi)
  bt_sym <- rlang::sym(bt)
  
  # ####
  
  df %>% 
    dplyr::group_by(!!!.groups_syms, !!!tree_syms, add=T) %>% 
    dplyr::mutate(
        di_sc = (!!di_sym)-2*(!!bt_sym),
        CSA_WOB = (di_sc^2* pi) / 40000 , 
        VWOB   = ((CSA_WOB + dplyr::lead(CSA_WOB) )/2 ) * (dplyr::lead(!!hi_sym) - (!!hi_sym) ) )  %>% 
    dplyr::ungroup()
}
