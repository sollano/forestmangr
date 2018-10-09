#' @title 
#' Calculate the volume with bark of trees using the Huber method
#' @description
#' Function used to calculate the volume with bark of trees using the Huber method.
#' This function has integration with dplyr, so it can be used inside a pipe, along with the
#' \code{group_by} function.
#' 
#' @param df A dataframe.
#' @param di Quoted name of the section diameter  variable, in centimeters.
#' @param section_length Quoted name of the section length variable, in meters
#' @param tree Quoted name of the tree variable. used to differentiate the trees' sections. If this argument is \code{NULL}, the defined groups in the dataframe will be used. Default: \code{NULL}.
#' @param .groups Optional argument. Quoted name(s) of additional grouping variables that can be added to differenciate subdivisions of the data. 
#' If this argument is \code{NULL}, the defined groups in the dataframe will be used. Default: \code{NULL}.
#' @param di_mm_to_cm Boolean argument that, if \code{TRUE}, converts the di argument from milimiters to centimeters. Default: \code{FALSE}.
#' @return Dataframe with volume values by section.
#' 
#' @references 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuracao florestal: perguntas e respostas. 3a. ed. Vicosa: Editora UFV, 2013. 605 p.
#'
#' @seealso Complementary functions:
#'   \code{\link{huberwob}}, For calculation of volume without bark using Huber's method,
#'   \code{\link{smalianwb}}, for calculation of volume with bark using Smalian's method,
#'   \code{\link{smalianwob}}, for calculation of volume without bark Smalian's method.
#'   
#' @export
#' @examples
#' library(forestmangr)
#' data("ex8_mfr")
#' 
#' head(ex8_mfr)
#' 
#' # Calculate the volume with bark using Huber's method:
#' huberwb(ex8_mfr,"di_wb", "sec_length", "TREE")
#' 
#' # Using pipes:
#' library(dplyr)
#' 
#'  ex8_mfr %>% 
#'  group_by(TREE) %>% 
#'  huberwb("di_wb", "sec_length")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

huberwb <- function(df, di, section_length, tree, .groups=NULL, di_mm_to_cm=FALSE){
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
  
  # se section_length nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(section_length) ){  
    stop("section_length not set", call. = F) 
  }else if( !is.character(section_length) ){
    stop("'section_length' must be a character containing a variable name", call.=F)
  }else if(length(section_length)!=1){
    stop("Length of 'section_length' must be 1", call.=F)
  }else if(forestmangr::check_names(df, section_length)==F){
    stop(forestmangr::check_names(df, section_length, boolean=F), call.=F)
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
  
  # Converter diametro da secao de milimetro para centimetro
  if(di_mm_to_cm){
    df[[di]] <- df[[di]]/10
  }
  
  di_sym <- rlang::sym(di)
  section_length_sym <- rlang::sym(section_length)
  # ####
  
  df %>% 
    dplyr::group_by(!!!.groups_syms, !!!tree_syms, add=T) %>% 
    dplyr::mutate(
       CSA_WB = ( ( (!!di_sym)^2* pi) / 40000) , 
       VWB = CSA_WB * (!!section_length_sym)  ) %>% 
    dplyr::ungroup()
}
