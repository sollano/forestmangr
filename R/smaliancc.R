#' @title Volume com casca pelo metodo de Smalian
#' 
#' @description Calculo do volume com casca em nivel de secao utilizando o metodo de Smalian.
#' 
#' @details 
#' Funcao utilizada para calcular o volume com casca pelo metodo de Smalian. A funcao possui
#' integracao com dplyr, podendo ser utilizada dentro de um pipe, em conjunto com a funcao
#' group_by.
#' 
#' @param df Data frame a ser utilizado.
#' @param di Nome entre aspas da variavel diametro da secao em centimetros.
#' @param hi Nome entre aspas da variavel altura da secao em metros.
#' @param .groups Nome(s) entre aspas da(s) variavel(s) classificatoria(s) utilizadas para identificar as arvores. Caso este argumento seja \code{NULL}, serao utilizados grupos ja definidos no dataframe. Padrao: \code{NULL}.
#' @return Dataframe contendo os valores em nivel de secao.
#' 
#' @references 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuracao florestal: perguntas e respostas. 3a. ed. Vicosa: Editora UFV, 2013. 605 p.
#'
#' @seealso Funcao complementar:
#'   \code{\link{smaliansc}}, para o calculo do volume sem casca.
#'   
#' @export
#' @examples
#' library(forestr)
#' data("ex7_mfr")
#' 
#' # Calcular o volume com casca pelo metodo de Smalian:
#' smaliancc(ex7_mfr,"di_cc", "hi", "ARVORE")
#' 
#' # ou, utilizando pipes:
#'  ex7_mfr %>% 
#'  group_by(ARVORE) %>% 
#'  smaliancc("di_cc", "hi")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

smaliancc <- function(df, di, hi, .groups, di_mm_to_cm=FALSE, hi_cm_to_m=FALSE ){
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
  
  # se di nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(di) ){  
    stop("di not set", call. = F) 
  }else if( !is.character(di) ){
    stop("'di' must be a character containing a variable name", call.=F)
  }else if(length(di)!=1){
    stop("Length of 'di' must be 1", call.=F)
  }else if(forestr::check_names(df, di)==F){
    stop(forestr::check_names(df, di, boolean=F), call.=F)
  }
  
  # se hi nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(hi) ){  
    stop("hi not set", call. = F) 
  }else if( !is.character(hi) ){
    stop("'hi' must be a character containing a variable name", call.=F)
  }else if(length(hi)!=1){
    stop("Length of 'hi' must be 1", call.=F)
  }else if(forestr::check_names(df, hi)==F){
    stop(forestr::check_names(df, hi, boolean=F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups) && is.null(dplyr::groups(df)) ){
    stop(".groups must be set if data doesn't have any groups", call. = F)
  }else if(missing(.groups) && !is.null(dplyr::groups(df)) ){
    .groups_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F) 
  }else if(forestr::check_names(df,.groups)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestr::check_names(df,.groups, boolean=F), call.=F) 
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
  
  # Converter altura da secao de centimetro para metro
  if(hi_cm_to_m){
    df[[hi]] <- df[[hi]]/100
  }
  
  di_sym <- rlang::sym(di)
  hi_sym <- rlang::sym(hi)

  # ####
  
   df %>% 
     dplyr::group_by( !!!.groups_syms,add=T ) %>% 
     dplyr::mutate( 
        AS_CC = ( ( (!!di_sym) ^2* pi) / 40000) , 
        VCC   =  ((AS_CC + dplyr::lead(AS_CC) )/2 ) * (dplyr::lead(!!hi_sym) - (!!hi_sym) ) ) %>% 
     dplyr::ungroup()
}
