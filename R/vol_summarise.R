#' @title 
#' Summarise volume of trees
#' @description 
#' This function can be used to calculate volume with and without bark
#' of trees in a dataframe.
#'
#' @param df A dataframe.
#' @param dbh Quoted name of the diameter at breast hight variable, im cm.
#' @param th Quoted name of the total height variable, in meters.
#' @param vwb Quoted name of the volume with bark varible, in cubic meters.
#' @param tree Quoted name of the tree variable. used to differentiate the trees' sections. If this argument is \code{NULL}, the defined groups in the dataframe will be used. Default: \code{NULL}.
#' @param .groups Optional argument. Quoted name(s) of additional grouping variables that can be added to differenciate subdivisions of the data. 
#' @param vwob Optional argument. Quoted name of the volume without bark variable, in cubic meters. Default: \code{NULL}.
#' If this argument is \code{NULL}, the defined groups in the dataframe will be used. Default: \code{NULL}.
#' @return A dataframe summarised by the .groups variable(s).
#' 
#' @seealso Complementary functions:
#'   \code{\link{smalianwb}}, For calculation of volume with bark using Smalian's method,
#'   \code{\link{smalianwob}}, For calculation of volume without bark using Smalian's method,
#'   \code{\link{huberwb}}, for calculation of volume with bark using Huber's method,
#'   \code{\link{huberwob}}, for calculation of volume without bark Huber's method.
#'
#' @export
#' @examples
#' library(forestmangr)
#' data("ex7_mfr")
#' head(ex7_mfr)
#' 
#' # In order to calculate the volume of each tree, first we
#' # Calculate the volume by tree section using Smalian's method:
#' sec_data_vol <- ex7_mfr %>% 
#' smalianwb("di_wb", "hi", "TREE") %>% 
#' smalianwob("di_wb", "hi", "bark_t", "TREE", bt_mm_to_cm = T)
#' 
#' sec_data_vol
#' 
#' # Now, we summarise the tree's volume:
#' vol_summarise(sec_data_vol, dbh = "DBH", th = "TH", vwb = "VWB",tree = "TREE", .groups = "STRATA",vwob = "VWOB")
#' 
#' # It's possible to do everything using pipes:
#' ex7_mfr %>% 
#' smalianwb("di_wb", "hi", "TREE") %>% 
#' smalianwob("di_wb", "hi", "bark_t", "TREE", bt_mm_to_cm = T) %>% 
#' vol_summarise("DBH", "TH", "VWB", "TREE", "STRATA", "VWOB")
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' 
vol_summarise <- function(df, dbh, th, vwb, tree, .groups=NULL, vwob=NULL){
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
  if(  missing(dbh) ){  
    stop("dbh not set", call. = F) 
  }else if( !is.character(dbh) ){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dbh)==F){
    stop(forestmangr::check_names(df, dbh, boolean=F), call.=F)
  }
  
  # se vwb nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(vwb) ){  
    stop("vwb not set", call. = F) 
  }else if( !is.character(vwb) ){
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
  
  dbh_name <- dbh
  th_name <- th
  vwb_name <- vwb
  vwob_name <- vwob
  
  # funcao para transformar strings em symmbolos que o dplyr entende
  dbh_sym <- rlang::sym(dbh) 
  th_sym  <- rlang::sym(th)
  vwb_sym <- rlang::sym(vwb)
  vwob_sym <- rlang::sym(vwob)
  
  # ####
  
  # !! diz para o dplyr que voce esta lidando com simbolos ou strings
  
  # := e utilizado quando o nome da variavel nova dentro do pipe esta dentro de um objeto
  
   out <- df %>%                                     # define data frame utilizado
    dplyr::na_if(0) %>%                              # Transforma zeros em NA
    dplyr::group_by( !!!.groups_syms, !!!tree_syms ) %>% # definicao da chave
    dplyr::summarize(                                # Funcao que compila os df
      !!dbh_name := mean(!!dbh_sym, na.rm = TRUE), # Media de dbh
      !!th_name  := mean(!!th_sym,  na.rm = TRUE), # media de th
      CSA          = pi * (!!rlang::sym(dbh_name))^2 / 40000       , # Area Seccional
      !!vwb_name := sum(!!vwb_sym,  na.rm = TRUE), # Soma de volume com casca
      !!vwob_name := sum(!!vwob_sym,  na.rm = TRUE), # Soma de volume sem casca
      BARK_PERC  = (( (!!rlang::sym(vwb_name)) - (!!rlang::sym(vwob_name)) )/ (!!rlang::sym(vwb_name))  )*100    , # Porcentagem da casca
     #VCIL     = CSA *  (!!rlang::sym(th_name)) ,
      FFWB        = (!!rlang::sym(vwb_name)) / (CSA * (!!rlang::sym(th_name)) )   , # Fator de forma com casca
      FFWOB        = (!!rlang::sym(vwob_name)) / (CSA * (!!rlang::sym(th_name)) )   ) %>%     # Fator de forma sem casca
    dplyr::mutate_at(                                # Funcao que cria novas variaveis utilizando as variaveis
      dplyr::vars(FFWB, FFWOB),                   # especificadas por vars
      dplyr::funs(medio = mean)    ) %>%             # Fator de forma medio
    dplyr::na_if(0) %>%                              # Se vwob nao for informado, variaveis que o utilizam serao 0, portanto, deve-se converte-las para NA, para depois remove-las
    dplyr::select_if(Negate(anyNA))                  # remove variaveis que nao foram informadas (argumentos opicionais nao inseridos viram NA)
  
  if(suppressWarnings(all(is.na(df$vwob)))) out$BARK_PERC <- NULL
  
  out
  
}
