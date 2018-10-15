#' @title
#' Divide data into 3 vertical strata
#' @description 
#' Get the vertical strata of data based on the height variable.
#' The data will be divided into inferior, medium and superior strata.
#' @param df A data frame.
#' @param th Quoted name of the total height variable.
#' @return a data frame.
#' 
#' @references 
#' Souza, A. L. and Soares, C. P. B. (2013) Florestas Nativas: estrutura, dinâmica e manejo. Viçosa: UFV.
#' 
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data(exfm10)
#' 
#' # To classify the data, supply the data frame and the height variable name:
#' vertical_stratum(exfm10, "TH" )
#' 
#' @author Eric Bastos Gorgens \email{e.gorgens@@gmail.com}
#'
vertical_stratum <- function(df, th ){
  # ####
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
  # ####
  th_sym <- rlang::sym(th)
  
  df %>%
    dplyr::mutate(vert.strat = dplyr::case_when(
      
      !!th_sym <= (mean(!!th_sym, na.rm=T) - stats::sd(!!th_sym, na.rm=T) )  ~ "Inferior",
      !!th_sym >= (mean(!!th_sym, na.rm=T) - stats::sd(!!th_sym, na.rm=T) ) & 
        !!th_sym < (mean(!!th_sym, na.rm=T) + stats::sd(!!th_sym, na.rm=T) ) ~ "Medium",
      !!th_sym >= (mean(!!th_sym, na.rm=T) + stats::sd(!!th_sym, na.rm=T) ) ~ "Superior"
      
    )) %>% 
    dplyr::select(vert.strat, dplyr::everything() )
  
}