#' @title 
#' Fit linear regressions by group, with the option of removing outliers using a interactive plot of residuals.
#' @description 
#' With this function it's possible to fit linear regressions by a grouping variable, and evaluate each equation via
#' a interactive plot of residuals, and get a data frame.
#' with each column as a coefficient and quality of fit variables, and other output options. Works with dplyr grouping functions.
#' @details 
#' this function uses lm_table as a basis, but calls a plot of residuals for each fitted model, for the user to evaluate. If
#' one decides to remove any of the points, one can click and drag, and then click on the 'remove points' button. After that,
#' one must simply click 'done' and the coefficients will be printed out.
#' 
#' It's possible to use the \code{output} argument to get a merged table if \code{output="merge"}, that binds
#' the original data frame and the fitted coefficients. 
#' If \code{output="merge_est"} we get a merged table as well, but with y estimated using the coefficients. If the fit is made using groups, this is taken into account, i.e. the estimation is made by group.
#' 
#' If \code{output="nest"}, a data frame with nested columns is provided. This can be used if the user desires to get a customized output.
#'
#' @param df A data frame.
#' @param model A linear regression model, with or without quotes. The variables mentioned in the model must exist in the provided data frame. X and Y sides of the model must be separated by "~".
#' @param .groups Quoted name(s) of grouping variables used to fit multiple regressions, one for each level of the provided variable(s). Default \code{NA}.
#' @param output_mode  Selects different output options. Can be either \code{"table"}, \code{"merge"}, \code{"merge_est"} and \code{"nest"}. See details for explanations for each option. Default: \code{"table"}.
#' @param est.name Name of the estimated y value. Used only if \code{est.name = TRUE}. Default: \code{"est"}. 
#' @return  A data frame. Different data frame options are available using the output argument.
#' 
#' @export
#' @examples 
#' if (interactive() ){
#'   library(forestmangr)
#'   library(dplyr)
#' 
#'   data("exfm19")
#' 
#'   # Fit SH model by group:
#'   lm_resid_group(exfm19, log(VWB) ~  log(DBH) + log(TH), "STRATA")
#' 
#' }
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' @importFrom ggplot2 ggplot after_stat
#' 
lm_resid_group <- function(df,model,.groups,output_mode='table',est.name = 'est'){
  reg<-data<-NULL
  if(missing(.groups))
    stop('Please define grouping variable(s)',call. = FALSE)
  
  .groups_syms <- rlang::syms(.groups) 
  
  
  if(any(is.na(.groups))){
    stop('please inform a group',call. = FALSE)
  }else if(length(.groups)==1){
    group_var2 <- paste(.groups,"_",sep="")
    df[[group_var2]] <- df[[.groups]]
  }else{
    group_var2 <- c()
    for(i in 1:length(.groups)){
      
      group_var2[i] <- paste(.groups[i],"_",sep="")
      df[[group_var2[i]]] <- df[[.groups[i]]]
      
    }
  }
  # return(group_var2)
  #return(group_var2)
  lm_resid_g_ex = df %>% 
    # dplyr::mutate(!!grou := {{ group_var }}) %>% # vamos duplicar a coluna de grupo, 
    dplyr::group_by(!!!.groups_syms) %>% # pois precisamos delas dentro e fora dos grupos
    tidyr::nest() %>%  # vamos nestar o dado, para aplicar em cada parte isolada. a coluna e chamada de data
    dplyr::mutate(reg = purrr::map(data,
                                   ~lm_resid(df = ., # simplesmente aplicamos o modelo em data,
                                             model=model, # que no caso sao os dados nestados
                                             group_print=group_var2, # nome da coluna de grupo, convertida em char
                                             output_mode=output_mode,
                                             est.name = est.name))) %>% # saida selecionada
    dplyr::mutate(data = purrr::map(data,# aqui apagamos as colunas duplicadas
                                    ~dplyr::select(., -dplyr::any_of(group_var2) ))) %>% 
    tidyr::unnest(reg) %>% # desnestar a saida final
    dplyr::ungroup() # desagrupar os dados
  return(lm_resid_g_ex)
}


