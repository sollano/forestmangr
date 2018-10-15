#' @title 
#' Check if character vector contains variable names
#' @description 
#' Function used to check if a string, or a character vector contains variable names of a given data frame.
#' @details
#' Function used to check if a string, or a character vector contains variable names of a given data frame. 
#' This functions is mainly used to error-proof other functions of this package, 
#'
#' @param df a data frame.
#' @param var_names Character vector to be compared with the data frame names.
#' @param boolean Boolean object used to define if the output is going to be a boolean object \code{TRUE} , or a string \code{FALSE}. Default: \code{TRUE}.
#'
#' @export
#' @examples 
#' library(forestmangr)
#'
#'check_names(iris, "Species")
#'check_names(iris, "Species", boolean = FALSE )
#'
#'check_names(iris, c("Especies", "Setal.Width") )
#'check_names(iris, c("Especies", "Setal.Width"), boolean = FALSE)
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

check_names <- function(df, var_names, boolean=TRUE){
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }
  
  # Parar se var_names nao for character
  if(!is.character(var_names)){stop("var_names must be a character", call.=F)}
  
  # Retirar espaco vazio dos nomes
  var_names <- var_names[var_names!=""]
  
  # Verificar se cada elemento de var_names(primeiro sapply) 
  # eh um nome de df(funcao q usa o segundo sapply)
  # se for ele ira receber um unico valor de TRUE, por causa da funcao any
  y <- sapply(var_names,function(x){ any(sapply(names(df), `==`,x))} )
  
  if(any(!y) & boolean==F){
    
    return(paste("Variable '", names(y[y==F]),"' does not exist in df    ",sep="" ))
    
  }else if(any(!y) & boolean==T){
    
    return(FALSE)
    
  } else if(all(y) & boolean==F){
    
    paste("All variables exist in df")
    
  }else if(all(y) & boolean==T){
    
    return(TRUE)
  }
  
}

