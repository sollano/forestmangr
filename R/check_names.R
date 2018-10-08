#' @export

check_names <- function(df, var_names, boolean=T){
  
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
