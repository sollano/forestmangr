#' @export

fit_mod <- function(df, model, name = "EST" , Y = "HT",  lnmodel=T, EANM = T){
  
  MODEL <- stats::as.formula(model) # transformar em formula
  
  VARSX <- all.vars(MODEL[[3]]) # todas as variaveis do lado x do modelo
  
  COEF_N <- length( VARSX )  # numero de coeficientes alem do beta0
  
  # criamos um data frame vazio, com o numero de linhas do df original,
  # e o numero de colunas de coef.
  data <- data.frame(matrix(data=NA, nrow = nrow(df), ncol = COEF_N ))

  # isso e necessario pois iremos preencher este dataframe com
  # com a multiplicacao de cada beta e sua respectiva variavel
    
  for(j in 1:COEF_N  ){
    
      # multiplica o b1 pela primeira variavel, o b2 pela segunda, e assim sucessivamente
      data[ ,j] <- df[ , paste("b", j, sep="") ] * df[ , VARSX[j] ]
    
  }
  
  # adicionar b0, pois nao foi considerado no for acima
  data$b0 <- df$b0 
  
  # para finalizar o modelo, temos que somar o resultado do for,
  # com o beta zero
  
  # aplica a funcao sum para todas as linhas e converte em vetor
  EST <- do.call(rbind, as.list( rowSums(data) )  )
  
  # se o y do modelo for logaritimico, tiramos o ln com exp
    if(lnmodel==T){EST <- exp(EST) } 
  
  # estimar apenas para arvores nao medidas, se o usuario quiser
    if(EANM==T){ 
      df$EST <- ifelse( is.na(df[[Y]])  , EST, df[[Y]]  )  
    }else{ # caso contrario,estimar para todas as arvores
      df$EST <- EST }

  # renomeia a variavel estimada para o nome que o usuario escolher
  names(df)[names(df) == "EST"] <- name
  
  # remove as variaveis usadas na estimacao (as variaveis do modelo, os betas,
  # R2 e erro)
  df[ c(all.vars(MODEL) ,  
        paste("b", 0:length(VARSX), sep="" ),  
        "Rsqr",
        "Rsqr_adj",
        "Std.Error") ] <- NULL
  
  return(df)
  
  
}

