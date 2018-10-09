#' Teste F de Graybill
#' @description 
#' Teste de hipotese conforme descrito por GRAYBILL (1976).
#' @details 
#' Teste utilizado para comparar duas variaveis, geralmente um metodo proposto e um padrao.
#' Este teste é muito utilizado, principalmente na área florestal, sendo muitas vezes mais indicado do que os testes de média. Isto porque o teste <i>F</i> de Graybill considera todos os dados, e não só a média, que pode não ser representativa dos dados, principalmente com a presença de outliers. 
#'
#' A validacao da predicao constitui-se em ajustar um modelo linear de 1º
#' grau dos valores preditos em funcao dos valores observados.
#' 
#' A significancia da regressao e avaliada aplicando-se o teste F para as
#'estimativas dos parametros, conforme metodologia descrita por GRAYBILL (1976).
#' 
#' @param df Data frame a ser utilizado.
#' @param Y1 Nome daVariavel padrao entre aspas.
#' @param Yj Nome Variavel proposta entre aspas.
#' @param output Tipo de saida simples (1), com algumas informacoes sobre as variaveis (2) ou completa (3). Padrao: \code{1}.
#' @param signif Valor da significancia a ser utilizada do teste. Padrao: \code{0.05}.
#' @return Dataframe; suas dimensoes variam de acordo com o parametro \code{output}.
#' 
#' @references 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuração florestal: perguntas e respostas. 3ª. ed. Viçosa: Editora UFV, 2013. 605 p.
#'
#'GRAYBILL, F. A. Theory and application of the linear model. Massa chusetts: Ouxburg Press, p. 704, 1976.
#'
#' LEITE, H. G.; OLIVEIRA, F. H. T. Statistical procedure to test identity between analytical methods. Communications in Soil Science Plant Analysis, v.33, n.7/8, p.1105-1118, 2002. 
#' 
#' @export
#' @examples
#' library(forestr)
#' data("ex11_mfr")
#' 
#' # O data frame ex11_mfr possui estimativas de altura para uma determinada floresta.
#' # Pode-se determinar se o dado estimado e estatisticamente igual ao observado utilizando 
#' # o teste F de Graybill:
#' 
#' FdeGraybill( ex11_mfr,"HT",  "HT_EST1")
#' 
#' # Pode-se alterar a significancia do teste utilizando o argumento signif:
#' FdeGraybill( ex11_mfr,"HT",  "HT_EST1" , signif = 0.01)
#' 
#' # Para saidas mais detalhadas do teste, pode-se utilizar o argumento output:
#' FdeGraybill( ex11_mfr,"HT",  "HT_EST2" , output=2)
#' FdeGraybill( ex11_mfr,"HT",  "HT_EST2" , output=3)
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

FdeGraybill <- function(df, Y1, Yj, output = 1, signif = 0.05) {
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se Y1 nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(Y1) ){  
    stop("Y1 not set", call. = F) 
  }else if( !is.character(Y1) ){
    stop("'Y1' must be a character containing a variable name", call.=F)
  }else if(length(Y1)!=1){
    stop("Length of 'Y1' must be 1", call.=F)
  }else if(forestr::check_names(df, Y1)==F){
    stop(forestr::check_names(df, Y1, boolean=F), call.=F)
  }
  
  # se Yj nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(Yj) ){  
    stop("Yj not set", call. = F) 
  }else if( !is.character(Yj) ){
    stop("'Yj' must be a character containing a variable name", call.=F)
  }else if(length(Yj)!=1){
    stop("Length of 'Yj' must be 1", call.=F)
  }else if(forestr::check_names(df, Yj)==F){
    stop(forestr::check_names(df, Yj, boolean=F), call.=F)
  }
    
  if( !output %in% c(1,2,3) ){stop("ouput argument must be 1, 2 or 3")}
  
  # Se signif nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( signif )){
    stop( "'signif' must be numeric", call.=F)
  }else if(length(signif)!=1){
    stop("length of 'signif' must be 1", call.=F)
  }else if(! signif > 0 | ! signif <= 0.30){
    stop("'signif' must be a number between 0 and 0.30", call.=F)
  }
  
  Y1 <- df[[Y1]]
  Yj <- df[[Yj]]
  
  fit <- stats::lm(Yj ~ Y1)
  QMRes <- sum(stats::residuals(fit)^2)/fit$df.residual
  beta_theta <- stats::coef(fit) - c(0,1)
  Y1linha_Y1 <- cbind(c(length(Y1), sum(Y1)), c(sum(Y1), sum(Y1^2)))
  FH0 <- round((t(beta_theta)%*%Y1linha_Y1%*%beta_theta)/(2*QMRes),4)
  
  Ftab <- round(stats::qf(p=signif, df1=2, df2=fit$df.residual, lower.tail = FALSE),4)
  pvalor <- signif(stats::pf(FH0,2,fit$df.residual,lower=F),4)
  
  if(FH0 > Ftab){Resultado <- "*"}else(Resultado <- "ns")  
  if(FH0 > Ftab){Conclusao <- "Yj e estatisticamente diferente de Y1, para o n. de significancia estabelecido"}else{Conclusao <- "Yj e estatisticamente igual a Y1, para o n. de significancia estabelecido"}

   
  if(output==1)
  {
    Tab_Res_Simp <-  data.frame("F_H0"    = FH0, 
                                "F_crit"  = Ftab, 
                                "p_valor" = pvalor, 
                                "Signif"  = signif,
                                "Teste"   = Resultado, 
                                stringsAsFactors = F) 
    
    return(Tab_Res_Simp)
  } 
  else if(output==2)
  {
    Tab_Res_Med <- data.frame(Resultado = rbind(mean(Y1), mean(Yj), stats::var(Y1), stats::var(Yj), stats::sd(Y1), stats::sd(Yj), length(Y1), 2, fit$df.residual, Ftab, FH0, signif, round(pvalor , 9)),
                              row.names = c("Media_Y1", "Media_Yj", "Variancia_Y1", "Variancia_Yj", "Desvio_Padrao_Y1", "Desvio_Padrao_Yj", "Observacoes", "g.l.1", "g.l.2", "F_crit", "F_H0", "Nivel_de_Significancia",  "p valor"), 
                              stringsAsFactors = F )
    
    return(Tab_Res_Med)
  }
  else if(output==3){
    aux1 <- c(round(mean(Y1),2), round(stats::var(Y1),2), round(stats::sd(Y1),2),  length(Y1), 2, Ftab, FH0, signif, pvalor, Resultado, Conclusao)
    aux2 <- c(round(mean(Yj),2), round(stats::var(Yj),2), round(stats::sd(Yj),2), length(Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
    
    Tab_Res_Comp <- data.frame("Valor_Padrao" = aux1,
                               "Valor_Proposto" = aux2, 
                               row.names = c("Media", "Variancia", "Desvio_Padrao", "Observacoes", "g.l.", "F_Critico", "F_H0", "Nivel_de_Significancia", "P-valor" ,"Teste", "Conclusao"), 
                               stringsAsFactors = F )
    
    
    return(Tab_Res_Comp)
    }
  }
