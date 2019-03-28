#' @title 
#' Graybill F Test
#' @description 
#' Hypothesis test as described by Graybill (1976).
#' @details 
#' This test is used to compare two variables, usually a proposed method,
#' and a standard variable.This test is popular among forestry engineers, 
#' specially because, since it considers all data in it's analysis, 
#' it's usually more precise than a standard mean t-test.
#' If the data has outliers, the mean may not represent the data correctly, so
#' Graybill F test is specially useful for heterogeneous data.
#'
#' A simple model regression is applied, and it's significance is evaluated
#' by applying Graybill F test for the parameters estimate,
#' according to the methodology described by Graybill (1976).
#' 
#' @param df A data frame.
#' @param Y1 Quoted name of the standard variable.
#' @param Yj Quoted name of the proposed variable.
#' @param signif Numeric value for the significance level used in the test. Default: \code{0.05}.
#' @param output Defines the type of output. If \code{"simple"}, a simple data frame is created, with only essential information about the test. If \code{"table"}, more information is provided, and if \code{"full"}, a data frame with informations about the test and both variables is created. Default: \code{"simple"}.
#' @return A data frame. Its dimensions will vary, according to the \code{output} argument.
#' 
#' @references 
#' Campos, J. C. C. and Leite, H. G. (2017) Mensuracao Florestal: Perguntas e Respostas. 5a. Vicosa: UFV.
#'
#' Graybill, F. A. (1976) Theory and application of the linear model. Massachusets: Ouxburg 239 Press.
#' 
#' Leite, H. G. and Oliveira, F. H. T. (2006) Statistical procedure to test identity between analytical methods, Communications in Soil Science and Plant Analysis, 33(7–8), pp. 1105–1118.
#'  
#' @export
#' @examples
#' library(forestmangr)
#' data("exfm11")
#' exfm11
#' 
#' # The data frame exfm11 contains a height variable called "TH". This will be our
#' # standard value. We'll compare it to height estimated using different hypsometric equations.
#' # These are variables "TH_EST1" and "TH_EST2":
#' graybill_f( exfm11,"TH", "TH_EST1")
#' 
#' # TH_EST1 is statistically different from "TH".
#' 
#' # It's possible to alter the test's significance level using the signif argument:
#' graybill_f( exfm11,"TH", "TH_EST1", signif = 0.01)
#' 
#' # Different output options are available through the output argument:
#' graybill_f( exfm11,"TH", "TH_EST2", output="table")
#' graybill_f( exfm11,"TH", "TH_EST2", output="full")
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

graybill_f <- function(df, Y1, Yj, signif = 0.05, output = "simple") {
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  
  if(missing(df) & !missing(Y1) & !missing(Yj) ){
    
    df <- data.frame("Y1"=Y1,"Yj"=Yj)
    Y1 <- "Y1"
    Yj <- "Yj"
    
  }else if(  missing(df) ){  
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
  }else if(forestmangr::check_names(df, Y1)==F){
    stop(forestmangr::check_names(df, Y1, boolean=F), call.=F)
  }
  
  # se Yj nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(Yj) ){  
    stop("Yj not set", call. = F) 
  }else if( !is.character(Yj) ){
    stop("'Yj' must be a character containing a variable name", call.=F)
  }else if(length(Yj)!=1){
    stop("Length of 'Yj' must be 1", call.=F)
  }else if(forestmangr::check_names(df, Yj)==F){
    stop(forestmangr::check_names(df, Yj, boolean=F), call.=F)
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('simple', 'table', 'full') ){ 
  stop("'output' must be equal to 'simple', 'table' or 'full' ", call. = F) 
  }

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
  
  if(FH0 > Ftab){Results <- "*"}else(Results <- "ns")  
  if(FH0 > Ftab){Conclusion <- "Yj is statistically different from Y1, at the stablished significance level"}else{Conclusion <- "Yj is statistically equal to Y1, at the stablished significance level"}

   
  if(output=="simple")
  {
    Tab_Res_Simp <-  data.frame("F_H0"    = FH0, 
                                "F_crit"  = Ftab, 
                                "p_value" = pvalor, 
                                "Signif"  = signif,
                                "Test"   = Results, 
                                stringsAsFactors = F) 
    
    return(Tab_Res_Simp)
  } 
  else if(output=="table")
  {
    Tab_Res_Med <- data.frame(Results = rbind(mean(Y1), mean(Yj), stats::var(Y1), stats::var(Yj), stats::sd(Y1), stats::sd(Yj), length(Y1), 2, fit$df.residual, Ftab, FH0, signif, round(pvalor , 9)),
                              row.names = c("Mean_Y1", "Mean_Yj", "Variance_Y1", "Variance_Yj", "Standard_deviation_Y1", "Standard_deviation_Yj", "N_Observations", "d.f.1", "d.f.2", "F_crit", "F_H0", "Significance_level",  "p-value"), 
                              stringsAsFactors = F )
    
    return(Tab_Res_Med)
  }
  else if(output=="full"){
    aux1 <- c(round(mean(Y1),2), round(stats::var(Y1),2), round(stats::sd(Y1),2),  length(Y1), 2, Ftab, FH0, signif, pvalor, Results, Conclusion)
    aux2 <- c(round(mean(Yj),2), round(stats::var(Yj),2), round(stats::sd(Yj),2), length(Yj), fit$df.residual, " ", " ", " ", " ", " ", " ")
    
    Tab_Res_Comp <- data.frame("Standard_Value" = aux1,
                               "Proposed_Value" = aux2, 
                               row.names = c("Mean", "Variance", "Standard_deviation", "N_Observations", "d.f.", "F_Critico", "F_H0", "Significance_level", "p-value" ,"Test", "Conclusion"), 
                               stringsAsFactors = F )
    
    
    return(Tab_Res_Comp)
    }
  }
