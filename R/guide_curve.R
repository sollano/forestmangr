#' @title 
#' Get the guide curve plot for growth and yield analysis of inventory data
#' @description 
#' Get the guide curve for growth and yield analysis of inventory data
#' using the factor method, and different statistical models.
#' 
#' @param df A data frame.
#' @param dh Quoted name for the dominant height variable.
#' @param age Quoted name for the age variable.
#' @param age_index Numeric value for the age index.
#' @param n_class Numeric value for the number of classes used to divide the data. Default \code{4}.
#' @param model model used to fit dh as a function of age. The models available are \code{"Schumacher"}, \code{"Curtis"}, \code{"Chapman-Richards"} and \code{"Bailey-Clutter"}. Default: \code{"Schumacher"}.
#' @param start_chap Numeric vector with the start values for the Chapman-Richards model. This must be a named vector, with b0, b1 and b2 as parameter names. Default: \code{c(b0=23, b1=0.03, b2 = 1.3)}.
#' @param start_bailey Numeric vector with the start values for the Bailey-Clutter model. This must be a named vector, with b0, b1 and b2 as parameter names. Default: \code{c( b0=3, b1=-130, b2 = 1.5)}.
#' @param round_classes If \code{TRUE}, class values will be rounded to the nearest 5. Default \code{TRUE}.
#' @param font Type of font used in the plot. Default: \code{"serif"}.
#' @param gray_scale If \code{TRUE}, the plot will be rendered in a gray scale. Default: \code{"TRUE"}.
#' @param output Type of output the function should return. This can either be \code{"plot"}, for the guide curve plot, \code{"table"}, for a data frame with the data used on the guide curve plot, and \code{"full"} for a list with 2 ggplot2 objects, one for residual plot and other for plot curves, a lm object for the regression, a data frame with quality of fit variables, the dominant height index, the class table used, and the table used for the guide curve plot. Default \code{"plot"}.
#' @return A data frame, a ggplot object, or a list, varying according to the \code{"output"} argument.
#' 
#' @export
#' 
#' @examples 
#' data("exfm14")
#' head(exfm14)
#' 
#' # To get a guide curve plot for this data, we simply need to input
#' # dominant height and age variables, age index, and number of classes to be used:
#' guide_curve(exfm14, "dh", "age", 72, 5)
#' 
#' # if we want to get the table used to get the plot, we can choose the output "table":
#' guide_curve(exfm14, "dh", "age", 72, 5, output = "table")
#' 
#' # Other models are available for use, such as Curtis, Chapman Richards, and Bailey:
#' # CR and BC models are non linear, and thus need start values. There are default values,
#' # but they may fail, depending on the data used, so it's recommended to try start values that
#' # are ideal for the data used:
#' guide_curve(exfm14, "dh", "age", 72, 5,
#'  model = "Chapman-Richards", start_chap = c(b0=23, b1=0.03, b2 = 1.3))
#' 
#' # Or, to get more information on the analysis, such as details on the regression,
#' # bias, rmse, plot for residuals and more (cpu taxing):
#' \dontrun{
#' guide_curve(exfm14, "dh", "age", 72, 5, output = "full")
#' }
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' @import ggplot2
#' 
guide_curve <- function(df, dh, age, age_index, n_class=4, model = "Schumacher", start_chap = c(b0=23, b1=0.03, b2 = 1.3), start_bailey = c( b0=3, b1=-130, b2 = 1.5), round_classes = FALSE, font = "serif", gray_scale = TRUE, output = "plot"){
  # ####
  C<-classe<-nivel<-DH_EST<-DH_CURVE<-.data<-NULL
  # checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se dh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dh) ){  
    stop("dh not set", call. = F) 
  }else if( !is.character(dh) ){
    stop("'dh' must be a character containing a variable name", call.=F)
  }else if(length(dh)!=1){
    stop("Length of 'dh' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dh)==F){
    stop(forestmangr::check_names(df, dh, boolean=F), call.=F)
  }
  
  # se age nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(age) ){  
    stop("age not set. Please use number of months", call. = F) 
  }else if( !is.character(age) ){
    stop("'age' must be a character containing a variable name", call.=F)
  }else if(length(age)!=1){
    stop("Length of 'age' must be 1", call.=F)
  }else if(forestmangr::check_names(df, age)==F){
    stop(forestmangr::check_names(df, age, boolean=F), call.=F)
  }
  
  # Se age_index nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( age_index )){
    stop( "'age_index' must be numeric", call.=F)
  }else if(length(age_index)!=1){
    stop("Length of 'age_index' must be 1", call.=F)
  }else if(! age_index %in%  seq(from=1,to=200,by=0.1) ){
    stop("'age_index' must be a number between 1 and 200", call.=F)
  }
  
  # Se n_class nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( n_class )){
    stop( "'n_class' must be numeric", call.=F)
  }else if(length(n_class)!=1){
    stop("Length of 'n_class' must be 1", call.=F)
  }else if(! n_class %in%  seq(from=1,to=10,by=1) ){
    stop("'n_class' must be a number between 1 and 10", call.=F)
  }
  
  # Se model nao for character,ou nao for de tamanho 1, parar
  if(!is.character( model )){
    stop( "'model' must be character", call.=F)
  }else if(length(model)!=1){
    stop("Length of 'model' must be 1", call.=F)
  }else if(! model %in% c("Schumacher", "schumacher", "Curtis","curtis", "Chapman-Richards","chapman-richards", "Bailey-Clutter", "bailey-clutter") ){ 
  stop("'model' must be equal to 'Schumacher', 'Curtis', 'Chapman-Richards' or 'Bailey-Clutter' ", call. = F) 
  }
  
  # Se start_chap nao for numerico, nao for de tamanho 3, ou nao estiver dentro dos padroes, parar
  if(!is.numeric( start_chap )){
    stop( "'start_chap' must be numeric", call.=F)
  }else if(length(start_chap)!=3){
    stop("Length of 'start_chap' must be 3", call.=F)
  }else if(! is.vector(start_chap) ){
    stop("'start_chap' must be a named vector following the model 'c(b0=a,b1=b,b2=c) ", call.=F)
  }else if(all(names(start_chap) !=c("b0","b1","b2")) ){
    stop("'start_chap' must be a named vector following the pattern 'c(b0=a,b1=b,b2=c) ", call.=F)
  }
  
  # Se start_chap nao for numerico, nao for de tamanho 3, ou nao estiver dentro dos padroes, parar
  if(!is.numeric( start_bailey )){
    stop( "'start_bailey' must be numeric", call.=F)
  }else if(length(start_bailey)!=3){
    stop("Length of 'start_bailey' must be 3", call.=F)
  }else if(! is.vector(start_bailey) ){
    stop("'start_bailey' must be a named vector following the pattern 'c(b0=a,b1=b,b2=c) ", call.=F)
  }else if( all(names(start_bailey) != c("b0","b1","b2") ) ){
    stop("'start_bailey' must be a named vector following the pattern 'c(b0=a,b1=b,b2=c) ", call.=F)
  }
  
  # se round_classes nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! round_classes %in% c(TRUE, FALSE) ){ 
    stop("'round_classes' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(round_classes)!=1){
    stop("Length of 'round_classes' must be 1", call.=F)
  }

  # Se font nao for character,ou nao for de tamanho 1, parar
  if(!is.character( font )){
    stop( "'font' must be character", call.=F)
  }else if(length(font)!=1){
    stop("Length of 'font' must be 1", call.=F)
  }
  
  # se gray_scale nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! gray_scale %in% c(TRUE, FALSE) ){ 
    stop("'gray_scale' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(gray_scale)!=1){
    stop("Length of 'gray_scale' must be 1", call.=F)
  }
    
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c("plot", "table", "table_plot", "full") ){ 
  stop("'output' must be equal to 'plot', 'table', 'table_plot' or 'full' ", call. = F) 
  }
  
  DF <- as.data.frame(df)
  DH <- dh
  AGE <- age
  AGE_INDEX <- age_index
  NC <- n_class
  inv <- forestmangr::inv
  
  # ####
  
  # calcula-se as variaveis necessarias para regressao
  LN_DH <- log(DF[[DH]])
  INV_I <- 1/DF[[AGE]]
  
  
  if(model %in% c("Schumacher","schumacher") ){
    
    # ajuste do modelo
    reg <- stats::lm(LN_DH ~ INV_I)
    
    # salva-se os coeficientes em objetos separados
    B0 <- stats::coef(reg)[[1]]
    B1 <- stats::coef(reg)[[2]]
    
    # estima-se a altura
    DF["DH_EST"] <- exp( B0 + B1*( 1/ DF[[AGE]] )  ) 
    
    # estima-se a altura dominante na age index
    # que sera utilizada como base futuramente
    DH_EST_II <- exp( B0 + B1*( 1/ AGE_INDEX )  )
    
    
  } else if(model %in% c("Curtis", "curtis") ){
    # calcula-se as variaveis necessarias para regressao
    Hd <- DF[[DH]]
    
    # ajuste do modelo
    reg <- stats::lm(Hd ~ INV_I)
    
    # salva-se os coeficientes em objetos separados
    B0 <- stats::coef(reg)[[1]]
    B1 <- stats::coef(reg)[[2]]
    
    # estima-se a altura
    DF["DH_EST"] <- B0 + B1*( 1/ DF[AGE] )  
    
    # estima-se a altura dominante na age index
    # que sera utilizada como base futuramente
    DH_EST_II <- B0 + B1*( 1/ AGE_INDEX )  
    
    
  } else if(model %in% c("Chapman-Richards", "chapman-richards") ){
    
    chapman_model <- stats::as.formula( paste(DH, "~ b0 * (1 - exp( -b1 *", AGE, ")  )^b2" ) )
    
    # ajuste do modelo
    reg <- minpack.lm::nlsLM( chapman_model, DF, start = start_chap )

    B0 <- stats::coef(reg)[[1]]
    B1 <- stats::coef(reg)[[2]]
    B2 <- stats::coef(reg)[[3]]
    
    
    # DH Estimado
    DF["DH_EST"] <- B0 * (1 - exp( -B1 * DF[[AGE]] )  )^B2 
    
    # estima-se a altura dominante na age index
    # que sera utilizada como base futuramente
    DH_EST_II <- B0 * (1 - exp( -B1 * AGE_INDEX )  )^B2 
    
  }else if(model %in% c("Bailey-Clutter", "bailey-clutter" ) ){
    
    bailey_model <- stats::as.formula( paste("log(", DH,") ~ b0 + b1*(inv(",AGE,")^b2)" ) )
    
    # ajuste do modelo
    reg <- minpack.lm::nlsLM( bailey_model, DF, start = start_bailey )
    
    B0 <- stats::coef(reg)[[1]]
    B1 <- stats::coef(reg)[[2]]
    B2 <- stats::coef(reg)[[3]]
    
    
    # DH Estimado
    DF["DH_EST"] <- exp( B0 + B1 * ( inv(DF[[AGE]])^B2 ) )
    
    # estima-se a altura dominante na age index
    # que sera utilizada como base futuramente
    DH_EST_II <- exp( B0 + B1 * ( inv(AGE_INDEX)^B2 ) )
    
  }else(stop("Please choose between Schumacher, Curtis, Chapman-Richards or Bailey-Clutter models", call. = F))
  
  
  ## Correlacao
  correl <- stats::cor(DF[[DH]], DF[["DH_EST"]])
  
  ## Bias
  bias_porc <- sum(DF[[DH]] - DF[["DH_EST"]])/nrow(DF) * 100
  
  ## RQEM - raiz quadrada do erro médio (RMSE - root mean squared error )
  RQEM <- 1/mean(DF[[DH]]) * ( sqrt( sum((DF[[DH]] - DF[["DH_EST"]])^2)/nrow(DF) )  ) * 100
  
  quali <- data.frame(bias_porc, RQEM, correl)
  
  # estima-se o fator que sera utilizado para gerar as curvas
  DF["FATOR"] <- DF[DH] / DF["DH_EST"]
  
  
  lim_inf <- DH_EST_II * min( DF["FATOR"] )
  lim_sup <- DH_EST_II * max( DF["FATOR"] )
  
  # calcula-se o intervalo de classe
  intervalo <- ceiling(  (lim_sup - lim_inf ) / NC  )
  
  # A funcao ira arredondar para o mais proximo divisor de 5, ou
  # apenas arredondar o numero, dependendo da escolha do usuario
  
  if(round_classes == T){
    
    mround <- function(x,base){ 
      base*round(x/base) 
    } 
    n <- 5
  } else if(round_classes == F){
    
    mround <- base::round
    n <- 0
    
  }
  
  # como iremos utilizar um loop for a seguir,
  # criamos uma lista que contem o numero de entradas referente
  # ao numero de classes desejado
  
  list <- vector("list", NC)
  
  # os primeiros limites sao calculados separadamente do loop
  list[[1]] <- data.frame(
    classe = as.character(utils::as.roman(1)),
    
    nivel = c("inf","sup"), 
    
    limites = c(mround(floor(lim_inf), n) , 
                mround(floor(lim_inf), n) + intervalo), 
    
    fator = c(mround(floor(lim_inf), n) / DH_EST_II,
              (mround(floor(lim_inf), n) + intervalo ) / DH_EST_II )  )
  
  
  # agora calcula-se os demais limites, seguindo o mesmo raciocinio anterior
  for(i in 2:(NC)){
    
    list[[i]] <- data.frame(
      classe = as.character(utils::as.roman(i)),
      nivel = c("inf","sup"), 
      limites = c(list[[i-1]] [[3]] [[2]],
                  list[[i-1]] [[3]] [[2]] + intervalo),
      fator = c(list[[i-1]] [[3]] [[2]] / DH_EST_II,
                (list[[i-1]] [[3]] [[2]]+ intervalo ) / DH_EST_II )  )
    
  }
  
  # transformacao da lista em matriz e em seguida em data frame
  tab <- data.frame(do.call(rbind, list))
  
  # cria-se uma coluna adicional, para que se possa unir os dados utilizando merge
  # sem perder a ordem dos fatores
  DF["AUX"] <- 1
  tab["AUX"] <- 1
  
  # une-se dos dados originais com a tabela de classes
  tab_curva <- merge( DF[c("AUX",DH, "DH_EST", AGE)], tab , by = "AUX")
  
  # transforma-se os limites em fator, para que o grafico possa ser plotado corretamente:
  tab_curva["limites"] <- factor(tab_curva[["limites"]])
  
  # Calcula-se do DH com base no fator calculado
  tab_curva["DH_CURVE"] <- tab_curva["DH_EST"] * tab_curva["fator"]
  
  # reorganiza-se os dados em funcao da age
  
  #tab_curva <- tab_curva[order(tab_curva[AGE]),]
  tab_curva <- dplyr::arrange(tab_curva,.data[[AGE]])
  
  # plota-se o grafico com ggplot
  
  graph <- ggplot2::ggplot(tab_curva ) +  # cria-se a base para o grafico
    ggplot2::geom_point(ggplot2::aes_string(AGE, DH)) + # plota-se os dados originais como pontos
    ggplot2::geom_line(ggplot2::aes_string( AGE, "DH_CURVE", color = "limites" ), size = 1.8 ) + # plota-se as linhas utilizando 
    ggplot2::labs(x = "Age",
                  y = "Dominant Height (m)",
                  color = "Site") +
    ggthemes::theme_igray(base_family = "serif") +
    ggplot2::guides(color= ggplot2::guide_legend( nrow = 1) ) + 
    {
      if(gray_scale) ggplot2::scale_color_grey(start = 0.8, end = 0.2)
     } +
    ggplot2::theme(
      legend.position = "bottom",
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title   = ggplot2::element_text(size = 17), 
      axis.text    = ggplot2::element_text(size = 15),
      axis.line.x = ggplot2::element_line(color="black"),
      axis.line.y = ggplot2::element_line(color="black"),
      strip.text.x = ggplot2::element_text(size = 19)   )
  
  
  tab_curva_cor <- tab_curva %>% 
    tidyr::unite(C, classe, nivel) %>% 
    dplyr::select(!!rlang::sym(AGE),C, DH_EST, DH_CURVE)%>% 
    dplyr::group_by_at(dplyr::vars(AGE, C) ) %>%  
    dplyr::mutate(aux=dplyr::row_number()) %>% 
    tidyr::spread(C, DH_CURVE, sep = "_")%>% 
    dplyr::summarise_at(dplyr::vars(dplyr::contains("_")),mean)
  
  if(output == "plot"){
    
    graph
    
  }else if(output == "table"){
    
    return(tab_curva_cor)
    
  }else if(output == "table_plot"){
    
    print(graph)
    return(tab_curva_cor)
    
  }else if(output == "full"){
    return(
      list(
        Res_perc = forestmangr::resid_plot(DF, DH, "DH_EST"),
        Plot_CG = graph,
        Regression = reg,
        Quality_of_fit = round(quali, 4),
        DH_I_Index = DH_EST_II,
        Class_table = tab[,-5],
        Curve_table = tab_curva_cor
      )
    )
    
    
  }else{
    
    stop("Please use 'plot', 'table' 'table_plot' or 'full' output ",.call=F)
    
  }
  
  
} 