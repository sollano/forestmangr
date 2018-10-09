#' @export

curva_guia <- function(df, hd, idade, idade_indice, n_classes, model = "Schumacher", start_chap = c(b0=23, b1=0.03, b2 = 1.3), start_bailey = c( b0=3, b1=-130, b2 = 1.5), round_classes = F, output = "full"){
  # checagem de variaveis ####
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se hd nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(hd) ){  
    stop("hd not set", call. = F) 
  }else if( !is.character(hd) ){
    stop("'hd' must be a character containing a variable name", call.=F)
  }else if(length(hd)!=1){
    stop("Length of 'hd' must be 1", call.=F)
  }else if(forestr::check_names(df, hd)==F){
    stop(forestr::check_names(df, hd, boolean=F), call.=F)
  }
  
  # se idade nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(idade) ){  
    stop("idade not set. Please use number of months", call. = F) 
  }else if( !is.character(idade) ){
    stop("'idade' must be a character containing a variable name", call.=F)
  }else if(length(idade)!=1){
    stop("Length of 'idade' must be 1", call.=F)
  }else if(forestr::check_names(df, idade)==F){
    stop(forestr::check_names(df, idade, boolean=F), call.=F)
  }
  
  # Se idade_indice nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( idade_indice )){
    stop( "'idade_indice' must be numeric", call.=F)
  }else if(length(idade_indice)!=1){
    stop("Length of 'idade_indice' must be 1", call.=F)
  }else if(! idade_indice %in%  seq(from=1,to=200,by=0.1) ){
    stop("'idade_indice' must be a number between 1 and 200", call.=F)
  }
  
  # Se n_classes nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( n_classes )){
    stop( "'n_classes' must be numeric", call.=F)
  }else if(length(n_classes)!=1){
    stop("Length of 'n_classes' must be 1", call.=F)
  }else if(! n_classes %in%  seq(from=1,to=10,by=1) ){
    stop("'n_classes' must be a number between 1 and 10", call.=F)
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
  }
  
  # Se start_chap nao for numerico, nao for de tamanho 3, ou nao estiver dentro dos padroes, parar
  if(!is.numeric( start_bailey )){
    stop( "'start_chap' must be numeric", call.=F)
  }else if(length(start_bailey)!=3){
    stop("Length of 'start_chap' must be 3", call.=F)
  }else if(! is.vector(start_bailey) ){
    stop("'start_chap' must be a named vector following the pattern 'c(b0=a,b1=b,b2=c) ", call.=F)
  }
  
  # se round_classes nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! round_classes %in% c(TRUE, FALSE) ){ 
    stop("'round_classes' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(round_classes)!=1){
    stop("Length of 'round_classes' must be 1", call.=F)
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c("graph", "table", "full") ){ 
  stop("'output' must be equal to 'graph', 'table' or 'full' ", call. = F) 
  }
  
  DF <- as.data.frame(df)
  HD <- hd
  IDADE <- idade
  IDADE_INDICE <- idade_indice
  NC <- n_classes
  inv <- forestr::inv
  
  # ####
  
  # calcula-se as variaveis necessarias para regressao
  LN_HD <- log(DF[[HD]])
  INV_I <- 1/DF[[IDADE]]
  
  
  if(model %in% c("Schumacher","schumacher") ){
    
    # ajuste do modelo
    reg <- stats::lm(LN_HD ~ INV_I)
    
    # salva-se os coeficientes em objetos separados
    B0 <- stats::coef(reg)[[1]]
    B1 <- stats::coef(reg)[[2]]
    
    # estima-se a altura
    DF["HD_EST"] <- exp( B0 + B1*( 1/ DF[[IDADE]] )  ) 
    
    # estima-se a altura dominante na idade indice
    # que sera utilizada como base futuramente
    HD_EST_II <- exp( B0 + B1*( 1/ IDADE_INDICE )  )
    
    
  } else if(model %in% c("Curtis", "curtis") ){
    # calcula-se as variaveis necessarias para regressao
    Hd <- DF[[HD]]
    
    # ajuste do modelo
    reg <- stats::lm(Hd ~ INV_I)
    
    # salva-se os coeficientes em objetos separados
    B0 <- stats::coef(reg)[[1]]
    B1 <- stats::coef(reg)[[2]]
    
    # estima-se a altura
    DF["HD_EST"] <- B0 + B1*( 1/ DF[IDADE] )  
    
    # estima-se a altura dominante na idade indice
    # que sera utilizada como base futuramente
    HD_EST_II <- B0 + B1*( 1/ IDADE_INDICE )  
    
    
  } else if(model %in% c("Chapman-Richards", "chapman-richards") ){
    
    chapman_model <- stats::as.formula( paste(HD, "~ b0 * (1 - exp( -b1 *", IDADE, ")  )^b2" ) )
    
    # ajuste do modelo
    reg <- stats::nls( chapman_model, DF, start = start_chap )
    
    B0 <- coef(reg)[[1]]
    B1 <- coef(reg)[[2]]
    B2 <- coef(reg)[[3]]
    
    
    # HD Estimado
    DF["HD_EST"] <- B0 * (1 - exp( -B1 * DF[[IDADE]] )  )^B2 
    
    # estima-se a altura dominante na idade indice
    # que sera utilizada como base futuramente
    HD_EST_II <- B0 * (1 - exp( -B1 * IDADE_INDICE )  )^B2 
    
  }else if(model %in% c("Bailey-Clutter", "bailey-clutter" ) ){
    
    bailey_model <- stats::as.formula( paste("log(", HD,") ~ b0 + b1*(inv(",IDADE,")^b2)" ) )
    
    # ajuste do modelo
    reg <-stats:: nls( bailey_model, DF, start = start_bailey )
    
    B0 <- coef(reg)[[1]]
    B1 <- coef(reg)[[2]]
    B2 <- coef(reg)[[3]]
    
    
    # HD Estimado
    DF["HD_EST"] <- exp( B0 + B1 * ( inv(DF[[IDADE]])^B2 ) )
    
    # estima-se a altura dominante na idade indice
    # que sera utilizada como base futuramente
    HD_EST_II <- exp( B0 + B1 * ( inv(IDADE_INDICE)^B2 ) )
    
  }else(stop("Please choose between Schumacher, Curtis, Chapman-Richards or Bailey-Clutter models", call. = F))
  
  
  ## Correlacao
  correl <- cor(DF[[HD]], DF[["HD_EST"]])
  
  ## Bias
  bias_porc <- sum(DF[[HD]] - DF[["HD_EST"]])/nrow(DF) * 100
  
  ## RQEM - raiz quadrada do erro médio (RMSE - root mean squared error )
  RQEM <- 1/mean(DF[[HD]]) * ( sqrt( sum((DF[[HD]] - DF[["HD_EST"]])^2)/nrow(DF) )  ) * 100
  
  quali <- data.frame(bias_porc, RQEM, correl)
  
  # estima-se o fator que sera utilizado para gerar as curvas
  DF["FATOR"] <- DF[HD] / DF["HD_EST"]
  
  
  lim_inf <- HD_EST_II * min( DF["FATOR"] )
  lim_sup <- HD_EST_II * max( DF["FATOR"] )
  
  # calcula-se o intervalo de classe
  intervalo <- ceiling(  (lim_sup - lim_inf ) / NC  )
  
  # A funcao ira arredondar para o mais proximo divisor de 5, ou
  # apenas arredondar o numero, dependendo da escolha do usuario
  
  if(round_classes == T){
    
    mround <- forestr::mround
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
    classe = as.character(as.roman(1)),
    
    nivel = c("inf","sup"), 
    
    limites = c(mround(floor(lim_inf), n) , 
                mround(floor(lim_inf), n) + intervalo), 
    
    fator = c(mround(floor(lim_inf), n) / HD_EST_II,
              (mround(floor(lim_inf), n) + intervalo ) / HD_EST_II )  )
  
  
  # agora calcula-se os demais limites, seguindo o mesmo raciocinio anterior
  for(i in 2:(NC)){
    
    list[[i]] <- data.frame(
      classe = as.character(as.roman(i)),
      nivel = c("inf","sup"), 
      limites = c(list[[i-1]] [[3]] [[2]],
                  list[[i-1]] [[3]] [[2]] + intervalo),
      fator = c(list[[i-1]] [[3]] [[2]] / HD_EST_II,
                (list[[i-1]] [[3]] [[2]]+ intervalo ) / HD_EST_II )  )
    
  }
  
  # transformacao da lista em matriz e em seguida em data frame
  tab <- data.frame(do.call(rbind, list))
  
  # cria-se uma coluna adicional, para que se possa unir os dados utilizando merge
  # sem perder a ordem dos fatores
  DF["AUX"] <- 1
  tab["AUX"] <- 1
  
  # une-se dos dados originais com a tabela de classes
  tab_curva <- merge( DF[c("AUX",HD, "HD_EST", IDADE)], tab , by = "AUX")
  
  # transforma-se os limites em fator, para que o grafico possa ser plotado corretamente:
  tab_curva["limites"] <- factor(tab_curva[["limites"]])
  
  # Calcula-se do HD com base no fator calculado
  tab_curva["HD_CURVA"] <- tab_curva["HD_EST"] * tab_curva["fator"]
  
  # reorganiza-se os dados em funcao da idade
  tab_curva <- tab_curva[order(tab_curva[IDADE]),]
  
  # plota-se o grafico com ggplot
  
  graph <- ggplot2::ggplot(tab_curva ) +  # cria-se a base para o grafico
    ggplot2::geom_point(ggplot2::aes_string(IDADE, HD)) + # plota-se os dados originais como pontos
    ggplot2::geom_line(ggplot2::aes_string( IDADE, "HD_CURVA", color = "limites" ), size = 1.8 ) + # plota-se as linhas utilizando 
    ggplot2::labs(x = "Idade (meses)",
                  y = "Altura dominante (m)",
                  color = "Site") +
    ggthemes::theme_igray(base_family = "serif") +
    ggplot2::guides(color= ggplot2::guide_legend( nrow = 1) ) + 
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
    dplyr::select(!!rlang::sym(IDADE),C, HD_EST, HD_CURVA)%>% 
    dplyr::group_by_at(dplyr::vars(IDADE, C) ) %>%  
    dplyr::mutate(aux=row_number()) %>% 
    tidyr::spread(C, HD_CURVA, sep = "_")%>% 
    dplyr::summarise_at(dplyr::vars(dplyr::contains("_")),mean)
  
  if(output == "graph"){
    
    graph
    
  }else if(output == "table"){
    
    return(tab_curva_cor)
    
  }else if(output == "full"){
    return(
      list(
        Res_porc = forestr::residuos(DF, HD, "HD_EST"),
        Grafico_CG = graph,
        Ajuste = reg,
        Qualidade_ajuste = round(quali, 4),
        HD_I_Indice = HD_EST_II,
        Tab_Classes = tab[,-5],
        Tab_Curva = tab_curva_cor
      )
    )
    
    
  }else{
    
    stop("Please use 'graph', 'table' or 'full' output ",.call=F)
    
  }
  
  
} 