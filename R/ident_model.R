#' @title
#' Identity of a Model Test
#' @description 
#' Function for using the Identity of a Model test, as described by Regazzi (1999).
#'
#' @param df a data frame.
#' @param factor Quoted name of the factor variable used to differentiate the data projects in the test.
#' @param reduced_model Quoted or unquoted reduced model used in the test.  The variables mentioned in the model must exist in the provided data frame. X and Y sides of the model must be separated by "~".
#' @param filter Optional argument. If supplied with levels present in \code{factor}, only these levels will be used in the test. \code{NA}.
#' @param gray_scale If \code{TRUE} a gray scale will be used in the plots. Default: \code{FALSE}.
#' @param signif Numeric value for the significance level used in the test. Default: \code{0.05}.
#' @param font font family used in the plots. Can be either \code{"serif"} for Times New Roman or \code{"sans"} for arial unicode MS. Default: \code{"serif"}.
#' @param output Defines the type of output. If \code{"table"} an anova table with the identity of model test is provided,
#' if \code{"plot"} a ggplot plot/object representing the test is created,
#' if \code{"table_plot"}, both anova table and plot are provided, and if \code{"full"},
#' a list is provided, with details on the dummies variables created, the reduced and complete models,
#' the anova table and the plot. Default: \code{"table"}
#' @return A data frame, a ggplot object, or a list, varying according to the \code{output} argument.
#' 
#' @references 
#' Regazzi, A. J. (1999) Teste para verificar a identidade de modelos de regressao e a igualdade de parametros no caso de dados de delineamentos experimentais, Ceres, 46(266), pp. 383–409.
#'
#' @export
#' @examples
#' library(forestmangr)
#' data("exfm13")
#' head(exfm13)
#' 
#' # The objective is to know if the diameter's behavior is similar among 3 species.
#' # For this we'll use a quadratic model. We'll use nitrogen (N) as our X variable.
#'
#' ident_model(exfm13, "species", dbh ~ N + N2)
#' 
#' # This test shows that there are differences between the species. 
#' # We can get more details on this using a different output, that will also
#' # give us a plot:
#' 
#' ident_model(exfm13, "species", dbh ~  N + N2, output = "table_plot")
#' 
#' # This gives us only the plot:
#' ident_model(exfm13, "species", dbh ~  N + N2, output = "table_plot")
#' 
#' # And this gives us additional information on the test:
#' ident_model(exfm13, "species", dbh ~  N + N2, output = "full")
#' 
#' # Looking at the plot, it seems that 2 species are behaving very similar, while
#' # the Pequi species is different from the other 2. We can confirm this by running
#' # the test in a paired fashion, using the filter argument:
#'
#' ident_model(exfm13, "species", dbh ~  N + N2,
#'  filter = c("PEQUI", "SUCUPIRA-PRETA"), output = "table_plot")
#'  
#' ident_model(exfm13, "species", dbh ~  N + N2, 
#' filter = c("PEQUI", "VINHATICO"), output = "table_plot")
#' 
#' ident_model(exfm13, "species", dbh ~  N + N2, 
#' filter = c("SUCUPIRA-PRETA", "VINHATICO"), output = "table_plot")
#'
#' # As we imagined, a single model can be used to describe the behavior of
#' # the "Sucupira-preta" and "Vinhatico" species,
#' # and a second model is needed to explain the Pequi Variable.
#'
#' # It's possible to apply a color scale to the plots, and also change it's font to arial:
#' 
#' ident_model(exfm13, "species", dbh ~  N + N2,output="plot",gray_scale=FALSE,font="sans")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' @author Marcio leles Romarco de Oliveira \email{marcioromarco@@gmail.com}
#' @import ggplot2

ident_model <- function(df, factor, reduced_model, filter = NA, gray_scale = TRUE, signif = 0.05, font="serif", output = "table" ){
  # ####
  ..eq.label..<-..rr.label..<-NULL
  # ####
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se factor nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(factor) ){  
    stop("factor not set", call. = F) 
  }else if( !is.character(factor) ){
    stop("'factor' must be a character containing a variable name", call.=F)
  }else if(length(factor)!=1){
    stop("Length of 'factor' must be 1", call.=F)
  }else if(forestmangr::check_names(df, factor)==F){
    stop(forestmangr::check_names(df, factor, boolean=F), call.=F)
  }
  
  # se reduced_model nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(reduced_model) ){  
    stop("reduced_model not set", call. = F) 
  }else if(is.character(reduced_model)){
   reduced_model <- stats::as.formula(reduced_model)
  }else if(!methods::is(reduced_model, "formula") ){
    stop("'reduced_model' must be a character or a formula containing a model", call.=F)
  }
  
  # Se filter nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(filter)||all(is.null(filter))||all(is.na(filter))||all(filter==F)||all(filter=="")){
    filter <- NULL
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(filter)){ 
    stop("filter must be a character", call. = F)
  }else if(! length(filter) == 2){ 
    stop("Length of 'filter' must be equal to 2", call.=F) 
    # se nao existir algum nivel citado em filter na var factor, parar
  }else if( any( ! filter %in% levels(as.factor(df[[factor]])) ) ){
    stop("Levels in 'filter' do not exist in factor", call.=F) 
  }else{
      # filter dados caso o filtro seja fornecido
      df <- df[ df[[factor]] %in% filter, ]
    }
  
  # se gray_scale nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! gray_scale %in% c(TRUE, FALSE) ){ 
    stop("'gray_scale' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(gray_scale)!=1){
    stop("Length of 'gray_scale' must be 1", call.=F)
  }
  
  # Se signif nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( signif )){
    stop( "'signif' must be numeric", call.=F)
  }else if(length(signif)!=1){
    stop("length of 'signif' must be 1", call.=F)
  }else if(! signif > 0 | ! signif <= 0.30){
    stop("'signif' must be a number between 0 and 0.30", call.=F)
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('table', 'plot', 'table_plot', 'full' ) ){ 
  stop("'output' must be equal to 'table', 'plot', 'table_plot' or 'full' ", call. = F) 
  }
  
  # ####
  
  # Definicao do dataframe
  DF <- as.data.frame(df)
  
  # Definicao do modelo reduzido
  MODELO_REDUZIDO <- reduced_model
  
  # Definicao da significancia do teste
  SIGNIF <- signif
  
  # Defnicao da variavel dependente
  Y <- all.vars( MODELO_REDUZIDO[[2]] )
  
  # Definicao das variaveis independentes
  # estas serao utilizadas para se criar as variaveis dummy
  VARSX <- all.vars(MODELO_REDUZIDO[[3]])
  
  
  # Testar se as colunas existem
  if( ! all ( VARSX %in% colnames(DF) ) ){
    
    stop("Variaveis independentes (x) nao existem no dataframe",call. = F)
  }
  
  if(!Y %in% colnames(DF) ){
    
    stop("Variavel dependente (y) nao existe no dataframe",call. = F)
  }
  
  # Testar se variaveis do modelo sao numericas
  
  if( !all(sapply(DF[VARSX], is.numeric))  ){
    
    stop("Variaveis independentes (x) nao sao numericas",call. = F)
  }
  
  if(!is.numeric(DF[[Y]]) ){
    
    stop("Variavel independente (y) nao e numerica",call. = F)
  }
  
  if(SIGNIF < 0 || SIGNIF > 1 || !is.numeric(SIGNIF)){ 
    
    stop("Valor de significancia invalido. Insira um valor numerico entre 0 e 1.",call. = F)
    
  }
  
  # Numero de variaveis independentes, ou seja,
  # numero de variaveis dummy por nivel que deve ser criada
  N_VARSX <- length(VARSX)
  
  # Conversao da variavel utilizada como fator, para fator
  FACTOR <- as.factor( DF[[factor]] )
  DF[factor] <- FACTOR
  
  # Definicao dos niveis do fator utilizado
  FACTOR_LEVELS <- levels(FACTOR)
  
  
  # Deve existir pelo menos 2 niveis de fator para serem testados
  
  if(nlevels(FACTOR) < 2 ){
    
    stop(cat("Devem existir pelo menos 2 projetos a serem testados \n ( variavel fator possui apenas 1 nivel)"),call. = F )
    
  }
  
  # Se o forem testados mais de 15 fatores, provavelmente foi inserida uma variavel errada como fator
  
  if(nlevels(FACTOR) > 15 ){
    
    warning( paste("Foram testados", nlevels(FACTOR), "projetos", "Confira a variavel de fator") )
    
  }
  
  # ####
  
  # Definicao do numero de niveis 
  N_FACTOR_LEVELS <- length(FACTOR_LEVELS)
  
  # ajuste do modelo reduzido
  LM_REDUZIDO <- stats::lm(MODELO_REDUZIDO, DF)
  
  # resumo sobre o ajuste do modelo reduzido
  SUMMARY_MOD_RED <- summary(LM_REDUZIDO)
  
  # Primeiro cria-se as dummies sem valores,
  # apenas com 0 e 1.
  
  # Sera utilizado o loop for  para isso.
  
  # lista que sera utilizado no loop.
  lista1 <- vector("list", N_VARSX)
  
  # Para cada nivel do fator
  for(i in 1:N_FACTOR_LEVELS){
    
    # criar um vetor na lista, onde
    # insere-se 1 para caso o fator seja igual ao nivel verificado,
    # e 0 caso contrario
    lista1[[paste("D", i, sep = "")]] <- ifelse(FACTOR == FACTOR_LEVELS[i], 1, 0 )
    # Isso sera feito para cada nivel!
    
  }
  
  # Conversao das dummies em colunas de uma matriz.
  DUMMIES1 <- do.call(cbind, lista1)
  
  # Lista que sera utilizada no proximo loop
  lista2 <- vector("list", N_VARSX)
  
  # Aqui cria-se as variaveis dummy com valores
  # Utiliza-se outro for loop, para inserir o valor da variavel verificada
  # quando o fator for igual ao utilizado
  for(j in 1:N_VARSX){
    
    for(i in 1:N_FACTOR_LEVELS){
      
      lista2[[paste("D", i, VARSX[j],sep = "")]] <- ifelse( FACTOR == FACTOR_LEVELS[i], DF[[ VARSX[j]  ]],  0  )
      
    }
    
  }
  
  # Conversao das dummies em colunas de uma matriz
  DUMMIES2 <- do.call(cbind, lista2)
  
  # Uniao das dummies em um unico dataframe
  DUMMIES_FINAL <- as.data.frame(cbind(DUMMIES1,DUMMIES2))
  
  # Definicao dos nomes das dummies
  NAMES_DUMMIES <- names(DUMMIES_FINAL)
  
  # Uniao das dummies aos dados originais
  DF_COMPLETO <- cbind(DF, DUMMIES_FINAL)
  
  # Criacao do modelo completo, utilizando os nomes das dummies
  # com a funcao paste0, concatena-se os nomes das dummies seguidos de "+",
  # formando assim o lado X da equacao.
  # -1 e adicionado ao final da equacao, para se remover o b0 do ajuste
  # A variavel dependente continua sendo a mesma do modelo reduzido, ficando:
  MODELO_COMPLETO <- stats::as.formula(
    paste(Y ,"~", paste0(NAMES_DUMMIES, collapse = "+") , -1))
  MODELO_COMPLETO
  
  # Ajuste do modelo completo
  LM_COMPLETO <- stats::lm(MODELO_COMPLETO, DF_COMPLETO)
  
  # Resumo do modelo completo
  SUMMARY_MOD_COMP <- summary(LM_COMPLETO)
  
  
  # A seguir sao feitos os calculos para a construcao da anova
  # segundo o teste F de regazzi:
  gl_comp <- ncol(DUMMIES_FINAL)
  
  gl_redz <- N_VARSX
  
  reducao_gl <- gl_comp - gl_redz
  
  residuo_gl <- LM_COMPLETO$df.residual
  
  SQParamC <- sum(LM_COMPLETO$fitted.values^2)
  
  SQParamR <- sum(LM_REDUZIDO$fitted.values^2) # + C
  
  SQ_reducao <- SQParamC - SQParamR
  
  SQRes_comp <- sum(LM_COMPLETO$residuals^2)
  
  QMParamC <- SQParamC/gl_comp
  QMParamR <- SQParamR/gl_redz
  
  QMReducao <- round(SQ_reducao / reducao_gl, 4)
  QMResiduo <- round(SQRes_comp / residuo_gl, 4)
  F_regazzi <- round(QMReducao / QMResiduo, 2)
  F_tabelado <- round(stats::qf(p = SIGNIF, df1 = reducao_gl , df2 = residuo_gl, lower.tail = F ), 2)
  p_valor <- stats::pf(F_regazzi , df1 = reducao_gl , df2 = residuo_gl, lower=F)
  # resultado <- ifelse(F_tabelado > F_regazzi, "*", "ns")
  resultado <- ifelse(p_valor < SIGNIF, "*", "ns")
  
  # Criacao da tabela final com os resultados
  TABELA_FINAL <- data.frame(
    
    SV = c("Parameter_c", "Parameter_r", "Reduction", "Residuals"),
    Df =  c(gl_comp, gl_redz, reducao_gl, residuo_gl ),
    Sum_Sq = round(c(SQParamC, SQParamR, SQ_reducao, SQRes_comp), 4),
    Mean_Sq = c(QMParamC, QMParamR, QMReducao, QMResiduo ),
    F_Regazzi = c("", "", F_regazzi ,""),
    F_value = c("", "", F_tabelado, ""),
    # "p-value" = c("", format(p_valor, scientific = F), "", ""),
    "p-value" = c("", "", signif(p_valor, 3), ""),
    Result = c("", "", resultado, "")
    
    
  )
  
  # Grafico com ou sem logaritimo
  
  # Se alguma das variaveis X tiver LN ou LOG no comeco do nome (maiuscula ou nao): 
  if( any( grepl("^LN|LOG", VARSX, ignore.case = T ) ) ){
    
    # Separa LN do nome da variavel, e salva sem o LN
    VARSX[1] <- unlist(strsplit(VARSX[1], "_|\\."))[[2]]
    
    # Poly de grau n com x log
    fun_graph <- y ~ stats::poly(log(x), N_VARSX, raw=T)
    
    # Se alguma das variaveis X tiver EXP no comeco do nome (maiuscula ou nao): 
  } else if(any( grepl("^EXP", VARSX, ignore.case = T ) ) ){
    
    # Separa LN do nome da variavel, e salva sem o LN
    VARSX[1] <- unlist(strsplit(VARSX[1], "_|\\."))[[2]]
    
    # Poly de grau n com x exponencial
    fun_graph <- y ~ stats::poly(exp(x), N_VARSX, raw=T)
    
    # Se alguma das variaveis X tiver INV no comeco do nome (maiuscula ou nao): 
  } else if(any( grepl("^INV", VARSX, ignore.case = T ) ) ){
    
    # Separa INV do nome da variavel, e salva sem o INV
    VARSX[1] <- unlist(strsplit(VARSX[1], "_|\\."))[[2]]
    
    # Poly de grau n com x sem inv
    fun_graph <- y ~ stats::poly(x, N_VARSX, raw=T)
    
  } else{
    
    # polinomio normal
    fun_graph <- y ~ stats::poly(x, N_VARSX, raw=T)
    
  }
  
  # Para o logaritimo no y, utiliza-se os argumentos opcionais de geom_smooth
  # salvos em uma lista.
  # Entao utiliza-se uma condicional, 
  # que ira converter para log caso se tenha lN ou LOG no nome, e caso contrario 
  # sera vazia:
  if( any( grepl("^LN|LOG", Y, ignore.case = T ) ) ){
    
    # Separa LN do nome da variavel, e salva sem o LN
    Y <- unlist(strsplit(Y, "_|\\." ))[[2]]
    
    
    lista_args <- list( family = stats::gaussian(link = 'log') )
    
  }else{
    
    lista_args <- list( )
    
    
  }
  # Grafico diferente para caso o resultado seja significativo ou nao
  
  if(resultado == "*"){
    
    graph <- ggplot2::ggplot(DF, ggplot2::aes_string(VARSX[1], Y, color = factor) ) +
      
      ggplot2::geom_smooth(method = "lm",
                           #        method.args = lista_args,
                           formula = fun_graph ,
                           se = F,
                           size = 1.5,
                           ggplot2::aes_string(color = factor ) ) +
      
      ggpmisc::stat_poly_eq(formula = fun_graph,size = 6,
                            eq.x.rhs = paste("~italic(", VARSX[1] ,")", sep = ""),
                            eq.with.lhs = paste( "italic(hat(", Y, "))~`=`~", sep="" ), 
                            ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), 
                            parse = TRUE) +
      
      ggplot2::stat_summary(fun.y = mean, geom = "point", ggplot2::aes(color=NULL),size = 5 ) +
      
      ggthemes::theme_igray(base_family = font) +
      
      ggplot2::theme(
        legend.position = "bottom",
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(color="black"),
        axis.line.y = ggplot2::element_line(color="black"),
        legend.title    = ggplot2::element_text(size = 17, face = "bold"),
        legend.text     = ggplot2::element_text(size = 17),
        axis.title      = ggplot2::element_text(size = 22, face = "bold"), 
        axis.text       = ggplot2::element_text(size = 17) )
    
  }else if(resultado == "ns"){
    
    graph <- ggplot2::ggplot(DF, ggplot2::aes_string(VARSX[1], Y) ) +
      
      ggplot2::geom_smooth(method = "lm",
                           #     method.args = lista_args,
                           formula = fun_graph, 
                           se = F, 
                           size = 1.5 ) +
      
      
      ggpmisc::stat_poly_eq(formula = fun_graph, size = 6,
                            eq.x.rhs = paste("~italic(", VARSX[1] ,")", sep = ""),
                            eq.with.lhs = paste( "italic(hat(", Y, "))~`=`~", sep="" ), 
                            ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")), 
                            parse = TRUE) +
      
      
      
      ggplot2::stat_summary(fun.y = mean, geom = "point", ggplot2::aes(color=NULL),size = 5 ) +
      
      ggthemes::theme_igray(base_family = font) +
      
      ggplot2::theme(
        legend.position = "bottom",
        panel.grid.major = ggplot2::element_blank(), 
        panel.grid.minor = ggplot2::element_blank(),
        panel.border = ggplot2::element_blank(),
        axis.line.x = ggplot2::element_line(color="black"),
        axis.line.y = ggplot2::element_line(color="black"),
        legend.title    = ggplot2::element_text(size = 17, face = "bold"),
        legend.text     = ggplot2::element_text(size = 17),
        axis.title      = ggplot2::element_text(size = 22, face = "bold"), 
        axis.text       = ggplot2::element_text(size = 17) )
    
  }
  
  if(gray_scale == T){
    
    graph <- graph + ggplot2::scale_color_grey()
    
  }
  
  # Uniao dos objetos de interesse em uma lista
  lista_final <- list(
    dummies_table  = dplyr::as.tbl(DUMMIES_FINAL),
    Reduced_Model  = SUMMARY_MOD_RED, 
    Complete_Model = SUMMARY_MOD_COMP,
    Plot           = graph,
    Regazzi_F_Test = TABELA_FINAL )
  
  
  if(output == "full"){
    
    return(lista_final)
    
    
  }else if(output == "table_plot"){
    
    print(graph)
    return(TABELA_FINAL)
    
  }else if(output == "plot"){
    
    return(graph)
    
    }else if(output == "table"){
    
    return(TABELA_FINAL)
    
  } else{
    
    stop("please use 'full' , 'table_plot' 'plot' or 'table' ")
    
  }
  
}
