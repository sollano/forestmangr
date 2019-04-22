#' @title
#' Identity of a Model complete Test
#' @description 
#' Function for using the Identity of a Model test, as described by Regazzi (1999). 
#' Tests all combinations of factors and outputs the final result of the test.
#'
#' @param df a data frame.
#' @param factor Quoted name of the factor variable used to differentiate the data projects in the test.
#' @param reduced_model Quoted or unquoted reduced model used in the test.  The variables mentioned in the model must exist in the provided data frame. X and Y sides of the model must be separated by "~".
#' @param gray_scale If \code{TRUE} a gray scale will be used in the plots. Default: \code{FALSE}.
#' @param signif Numeric value for the significance level used in the test. Default: \code{0.05}.
#' @param font font family used in the plots. Can be either \code{"serif"} for Times New Roman or \code{"sans"} for arial unicode MS. Default: \code{"serif"}.
#' @param output Defines the type of output. If \code{"table"} an anova table with the identity of model test is provided,
#' if \code{"plot"} a ggplot plot/object representing the final test is created, 
#' if \code{"plots"} in addition to the final test plot, the original test plot is also printed,
#' if \code{"table_plot"}, both anova table and plot are provided. Default: \code{"table_plot"}
#' @return A list, or a ggplot object, varying according to the \code{output} argument.
#' 
#' @references 
#' Regazzi, A. J. (1999) Teste para verificar a identidade de modelos de regressao e a igualdade de parametros no caso de dados de delineamentos experimentais, Ceres, 46(266), pp. 383–409.
#'
#' @export
#' @examples
#' library(forestmangr)
#' data("exfm13")
#' exfm13
#' 
#' # The objective is to know if the diameter's behavior is similar among 3 species.
#' # For this we'll use a quadratic model. We'll use nitrogen (N) as our X variable.
#'
#' ident_model_full(exfm13, "species", dbh ~ N + N2)
#' 
#' # After an initial test, differences are found. Since there are 3 different species,
#' # further testing is needed. After testing all combinations, we can see that
#' # SUcupira-Preta and Vinhatico are equal, and different from Pequi.
#' # Thus there should be 2 equations, one for pequi, and othe for Sucupira-Preta and Vinhatico.
#' 
ident_model_full <- function(df, factor, reduced_model, gray_scale = TRUE, signif = 0.05, font="serif", output = "table_plot" ){
  # ####
  ..eq.label..<-..rr.label..<-equacao<-NULL

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
  }else if(! output %in% c('table', 'plot', 'plots', 'table_plot') ){ 
    stop("'output' must be equal to 'table', 'plot', 'plots' or 'table_plot' ", call. = F) 
  }
  
 # ####
 
  DF <- as.data.frame(df)
  
  #drop factor levels
  DF <- droplevels(DF)
  
  # Definicao do modelo reduzido
  MODELO_REDUZIDO <- reduced_model
  
  # Definicao da significancia do teste
  SIGNIF <- signif
  
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
  
  
  # Rodar o teste
  test <- ident_model(DF, factor, MODELO_REDUZIDO,output = "table",gray_scale = gray_scale,signif = SIGNIF, font = font)
  
  # captirar o resultado
  resultado <- as.character(test[[3,"Result"]])
  
  resultado
  
  # Testar todas as combinacoes possiveis dos fatores
  ALL_COMB <- utils::combn(FACTOR_LEVELS,2)
  ALL_COMB
  
  # Se o resultado for nao significativo
  if(resultado=="ns"){
    print("no differences where found")
    return(test)
    # se so tiverem duas combinacoes possiveis
  }else if(resultado=="*"&length(ALL_COMB)==2){
    print("there is only one possible combination of factors. No more tests are necessary.")
    return(test)
    
    # Se tiver mais de 1 combinacao, fazer o teste para todas
  }else if(resultado=="*" & length(ALL_COMB)>2){
    print("Multiple combinations of factors found;")
    print("Testing all possible combinations")
    
    COMB_DF <- as.data.frame(ALL_COMB)
    names(COMB_DF) <- paste("test",1:length(COMB_DF),sep="_")
    COMB_DF <- suppressWarnings(COMB_DF %>% tidyr::gather(test,levels))
    
    # criar listas vazias para o for
    lista_testes <- list()
    lista_results <- list()
    
    # fazer um teste para cada combinacao
    for(i in 1:ncol(ALL_COMB)){
      
      # Rodar o teste para a combinacao i, filtrando os dados de acordo com a combinacao i
      lista_testes[[paste(ALL_COMB[,i],collapse =  " & ")]] <- ident_model(DF, factor, MODELO_REDUZIDO,filter = ALL_COMB[,i],output="table")
      # salvar o resultado do teste i em uma lista separada, para ser usado mais tarde
      lista_results[[i]] <- matrix(as.character(lista_testes[[i]] [[3,"Result"]]),nrow=2,ncol=1)
      
    }
    
    #unir com teste original
    lista_testes <- c(original_test=list(test),lista_testes)
    
    if(output=="table"){
      return(lista_testes)
    }
    
    # making DF_F ####
    
    # Salvar o vetor com os resultados do teste no dataframe 
    COMB_DF$resultado <- as.vector(do.call(rbind,lista_results))
    
    COMB_DF <- COMB_DF %>% 
      dplyr::arrange(resultado) %>% 
      # manter resultados com ns
      # manter apenas resultados com * que nao aparecem em ns, ou seja, apenas os * unicos
      dplyr::filter(resultado=="ns" | (resultado=="*" & ! levels %in% levels[resultado=="ns"] )) %>% 
      dplyr::distinct(levels,.keep_all = TRUE)
    
    COMB_DF_F <- COMB_DF %>% 
      # concatenar os resultados ns em uma unica string
      # chamar levels de equacao.
      # Isso e importante pois essa coluna sera utilizada
      # para difenciar as diferentes equacoes
      dplyr::mutate(equacao=dplyr::case_when(
        resultado=="*" ~ levels,
        resultado=="ns" ~ paste(levels[resultado=="ns"],collapse = " & ")  ),
        test=NULL, # remover outras colunas
        levels=NULL) %>% 
      # remover duplicatas
      dplyr::distinct()
    
    COMB_DF_F
    
    # Converter levels para fator, para ficar compativel com
    # a coluna que ele sera comparado no left_join
    COMB_DF <- COMB_DF  %>% 
      dplyr::mutate(levels=forcats::as_factor(levels))
    
    # Renomear levels para o nome utilizado no DF
    names(COMB_DF)[names(COMB_DF)=="levels"] <- factor
    
    # Unir DF com as nomes unicos de equcaoes e seus resultados (COMB_DF)
    # E depois unir com os nomes finais das equacoes (COMB_DF_F)
    DF_F <- dplyr::left_join(DF, COMB_DF,by=factor) %>% 
      dplyr::left_join(COMB_DF_F,by="resultado") %>% 
      dplyr::arrange(equacao) # Organizar por equacao
    
    # plot ####
    
    # renomear equacao para ficar com o mesmo nome
    # do fator
    DF_F[[factor]] <- NULL
    names(DF_F)[names(DF_F)=="equacao"] <- factor
    
    graph <- ident_model(DF_F, factor, MODELO_REDUZIDO, output = "plot",gray_scale = gray_scale,signif = signif, font = font)
    
    if(output == "table_plot"){
      
      print(ident_model(DF, factor, MODELO_REDUZIDO,output = "plot",gray_scale = gray_scale,signif = signif, font = font))
      print(graph)
      return(lista_testes)
      
    }else if(output == "plot"){
      
      return(graph)
      
    }else if(output == "plots"){
      
      print(ident_model(DF, factor, MODELO_REDUZIDO,output = "plot",gray_scale = gray_scale,signif = signif, font = font))
      return(graph)
      
    }else{
      
      stop("please use 'table_plot' 'plot', 'plots' or 'table' ")
      
    }
    
    
    
  }
  
   
}  
