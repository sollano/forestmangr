#' Ajuste de funcoes lineares com saida customizada
#' @description 
#' Funcao para se gerar uma tabela por grupo ou nao contendo apenas as informacoes mais importantes de uma regressao.
#' Possui integracao com o pacote dplyr.
#' @details 
#' Com esta funcao nao ha a necessidade de se utilizar a funcao do() para se ajustar um modelo linear
#' dentro de um pipe do dplyr. Alem disso sua saida com os coeficientes em forma de coluna e
#' propria para a aplicacao do modelo, podendo-se chamar os coeficientes por nome ou posicao na coluna.
#' Alem disso, caso output = "merge", une-se o dataframe original aos coeficientes gerados automaticamente.
#' 
#' @param df Data frame a ser utilizado.
#' @param modelo Modelo que sera ajustado. Pode ser entrado de com ou sem aspas. lados x e y da equacao devem ser separados por "~".
#' @param .groups (opcional) Variaveis classificatorias que serao utilizadas para realizar a regressao por grupos. Caso este argumento seja \code{NULL}, serao utilizados grupos ja definidos no dataframe. Caso nao sejam encontrados grupos no dataframe, a regressao sera aplicada para todo o dataframe. Padrao: \code{NULL}.
#' @param output Indica se a saida sera a tabela de coeficientes, a tabela unida aos dados originais ou o y estimado. Pode ter como entrada "table", "merge", "est" ou "nest". Padrao: \code{"table"}. 
#' @param est.name Nome do y estimado, caso \code{est = TRUE}. Padrao: \code{"est"}. 
#' @param keep_model Indica se a variavel contendo o(s) ajuste(s) deve ser mantida ou nao. Padrao: \code{FALSE}.
#' @return  Dataframe. Sua composicao varia de acordo com o argumento ouput.
#'
#' @export
#' @examples 
#' library(forestmangr)
#' library(dplyr)
#' 
#' data("ex7_mfr")
#' head(ex7_mfr)
#' 
#' # Para preparar gerar dados de exemplo para a regressao, 
#' # utiliza-se a funcao smaliancc:
#' dados_smalian <- smaliancc(ex7_mfr,"di_cc", "hi","ARVORE") %>% 
#'    group_by(FAZENDA, PROJETO, TALHAO, MATGEN, ARVORE ) %>% 
#'    summarise(VCC=sum(VCC,na.rm=T), DAP=mean(DAP), HT=mean(HT) ) %>% 
#'    ungroup
#'    
#' dados_smalian
#' 
#' # Agora sera ajustado o modelo de Schumacher e Hall:
#' 
#' # Rodar regressao, e gerar tabela com coeficientes, R2 e erro:
#' lm_table(dados_smalian, log(VCC) ~  log(DAP) + log(HT))
#' 
#' # ou
#' dados_smalian %>% 
#'   lm_table(log(VCC) ~  log(DAP) + log(HT) )
#'   
#' 
#' # Rodar regressao por talhao, e gerar tabela com coeficientes, R2 e erro:
#' lm_table(dados_smalian, log(VCC) ~  log(DAP) + log(HT), "TALHAO")
#' 
#' # ou
#' dados_smalian %>% 
#'   group_by(TALHAO) %>% 
#'   lm_table(log(VCC) ~  log(DAP) + log(HT) )
#'   
#' # Rodar regressao, e gerar tabela com coeficientes, R2 e erro,
#' # e anexar tabela aos dados originais:
#' lm_table(dados_smalian, log(VCC) ~  log(DAP) + log(HT), "TALHAO", output = "merge")
#' 
#' dados_smalian %>% 
#'   lm_table(log(VCC) ~  log(DAP) + log(HT),"TALHAO", output = "merge")
#'    
#' # Rodar regressao, e estimar a variavel y do modelo
#' dados_smalian %>% 
#'  lm_table(log(VCC) ~  log(DAP) + log(HT),"TALHAO", output = "est", est.name = "VOL_EST")
#'    
#' # Rodar regressao, e gerar dataframe com variaveis agrupadas
#' dados_smalian %>% 
#'  lm_table(log(VCC) ~  log(DAP) + log(HT),"TALHAO", output = "nest")
#'  
#'  
#' # No exemplo a seguir, estima-se a altura das arvores nao medidas
#' # de um inventario florestal.
#' 
#' # Para isso basta ajustar o modelo normalmente, e selecionar "est" como output.
#' # A funcao ira ajustar o modelo ignorando as arvores nao medidas
#' # automaticamente.
#' #
#' # Em seguida seleciona-se apenas as arvores nao medidas, e repete-se
#' # as arvores medidas em uma coluna separada, utilizando ifelse.
#' 
#' data("ex15_mfr")
#' 
#' dados_inv <- ex15_mfr
#' 
#' dados_inv <- dados_inv %>%  
#'   lm_table(log(HT) ~ inv(DAP),output = "est" ) %>% 
#'   mutate( HT_EST = if_else(is.na(HT), est, HT ) )
#' head(dados_inv, 20)
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

lm_table <- function(df, modelo, .groups, output = "table", est.name = "est", keep_model = F){
  # Checagem de variaveis ####
  
  # Definir pipe para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se modelo nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(modelo) ){  
    stop("modelo not set", call. = F) 
  }else if(is.character(modelo)){
    modelo <- stats::as.formula(modelo)
  }else if(!is(modelo, "formula") ){
    stop("'modelo' must be a character or a formula containing a model", call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if((missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups=="") && !is.null(dplyr::groups(df))){
    .groups_syms <- rlang::syms(dplyr::groups(df))
  }else if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
    .groups_syms <- character()
    # Se groups for fornecido verificar se todos os nomes de variaveis fornecidos existem no dado  
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(forestmangr::check_names(df,.groups)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,.groups, boolean=F), call.=F)
    # se os grupos forem fornecidos e forem nomes dos dados
    # Transformar o objeto em simbolo, para que dplyr entenda
    # e procure o nome das variaveis dentro dos objetos
  }else{
    .groups_syms <- rlang::syms(.groups)
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('table', 'merge', 'est', 'estimate', 'nest') ){ 
  stop("'output' must be equal to 'table', 'merge', 'est', 'estimate' or 'nest' ", call. = F) 
  }
  
  # se keep_model nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! keep_model %in% c(TRUE, FALSE) ){ 
    stop("'keep_model' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(keep_model)!=1){
    stop("Length of 'keep_model' must be 1", call.=F)
  }
  
  # ####
  
  #Extrair o y do modelo
  Y <- all.vars( formula(modelo)[[2]] )
  
  # converte modelo para formula
  mod <- formula(modelo)
  
  tidy_ <- function(x){
    tibble::tibble(b=broom::tidy(x)$term, # criamos um data frame que tem apenas as colunas dos betas e seus valores
           estimate=broom::tidy(x)$estimate ) %>% 
      dplyr::mutate(b = factor(b, labels=0:(length(b)-1) ) ) %>% # mudamos os nomes dos coeficientes para bn
      tidyr::spread( b, estimate,sep="" )     # com spread deixamos a tabela horizontal, colocando cada coeficiente em uma coluna
    
  }
  
  glance_ <- function(x){ 
    
    tibble::tibble( 
      Rsqr      = broom::glance(x)$r.squared,
      Rsqr_adj  = broom::glance(x)$adj.r.squared, # criamos um data table que tem apenas as variaveis R quadrado ajustado e erro padrao
      Std.Error = broom::glance(x)$sigma ) }
  
  # Aqui e onde a regressao e realmente feita
  
  x <- df %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by( !!!.groups_syms ) %>% 
    tidyr::nest()  %>% 
    dplyr::mutate(Reg = purrr::map(data, ~stats::lm(mod, data =., na.action = na.exclude ) ),
           Coefs  = purrr::map(Reg, tidy_   ),
           Qualid = purrr::map(Reg, glance_ ),
           Res = purrr::map(Reg, resid),
           est = purrr::map2(Reg, data, predict) ) %>% 
    dplyr::ungroup()
  
  x$A <- NULL 
  
  # tirar o ln do est se o y do modelo tiver ln
  if( any( grepl("^LN|LOG", formula(modelo)[2], ignore.case = T ) ) ){
    
    x <- x %>% dplyr::mutate(est = purrr::map(est, exp) ) 
    
  }
  
  
  if(output == "table"){
    
    # table ira resultar em uma tabela com os coeficientes e as variaveis de qualidade
    y <-  x %>% 
      tidyr::unnest(Coefs, Qualid, .drop = F) %>% 
      dplyr::select(-data,-Res,-est)
    
    
  }else if(output == "merge"){
    
    # Merge ira unir os coeficientes aos dados originais
    y <- x %>% 
      tidyr::unnest(data, .drop = F) %>% 
      tidyr::unnest(Coefs, Qualid ) %>% 
      dplyr::select(-est, -Res)
    
  }else if(output %in% c("est", "estimate")){
    
    #est ou estimate ira estimar a variavel y e uni-la aos dados originais
    y <- x %>% 
      tidyr::unnest(data, est, .drop = F) %>% 
      dplyr::select(- dplyr::one_of( c("Coefs", "Qualid", "Res", "est" )), est )   # passar est para final da tabela
    
  }else if( output == "nest" ){
    
    # nest ira retornar uma tibble com varias list-columns
    y <- x
    
  }
  
  # Remover o modelo caso o usuario deseje
  if(keep_model==F & output != "nest"){y$Reg <- NULL}
  # Renomear est
  names(y)[names(y)=="est"] <- est.name
  
  return(y)
  
}
