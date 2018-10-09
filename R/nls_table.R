#' Ajuste de funcoes nao lineares com saida customizada
#' @description 
#' Funcao para se gerar uma tabela por grupo ou nao contendo apenas as informacoes mais importantes de uma regressao nao linear,
#' utilizando o algoritmo Levenberg-Marquardt ou Gauss-Newton.
#' Possui integracao com o pacote dplyr.
#' @details 
#' Com esta funcao nao ha a necessidade de se utilizar a funcao do() para se ajustar um modelo linear
#' dentro de um pipe do dplyr. Alem disso sua saida com os coeficientes em forma de coluna e
#' propria para a aplicacao do modelo, podendo-se chamar os coeficientes por nome ou posicao na coluna.
#' Alem disso, caso output = "merge", une-se o dataframe original aos coeficientes gerados automaticamente.
#' 
#' @param df Data frame a ser utilizado.
#' @param modelo Modelo que sera ajustado. Pode ser entrado de com ou sem aspas. lados x e y da equacao devem ser separados por "~".
#' @param mod_start Valores iniciais para os coeficientes do modelo. Pode ser um vetor contendo apenas um valor para cada coeficiente, ou um dataframe contendo valores de coeficiente por um determinado grupo.
#' @param .groups (Opcional) Nomes entre aspas das variaveis classificatorias que podem ser utilizadas para se realizar a regressao por grupo. Caso este argumento seja \code{NULL}, serao utilizados grupos ja definidos no dataframe. Caso nao sejam encontrados grupos no dataframe, a regressao sera aplicada para todo o dataframe. Padrao: \code{NULL}.
#' @param output Indica se a saida sera a tabela de coeficientes, a tabela unida aos dados originais ou o y estimado. Pode ter como entrada "table", "merge", "est" ou "nest". Padrao: \code{"table"}. 
#' @param  est.name Nome do y estimado, caso \code{est = TRUE}. Padrao: \code{"est"}. 
#' @param replace Indica se ajustes por grupo que nao convergiram devem ser substituidos pelo modelo ajustado para todos os tados. Padrão \code{FALSE}.
#' @param keep_model Indica se a variavel contendo o(s) ajuste(s) deve ser mantida ou nao. Padrão: \code{FALSE}.
#' @param algorithm Algoritmo a ser utilizado. Pode ser \code{"LM"} (Levenberg-Marquardt, mais robusto) ou \code{"GN"} (Gauss-Newton, menos robusto, utiliza a funcao nls padrao). Padrão: \code{"LM"}.
#' @return  Dataframe. Sua composicao varia de acordo com o argumento ouput.
#'
#' @export
#' @examples 
#'library(forestmangr)
#'library(dplyr)
#'data("ex14_mfr")
#'dados <- ex14_mfr
#'head(dados)
#'
#'# Ajustar o modelo nao linear de Chapman e Richards para todos os dados
#'nls_table(dados, HD ~ b0 * (1 - exp( -b1 * idade )  )^b2, 
#'          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ) )
#'
#'# Ajustar o modelo nao linear de Chapman e Richards por talhao
#'nls_table(dados,HD ~ b0 * (1 - exp( -b1 * idade )  )^b2,
#'          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),
#'          "talhao")
#'          
#'# ou, utilizando group_by
#'
#'dados %>% 
#'group_by(talhao) %>% 
#'nls_table(HD ~ b0 * (1 - exp( -b1 * idade )  )^b2,
#'          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ) )
#'
#'
#'# Percebe-se que nao houve convergencia em aguns talhoes; neste caso gera-se NAs.
#'# E possivel substituir estes NAs pelos coeficientes ajustados para todos os dados,
#'# com o argumento replace:
#'nls_table(dados,HD ~ b0 * (1 - exp( -b1 * idade )  )^b2, 
#'          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),
#'          "talhao",
#'          replace = T )
#'
#'# Caso se tenha uma tabela com varios chutes iniciais, um para cada talhao, por exemplo,
#'# pode-se utiliza-la da seguinte forma:
#'tab_coef <- data.frame(talhao = c(1:20, 24,25,27,28,30,31,33,35,36,37), 
#'                       rbind(data.frame(b0 = rep(23, 20), b1 = rep(0.03, 20), b2 = rep(1.3, 20) ), 
#'                             data.frame(b0 = rep(23, 10), b1 = rep(0.03, 10), b2 = rep(.5, 10) )  )  )
#'
#'# Basta inserir o dataframe no argumento mod_start
#'nls_table(dados, HD ~ b0 * (1 - exp( -b1 * idade )  )^b2, 
#'          mod_start = tab_coef ,
#'          "talhao",
#'          replace = F )
#'# lembrando que este deve ser um datafame, neste caso.
#'
#'nls_table(dados, HD ~ b0 * (1 - exp( -b1 * idade )  )^b2, 
#'          mod_start = tab_coef ,
#'          "talhao",
#'          replace = T )
#'
#'# E possivel tambem unir os coeficientes obitidos aos dados originais para conveniencia, 
#'# facilitando na hora de se estimar a variavel posteriormente:
#'nls_table(dados,HD ~ b0 * (1 - exp( -b1 * idade )  )^b2, 
#'          mod_start = tab_coef ,
#'          "talhao", 
#'          replace = T,
#'          output = "merge" ) %>% 
#'  mutate(
#'  HD_EST = b0 * (1 - exp( -b1 * idade )  )^b2,
#'  bias = bias_por(y = HD, yhat = HD_EST),
#'  rmse = rmse_por(y = HD, yhat = HD_EST) )
#'
#' # ou, estimar a variavel diretamente utilizando o output "est"
#' nls_table(dados,HD ~ b0 * (1 - exp( -b1 * idade )  )^b2, 
#'          mod_start = tab_coef ,
#'          "talhao", 
#'          replace = T,
#'          output = "est", 
#'          est.name = "HD_EST" ) %>% 
#'  mutate(
#'  bias = bias_por(y = HD, yhat = HD_EST),
#'  rmse = rmse_por(y = HD, yhat = HD_EST) )
#'
#'# No processo interno desta funcao, sao utilizadas
#'# "list_columns", que sao convertidos em colunas comuns 
#'# posteriormente. Caso seja de interesse do usuario trabalhar
#'# com estas colunas, basta utilizar o output "nest":
#'nls_table(dados,HD ~ b0 * (1 - exp( -b1 * idade )  )^b2, 
#'          mod_start = tab_coef ,
#'          "talhao",
#'          output = "nest" )
#'              
#'              
#'# Utilizar algoritmo Gauss-Newton, o mesmo da funcao nls             
#' dados %>% 
#'group_by(talhao) %>% 
#'nls_table(HD ~ b0 * (1 - exp( -b1 * idade )  )^b2,
#'          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),algorithm="GN" )
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}


nls_table <- function(df, modelo, mod_start, .groups, output = "table", est.name = "est", replace = F, keep_model = F, global_start, algorithm="LM") {
  # Checagem de variaveis ####
  
  # Definir pipe para facilitar
  `%>%` <- dplyr::`%>%`
  `%T>%` <- magrittr::`%T>%`
  
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
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('table', 'merge', 'est', 'estimate', 'nest') ){ 
    stop("'output' must be equal to 'table', 'merge', 'est', 'estimate' or 'nest' ", call. = F) 
  }
  
  if(  missing(mod_start) || mod_start == "" || is.null(mod_start) ){  
    stop("mod_start not set. Please insert start up values for the model's parameters", call. = F) 
  }else if( is.vector(mod_start) ){
    
  }else if(is.vector(mod_start) & !is.numeric( mod_start )){
    stop( "'mod_start' must be a numeric vector or a dataframe", call.=F)
  }else if( is.data.frame(mod_start)){
    
  }else{
    stop("'mod_start' must be a numeric vector or a dataframe", call.=F)
  }
  
  # se replace nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! replace %in% c(TRUE, FALSE) ){ 
    stop("'replace' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(replace)!=1){
    stop("Length of 'replace' must be 1", call.=F)
  }
  
  mod <- stats::formula(modelo)
  
  # Se nenhum nome estiver no modelo, parar
  if(!any(names(df) %in% all.vars( mod ) ) ){
    
    stop("Variables not found. Check variable names inside model", call. = F)
    
    
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
  
  # Se algorithm nao for character,ou nao for de tamanho 1, parar
  if(!is.character( algorithm )){
    stop( "'algorithm' must be character", call.=F)
  }else if(length(algorithm)!=1){
    stop("Length of 'algorithm' must be 1", call.=F)
  }else if(! algorithm %in% c('GN', 'LM') ){ 
    stop("'algorithm' must be equal to 'GN' or 'LM' ", call. = F) 
  }
  
  # ####
  
  # Criar funcao customizada que retira os betas colocando cada um em uma coluna
  tidy_ <- function(x) {
    tibble::tibble(b = broom::tidy(x)$term, estimate = broom::tidy(x)$estimate) %>% 
      dplyr::mutate(b = factor(b, labels = 0:(length(b) - 1))) %>% 
      tidyr::spread(b, estimate, sep = "")
  }
  
  # Criar funcao customizada que extrai os valores estimados e salva em um dataframe
  predict_ <- function(x,.groups){ 
    data.frame(est = stats::predict(x,.groups) )
  }
  
  # Criar funcao customizada que repete valores e salva em um dataframe
  rep_ <- function(x,n){
    
    data.frame(C = rep(x,n) )
    
  }
  if(algorithm=="LM"){
    nls <- minpack.lm::nlsLM
  }else if(algorithm=="GN"){
    nls <- stats::nls
  }
  # Transformar nls em uma funcao segura, que gera erros silenciosamente
  safe_nls <- purrr::safely(nls)
  
  # A seguir sera calculado a media dos coeficientes, que pdoera ser utilizada
  # e converte-se mod_start para vetor, caso ele seja uma lista
  if(is.data.frame(mod_start)){
    
    # calcular a media e converter para vetor
    mod_start_mean <-  mod_start[, - which(names(mod_start) %in% .groups ) ]
    mod_start_mean <- mod_start_mean[ , sapply(mod_start_mean , is.numeric) ]
    mod_start_mean <- mod_start_mean %>% dplyr::summarise_all(mean,na.rm=T) %>% unlist
    
  }else if(is.vector(mod_start)){
    
    mod_start_mean <- as.data.frame(t(mod_start)) %>% dplyr::summarise_all(mean,na.rm=T) %>% unlist
    
  }else if(is.list(mod_start)){
    # se mod_start for lista, converter para fator
    mod_start <- do.call(cbind,mod_start)
    # calcular a media e converter para vetor
    mod_start_mean <- as.data.frame(t(mod_start)) %>% dplyr::summarise_all(mean,na.rm=T) %>% unlist
  }else{
    stop("mod_start must be a data frame, a vector, or a list",call. = F)
  }
  
  # se verdadeiro, a regressao sera rodada para todos os dados
  # Seus coeficientes serao salvos no objeto aux
  # Se nao, aux sera um dataframe com uma coluna chamada "X1"
  if(replace == T){
    
    if(!missing(global_start)){
      # Substituir media geral se o start global for fornecido
      mod_start_mean <- global_start
      
    }
    
    aux <- df %>%
      dplyr::ungroup() %>% 
      dplyr::mutate(B = "dummy") %>% 
      dplyr::group_by(!!rlang::sym("B")) %>% 
      tidyr::nest() %T>% #%>% 
      {options(warn=-1)} %>% 
      dplyr::mutate(Reg = purrr::map(data, ~safe_nls( mod, ., mod_start_mean, na.action=na.exclude )[[1]]  )  #,
                    #  Coefs = purrr::map(Reg, tidy_)
      )  %T>% 
      {options(warn=0)}
    
    # Parar se a convergencia global nao acontecer
    if(is.null(aux$Reg[[1]])){stop("Convergence not met at global fit", call. = F)}
    
    aux <- aux %>% dplyr::mutate( Coefs = purrr::map(Reg, tidy_) )
    
    #return(aux)
    # betas auxiliares
    aux1 <- aux %>% 
      tidyr::unnest(Coefs, .drop = T) %>% 
      dplyr::select(-B) %>% 
      as.data.frame
    
    # Regressao auxiliar
    aux2 <- aux %>% 
      dplyr::pull(Reg) %>% 
      .[[1]]
    
    
    
  }else if(replace == F){
    
    # Aqui a classe de aux tem que coincidir com a classe da coluna em
    # ele sera inserido (por isso dataframe)
    aux1 <- data.frame(1)
    aux2 <- data.frame(1)
    
  }
  
  # possibly faz com que, quando se gere um erro,
  # seja possivel inserir um resultado para substitui-lo.
  
  # colocando aux como esse argumento em tidy,
  # faz-se que quando houver erro, ou seja, nao houver regressao,
  # sera utilizado aux1, que ira conter
  # os coeficientes ajustados para todos os dados ( caso replace seja verdadeiro)
  # ou um dataframe vazio ( caso replace seja falso)
  
  # No caso de nls, a regressao rodada para todos os dados sera inserida quando
  # a regressao por grupo falhar ( caso replace seja verdadeiro)
  # ou um dataframe vazio ( caso replace seja falso)
  
  # Predict ira gerar um dataframe 1x1 caso falhe. Isso sera utilizado como condicao futuramente,
  # caso replace seja FALSO. Caso replace seja verdadeiro, o erro nao ocorrera,
  # pois onde ele iria falhar, a regressao sera substituida (devido a pos_nls).
  
  pos_tidy <- purrr::possibly(tidy_ , aux1 )
  pos_nls <- purrr::possibly(nls, aux2)
  pos_predict <- purrr::possibly(predict_, data.frame(1) )
  
  
  # Se o chute inicial for um dataframe, unir os coeficientes aos dados
  if(is.data.frame(mod_start) ){
    
    # Pegar os nomes dos betas
    mod_start_names <- names( mod_start[, - which(names(mod_start) %in% .groups ) ] )
    
    # Não tem problema tentar colocar chutes iniciais que nao estejam no modelo,
    # nem tentar colocar nomes que nao existam.groups Por isso o modelo pode rodar com 2 coefs
    # ou 20
    
    suppressMessages(
      
      x <-   df %>%
        dplyr::ungroup() %>% 
        dplyr::full_join(mod_start) %>% 
        dplyr::group_by(!!!.groups_syms) %>% 
        tidyr::nest() %T>% #%>% 
        {options(warn=-1)} %>% 
        dplyr::mutate(Reg = purrr::map(data,  ~pos_nls( mod, ., c( b0  = .[[ mod_start_names[01] ]][1], 
                                                                   b1  = .[[ mod_start_names[02] ]][1], 
                                                                   b2  = .[[ mod_start_names[03] ]][1], 
                                                                   b3  = .[[ mod_start_names[04] ]][1], 
                                                                   b4  = .[[ mod_start_names[05] ]][1], 
                                                                   b5  = .[[ mod_start_names[06] ]][1], 
                                                                   b6  = .[[ mod_start_names[07] ]][1], 
                                                                   b7  = .[[ mod_start_names[08] ]][1], 
                                                                   b8  = .[[ mod_start_names[09] ]][1], 
                                                                   b9  = .[[ mod_start_names[10] ]][1], 
                                                                   b10 = .[[ mod_start_names[11] ]][1], 
                                                                   b11 = .[[ mod_start_names[12] ]][1], 
                                                                   b12 = .[[ mod_start_names[13] ]][1], 
                                                                   b13 = .[[ mod_start_names[14] ]][1], 
                                                                   b14 = .[[ mod_start_names[15] ]][1], 
                                                                   b15 = .[[ mod_start_names[16] ]][1], 
                                                                   b16 = .[[ mod_start_names[17] ]][1], 
                                                                   b17 = .[[ mod_start_names[18] ]][1], 
                                                                   b18 = .[[ mod_start_names[19] ]][1], 
                                                                   b19 = .[[ mod_start_names[20] ]][1], 
                                                                   b20 = .[[ mod_start_names[21] ]][1]   ), na.action=na.exclude   ) ), 
                      Coefs = purrr::map(Reg, pos_tidy),
                      est = purrr::map2(Reg, data, pos_predict) ) %T>% 
                      {options(warn=0)}
      
    ) #suppressMessages
  }else{  
    
    
    x <-   df %>%
      dplyr::ungroup() %>% 
      dplyr::group_by(!!!.groups_syms) %>% 
      tidyr::nest() %T>% #%>% 
      {options(warn=-1)} %>% 
      dplyr::mutate(Reg = purrr::map(data, ~pos_nls( mod, ., mod_start, na.action=na.exclude ) ),
                    Coefs = purrr::map(Reg, pos_tidy),
                    est = purrr::map2(Reg, data, pos_predict) ) %T>% 
                    {options(warn=0)}
  }
  
  if(replace == F){
    
    x <- x %>% 
      dplyr::mutate( est_n =  purrr::map(est, nrow  ),
                     data_n =  purrr::map(data, nrow  ),
                     data_na = purrr::map2(NA,data_n,rep_ )  ) %>% 
      tidyr::unnest(est_n,data_n) %>% 
      dplyr::mutate(est = ifelse(est_n == 1, data_na, est) ) %>% 
      dplyr::select(-est_n,-data_n,-data_na) #%>% 
    #tidyr::unnest(data,est) %>% 
    #select(-A)
    
  }
  
  
  # tirar o ln do est se o y do modelo tiver ln
  if( any( grepl("^LN|LOG", stats::formula(modelo)[2], ignore.case = T ) ) ){
    
    x <- x %>% dplyr::mutate(est = purrr::map(est, exp) )  
    
  }
  
  if (output == "table") {
    y <- tidyr::unnest(x, Coefs, .drop = F) %>% 
      dplyr::select(-data, -est) 
    
  }
  else if (output == "merge") {
    
    # desninhar o dado e remover os chutes iniciais
    x <- x %>% tidyr::unnest(data, .drop = F) 
    x[c("b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15","b16","b17","b18","b19","b20")] <- NULL
    
    # unir os coeficientes aos dados
    y <- x %>% 
      tidyr::unnest(Coefs) %>% 
      dplyr::select(-est)
  }
  else if(output %in% c("est", "estimate")){
    
    #est ou estimate ira estimar a variavel y e uni-la aos dados originais
    y <- x %>% 
      tidyr::unnest(data, est, .drop = F) %>% 
      dplyr::select(-est, est )   # passar est para final da tabela
    
    y[c("A","Coefs", "b0","b1","b2","b3","b4","b5","b6","b7","b8","b9","b10","b11","b12","b13","b14","b15","b16","b17","b18","b19","b20")] <- NULL
    
    
  }
  else if (output == "nest") {
    y <- x
  }
  
  # Remover variavel criada pelos erros (caso ocorram)
  y$X1 <- NULL
  # Remover o modelo caso o usuario deseje
  if(keep_model==F & output != "nest"){y$Reg <- NULL}
  # Renomear est
  names(y)[names(y)=="est"] <- est.name
  
  # Quando a regressao nao roda, a funcao nao gera erros,
  # devido as precaucoes tomadas previamente.
  # O dataframe gerado possui apenas 1 coluna quando isso ocorre.
  # Portanto, verifica-se se a regressao foi atingida ou nao com o if a seguir
  #if(length(y) <= 1 ){stop("Convergence not met. Try to use different start values",call.=F)}
  
  return(y)
}
