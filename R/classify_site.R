#' @title 
#' Classify inventory data based on site index
#' @description 
#' Use the site variable to classify a forest management data.
#' 
#' @param df A data frame.
#' @param site Quoted name for the site variable.
#' @param nc number of categories used to classify the data. If \code{3}, a additional column will be created with levels Lower, Middle and Upper, referencing the 3 categories. If not, only numbers will be used to differentiate the categories. Default: \code{3}.
#' @param plot Quoted name for the plot variable.
#' @param .groups Optional argument. Quoted name(s) of grouping variables used to fit multiple regressions, one for each level of the provided variable(s). Default \code{NA}.
#' @return A data frame classified based on the site index.
#' 
#' @seealso other sampling functions: 
#'   \code{\link{fit_clutter}} for  fitting Clutter's Growth and Yield, and
#'   \code{\link{est_clutter}} for estimating Clutter's Growth and Yield model variables.
#' @export
#' 
#' @examples 
#' 
#' library(forestmangr)
#' data("exfm17")
#' head(exfm17)
#' 
#' # Classify data into 3 classes:
#' ex_class <- classify_site(exfm17, "S", 3, "plot")
#' head(ex_class ,15)
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#'
classify_site <- function(df, site, nc=3, plot, .groups=NA){
  site_mean<-NULL
  # checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se site nao for fornecido nao for character, ou nao for um nome de siteiavel,ou nao for de tamanho 1, parar
  if(  missing(site) ){  
    stop("site not set", call. = F) 
  }else if( !is.character(site) ){
    stop("'site' must be a character containing a siteiable name", call.=F)
  }else if(length(site)!=1){
    stop("Length of 'site' must be 1", call.=F)
  }else if(forestmangr::check_names(df, site)==F){
    stop(forestmangr::check_names(df, site, boolean=F), call.=F)
  }
  
  # Se plot nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(plot) && is.null(dplyr::groups(df)) ){
    stop("plot not set. plot must be set if data doesn't have any groups", call. = F)
  }else if(missing(plot) && !is.null(dplyr::groups(df)) ){
    plot_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(plot)){
    stop("plot must be a character", call. = F)
  }else if(! length(plot)%in% 1:10){
    stop("Length of 'plot' must be between 1 and 10", call.=F) 
  }else if(forestmangr::check_names(df,plot)==F){
    # Parar se algum nome nao existir, e avisar qual nome nao existe
    stop(forestmangr::check_names(df,plot, boolean=F), call.=F) 
  }else{
    plot_syms <- rlang::syms(plot) 
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups)||any(is.null(.groups))||any(is.na(.groups))||any(.groups==F)||any(.groups=="") ){
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
  
  site_sym <- rlang::sym(site)
  
  # ####
  
  # Primeiro calcula-se a media da variavel a ser classificada
  # por grupo determinado pelo usuario, e a anexa-se a mesma aos dados,
  # que sao organizados da menor para o maior valor de media
  
  suppressMessages(
    df <- df %>% 
      dplyr::group_by(!!!.groups_syms, !!!plot_syms, .add=T ) %>% 
      dplyr::summarise(site_mean = mean( !!site_sym ) ) %>% 
      dplyr::left_join(df,multiple = "all") %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric),\(x) round(x, digits = 4) )) %>% 
      dplyr::arrange(site_mean)
  )
  # Em seguida, com base no numero de classes, estas medias serao dividas em
  # nc classes.
  #
  # define-se a variavel que sera classificada
  VAR <- df[["site_mean"]]
  
  # cria-se uma lista com o numero de linhas correspondente ao numero de classes
  list <- vector("list", nc)
  
  # define-se o intervalo que sera utilizado no calculo
  intervalo <-   (max(VAR) - min(VAR) ) / nc  
  
  # cria-se o primeiro intervalo da tabela de classe;
  
  # a tabela de classe possui 3 colunas:
  # primeiro o intervalo inferior,
  # segundo o intervalo inferior,
  # terceiro o centro de classe.
  
  list[[1]] <- c(min(VAR), 
                 min(VAR) + intervalo , 
                 mean( c(min(VAR), 
                         min(VAR) + intervalo  ) ),
                 1)       
  
  # com o loop for cria-se os demais intervalos
  # o loop vai da segunda linha da lista, ate a ultima linha,
  # que corresponde ao numero de classes utilizado.
  
  # o loop cria as 3 colunas citadas anteriormente, seguindo o padrao:
  
  # primeiro seleciona-se o intervalo superior da classe anterior, 
  # ou seja, linha anterior [i-1] posicao 2 [2];
  # segundo seleciona-se o mesmo item selecionado anteriormente, e adiciona-se
  # o intervalo de classe;
  # terceiro cria-se o centro de classe.
  for(i in 2:nc){
    
    list[[i]] <- c(list[[i-1]] [[2]],  
                   list[[i-1]] [[2]] + intervalo , 
                   mean( c(list[[i-1]][[2]],  
                           list[[i-1]][[2]] + intervalo ) ),
                   i )
    
  }
  
  # transformacao da lista em matriz e em seguida em data frame
  tab <- data.frame(do.call(rbind, list))
  
  # renomear
  names(tab) <- c("inf", "sup", "cc", "categoria")
  
  ## Para se classificar o dado de fato, compara-se o
  ## valor medio com os intervalos superiores das classes.
  ## Para isso, sera utilizado o loop while, em conjunto com o loop for.
  ##
  ## Primeiro, cria-se duas listas com o tamanho do dataframe:
  
  list1 <- vector("list", nrow(df)  )
  list2 <- vector("list", nrow(df)  )
  
  ## Serao utilizadas duas listas, pois serao gerados dados de classes diferentes,
  ## como eles serao gerados em vetores, nao podem ser misturados.
  ## Se numeros e caracteres sao utilizados no mesmo vetor, todos viram caracteres.
  
  ## Em seguida inicia-se o vetor do loop while em 1:
  i <- 1
  
  ## Agora, os loops:
  # Para (loop for) cada classe (1, 2 e 3), faz-se o loop while.
  for(j in 1: nrow(tab)){
    
    # enquanto a classe j for maior ou igual que o site medio i,
    while(tab$sup[j] >= VAR[i]){
      
      # insere-se o intervalo j e a categoria j em um elementos i das listas
      
      list1[[i]] <- tab$sup[j];
      
      list2[[i]] <- c(tab$categoria[j] ) ; 
      
      # aumentar i, ou seja, passar para o proximo site medio
      i <- i+1;
      
      # parar quando acabar o dataframe
      if (is.na(VAR[i] ) ) break
      
    }
    
  }
  
  ## Agora converte-se as listas em data frames:
  aux1 <- data.frame(do.call(rbind, list1))
  aux2 <- data.frame(do.call(rbind, list2))
  
  ## O ultimo passo e unir os dois dataframes gerados em um novo,
  ## dar nome as variaveis, e adicionar os resultados aos dados:
  
  aux3 <- cbind(aux1, aux2)
  names(aux3) <- c("interval", "category")
  
  # Se forem 3 categorias, adicionar os nomes baixa media e alta as classes:
  if(nc ==3) {
    
    aux3$category_ <- car::recode(aux3$category, " 1 = 'Lower'; 2 = 'Middle'; 3 = 'Upper' "  )
    
  }
  
  df <- cbind(df, aux3)
  
  return(df)
  
}
