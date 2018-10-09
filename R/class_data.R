#' @export

class_data <- function(df, var, nc, .groups){
  # checagem de variaveis ####
  
  # Definir pipe do dplyr, para facilitar
  `%>%` <- dplyr::`%>%`
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se var nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(var) ){  
    stop("var not set", call. = F) 
  }else if( !is.character(var) ){
    stop("'var' must be a character containing a variable name", call.=F)
  }else if(length(var)!=1){
    stop("Length of 'var' must be 1", call.=F)
  }else if(forestmangr::check_names(df, var)==F){
    stop(forestmangr::check_names(df, var, boolean=F), call.=F)
  }
  
  # se .groups nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(.groups) ){  
    stop(".groups not set", call. = F) 
  }else if( !is.character(.groups) ){
    stop("'.groups' must be a character containing a variable name", call.=F)
  }else if( ! length(.groups) %in% 1:10 ){
    stop("Length of '.groups' must an integer number between 1 1 and 10", call.=F)
  }else if(forestmangr::check_names(df, .groups)==F){
    stop(forestmangr::check_names(df, .groups, boolean=F), call.=F)
  }
  
  # Se .groups nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(.groups) && is.null(dplyr::groups(df))){
    stop(".groups must be set if data doesn't have any groups", call. = F)
  }else if(missing(.groups) && !is.null(dplyr::groups(df))){
    .groups_syms <- rlang::syms(dplyr::groups(df))
  }else if(!is.character(.groups)){
    stop(".groups must be a character", call. = F)
  }else if(! length(.groups)%in% 1:10){
    stop("Length of '.groups' must be between 1 and 10", call.=F)
  }else if(forestmangr::check_names(df,.groups)==F){
    stop(forestmangr::check_names(df,.groups, boolean=F), call.=F)
  }else{
    .groups_syms <- rlang::syms(.groups)
  }

  var_sym <- rlang::sym(var)
  
  # ####
  
  # Primeiro calcula-se a media da variavel a ser classificada
  # por grupo determinado pelo usuario, e a anexa-se a mesma aos dados,
  # que sao organizados da menor para o maior valor de media
  df <- df %>% 
    dplyr::group_by( !!!.groups_syms ) %>% 
    dplyr::summarise(Site_medio = mean( !!var_sym ) ) %>% 
    dplyr::left_join(df,.
              , by = .groups 
    ) %>%
    round(4) %>% 
    dplyr::arrange(Site_medio)
  
  # Em seguida, com base no numero de classes, estas medias serao dividas em
  # nc classes.
  #
  # define-se a variavel que sera classificada
  VAR <- df[["Site_medio"]]
  
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
  names(aux3) <- c("Intervalo", "Categoria")
  
  # Se forem 3 categorias, adicionar os nomes baixa media e alta as classes:
  if(nc ==3) {
    
    aux3$Categoria_ <- car::recode(aux3$Categoria, " 1 = 'Inferior'; 2 = 'Media'; 3 = 'Superior' "  )
    
  }
  
  df <- cbind(df, aux3)
  
  return(df)
  
}
