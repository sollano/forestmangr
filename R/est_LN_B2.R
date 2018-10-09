#' @export

est_LN_B2 <- function(df, site, G, idade, a0, a1, categoria, method = "media" ){
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
  
  # se site nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(site) ){  
    stop("site not set", call. = F) 
  }else if( !is.character(site) ){
    stop("'site' must be a character containing a variable name", call.=F)
  }else if(length(site)!=1){
    stop("Length of 'site' must be 1", call.=F)
  }else if(forestmangr::check_names(df, site)==F){
    stop(forestmangr::check_names(df, site, boolean=F), call.=F)
  }
  
  # se G nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(G) ){  
    stop("G not set", call. = F) 
  }else if( !is.character(G) ){
    stop("'G' must be a character containing a variable name", call.=F)
  }else if(length(G)!=1){
    stop("Length of 'G' must be 1", call.=F)
  }else if(forestmangr::check_names(df, G)==F){
    stop(forestmangr::check_names(df, G, boolean=F), call.=F)
  }
  
  if(  missing(idade) ){  
    stop("idade not set", call. = F) 
  }else if( !is.numeric(idade) ){
    stop("'idade' must be numeric", call.=F)
  }else if( !is.vector(idade) ){
    stop("'idade' must be a numeric vector", call.=F)
  }
  
  # se categoria nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(categoria) ){  
    stop("categoria not set", call. = F) 
  }else if( !is.character(categoria) ){
    stop("'categoria' must be a character containing a variable name", call.=F)
  }else if(length(categoria)!=1){
    stop("Length of 'categoria' must be 1", call.=F)
  }else if(forestmangr::check_names(df, categoria)==F){
    stop(forestmangr::check_names(df, categoria, boolean=F), call.=F)
  }
  
  # Se a0 nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( a0 )){
    stop( "'a0' must be numeric", call.=F)
  }else if(length(a0)!=1){
    stop("Length of 'a0' must be 1", call.=F)
  }
  
  # Se 01 nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( 01 )){
    stop( "'01' must be numeric", call.=F)
  }else if(length(01)!=1){
    stop("Length of '01' must be 1", call.=F)
  }
  
  # Se method nao for character,ou nao for de tamanho 1, parar
  if(!is.character( method )){
    stop( "'method' must be character", call.=F)
  }else if(length(method)!=1){
    stop("Length of 'method' must be 1", call.=F)
  }else if(! method %in% c('modelo', 'media') ){ 
  stop("'method' must be equal to 'modelo' ou 'media' ", call. = F) 
  }
  
  site_sym <- rlang::sym(site)
  G_sym <- rlang::sym(G)
  categoria_sym <- rlang::sym(categoria)
  
  # ####
  
  #colocar if para caso idade nao for numerica
  
  tab_site_medio <- df %>%
    dplyr::group_by(!!categoria_sym) %>% 
    dplyr::summarise(
      Site = mean(!!site_sym),
      B_MEDIO = mean(!!G_sym) )
  
  EST <- merge(tab_site_medio, data.frame(Idade = idade) ) %>% 
    dplyr::arrange(!!categoria_sym)
  ## Para se estimar a area basal utilizando a equacao do sistema
  ## de Clutter, precisa-se de um valor de area basal inicial, ja que na equacao
  ## utiliza-se de B1 e B2 nos calculos. Entao primeiro estima-se uma area basal
  ## com um modelo baseado apenas no site, e dai pra frente se estima utilizando
  ## a equacao de Clutter.
  
  ## Primeiro cria-se uma tabela com os coeficientes 
  ## para se estimar o primeiro valor de B2. Utiliza-se os dados originais:
  
  Site <- df %>% dplyr::pull(!!site_sym)
  B <- df %>% dplyr::pull(!!G_sym)
  
  
  ## O processo de estimacao sera feito utilizando um loop for.
  ## Sendo assim, o primeiro passo e criar uma lista fora do loop:
  list2 <- vector("list", length = nrow(EST))
  
  # Estimar a area basal inicial da classe 1
  # utilizando o modelo ou a media, dependendo do usuario
  if(method == "modelo"){
    
    Site_quad <- Site^2
    
    reg_B2_inicial <- stats::lm(B ~ Site + Site_quad)
    
    tab_coef_B2 <- data.frame(b0 = coef(reg_B2_inicial)[[1]],
                              b1 = coef(reg_B2_inicial)[[2]],
                              b2 = coef(reg_B2_inicial)[[3]] )
    
    
    ## Em seguida, estima-se o primeiro valor de area basal,
    ## utilizando a equcao que se baseia no site:
    list2[[1]] <- log(tab_coef_B2$b0 + 
                        tab_coef_B2$b1*EST$Site[1] + 
                        tab_coef_B2$b2*(EST$Site[1]^2) )
    
    
  }else if(method == "media"){
    
    list2[[1]] <- log( EST$B_MEDIO[1] )
    
    
  }
  ## e feito um log do resultado, pois no modelo de Clutter
  ## a variavel utilizada e Ln(B1).
  ##
  ## Agora ja e possivel estimar utilizando o modelo Clutter, dentro do loop.
  ## Existem 3 condicionais no loop: 
  ##
  ## 1. Primeiro: Inserir NA caso o proximo dado nao exista, ou quando se trocar de classe.
  ## Isso evita que os dados de classes diferentes nao sejam misturados na hora do calculo.
  ## obs(o ultimo dado de cada classe tem que ser NA, pois se necessita da idade futura para estimar a B futura)
  ##
  ## 2. A primeira area basal de cada classe deve ser calculada utilizando o modelo baseado no site,
  ## para isso utiliza-se a segunda condicao. Isso e verificado checando se a elemento anterior e NA;
  ## Se ele for NA, quer dizer que houve troca de Categoria, ou seja, este e o primeiro dado da Categoria, portanto,
  ## deve-se fazer o calculo com esse modelo.
  ## Caso contrario, ou seja, se os dados forem da mesma categoria, realizar o calculo
  ## utilizando o modelo de Clutter para Area Basal.
  
  categ <-EST %>% dplyr::pull(!!categoria_sym)
  
  for(i in 2:nrow(EST)){
    
    
    if(is.na(categ[i+1]) | categ[i] != categ[i+1] ){
      
      list2[[i]]  <- NA
      
      
    }else if(is.na(list2[[i-1]]) ){
      
      
      # Estimar a area basal inicial de cada classe
      # utilizando o modelo ou a media, dependendo do usuario
      if(method == "modelo"){    
        
        list2[[i]] <- log(tab_coef_B2$b0 + 
                            tab_coef_B2$b1*EST$Site[i] + 
                            tab_coef_B2$b2*(EST$Site[i]^2) )
        
        
      }else if(method == "media"){
        
        list2[[i]] <- log( EST$B_MEDIO[i] )
        
      }
      
      
    } else{
      
      list2[[i]]  <- list2[[i-1]] * (EST$Idade[i] / EST$Idade[i+1] )  + 
        a0 * (1 - (EST$Idade[i] / EST$Idade[i+1]  ) ) + 
        a1 * (1 - (EST$Idade[i] / EST$Idade[i+1]  ) ) * EST$Site[i]  
      
      
      
    }
    
  }
  
  ## Agora converte-se a lista em um vetor, e salva-se o vetor como
  ## uma variavel no dataframe:
  EST$LN_B2_EST <- as.vector(do.call(rbind, list2))
  
  return(EST)
  
}
