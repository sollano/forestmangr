#' @title 
#' Estimate future and present basal area values of Clutter's Growth and Yield Model
#' @description 
#' This function estimates the present the present value of basal area for each class
#' using either the class' mean, or a linear quadratic model, and then uses it's value to calculate
#' the basal area from clutter's growth and yield model.
#'
#' @param df A dataframe.
#' @param age A numeric vector with the desired age range to be used in the estimation, or a Quoted name for the age variable.
#' @param basal_area Quoted name for the basal area variable.
#' @param site Quoted name for the average site variable.
#' @param category Quoted name for the category variable.
#' @param a0 Numeric value for the a0 coefficient from Clutter's growth and yield model.
#' @param a1 Numeric value for the a1 coefficient from Clutter's growth and yield model.
#' @param method Method used for estimating the present basal area of each class. It can either be the class' average basal area \code{"average"}, or an estimated value from a linear quadratic model of site as a function of basal area \code{"model"}. Default: \code{"average"}.
#' @return A dataframe with the estimated values of basal area.
#' @export
#' 
#' @examples
#' 
#' library(forestmangr)
#' data("exfm17")
#' 
#' head(exfm17)
#' 
#' clutter <- fit_clutter(exfm17, "age", "DH", "B", "V", "S", "plot")
#' clutter
#' 
#' # Classify data into 3 classes:
#' ex_class <- classify_site(exfm17, "S", 3, "plot")
#' head(ex_class ,15)
#' 
#' # Estimate basal area using the average basal area as the initial basal area:
#' ex_est<-est_B2_clutter(ex_class,20:125, "B","S","category_",clutter$a0,clutter$a1,"average")
#' head(ex_est, 15)
#'
#' # Estimate basal area using an estimated basal area as the initial basal area:
#' ex_est<-est_B2_clutter(ex_class,20:125,"B","S","category_",clutter$a0,clutter$a1,"model") 
#' head(ex_est, 15)
#' 
#' #' Estimate basal area using an estimated basal area as the initial basal area:
#' ex_est<-est_B2_clutter(ex_class,"age","B","S","category_", clutter$a0,clutter$a1,"model") 
#' head(ex_est, 15)
#' 
#'   
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#'
est_B2_clutter <- function(df, age, basal_area, site, category, a0, a1, method = "average" ){
  # checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  if(  missing(age) ){  
    stop("age not set", call. = F) 
  }else if( !is.numeric(age) & !is.character(age) ){
    stop("'age' must be a numeric vector or a variable name1", call.=F)
  }else if(is.numeric(age) & !is.vector(age)){
    stop("'age' must be a numeric vector or a variable name2", call.=F)
  }else if(is.character(age) & length(age)!=1){
    stop("'age' must be a numeric vector or a variable name3", call.=F)
  }else if( is.character(age)){
    if(forestmangr::check_names(df, age)==F)  stop(forestmangr::check_names(df, age, boolean=F), call.=F)
    age <- df[[age]]
  }
  
  # se basal_area nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(basal_area) ){  
    stop("basal_area not set", call. = F) 
  }else if( !is.character(basal_area) ){
    stop("'basal_area' must be a character containing a variable name", call.=F)
  }else if(length(basal_area)!=1){
    stop("Length of 'basal_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, basal_area)==F){
    stop(forestmangr::check_names(df, basal_area, boolean=F), call.=F)
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
  

   
  # se category nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(category) || is.null(category) || is.na(category) || category == "" ){  
    #stop("category not set", call. = F)
    df $ category <- "cat"
    category <- "category"
  }else if( !is.character(category) ){
    stop("'category' must be a character containing a variable name", call.=F)
  }else if(length(category)!=1){
    stop("Length of 'category' must be 1", call.=F)
  }else if(forestmangr::check_names(df, category)==F){
    stop(forestmangr::check_names(df, category, boolean=F), call.=F)
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
  }else if(! method %in% c('model', 'average') ){ 
  stop("'method' must be equal to 'model' ou 'average' ", call. = F) 
  }
  
  
  site_sym <- rlang::sym(site)
  basal_area_sym <- rlang::sym(basal_area)
  category_sym <- rlang::sym(category)
  
  # ####

  tab_site_medio <- df %>%
    dplyr::group_by(!!category_sym) %>% 
    dplyr::summarise(
      Site = mean(!!site_sym),
      G_mean = mean(!!basal_area_sym) )
  
  EST <- merge(tab_site_medio, data.frame(Age = age) ) %>% 
    dplyr::arrange(!!category_sym)
  ## Para se estimar a area basal utilizando a equacao do sistema
  ## de Clutter, precisa-se de um valor de area basal inicial, ja que na equacao
  ## utiliza-se de B1 e B2 nos calculos. Entao primeiro estima-se uma area basal
  ## com um model baseado apenas no site, e dai pra frente se estima utilizando
  ## a equacao de Clutter.
  
  ## Primeiro cria-se uma tabela com os coeficientes 
  ## para se estimar o primeiro valor de B2. Utiliza-se os dados originais:
  
  Site <- df %>% dplyr::pull(!!site_sym)
  B <- df %>% dplyr::pull(!!basal_area_sym)
  
  
  ## O processo de estimacao sera feito utilizando um loop for.
  ## Sendo assim, o primeiro passo e criar uma lista fora do loop:
  list2 <- vector("list", length = nrow(EST))
  
  # Estimar a area basal inicial da classe 1
  # utilizando o model ou a average, dependendo do usuario
  if(method == "model"){
    
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
    
    
  }else if(method == "average"){
    
    list2[[1]] <- log( EST$G_mean[1] )
    
    
  }
  ## e feito um log do resultado, pois no modelo de Clutter
  ## a variavel utilizada e Ln(B1).
  ##
  ## Agora ja e possivel estimar utilizando o modelo Clutter, dentro do loop.
  ## Existem 3 condicionais no loop: 
  ##
  ## 1. Primeiro: Inserir NA caso o proximo dado nao exista, ou quando se trocar de classe.
  ## Isso evita que os dados de classes diferentes nao sejam misturados na hora do calculo.
  ## obs(o ultimo dado de cada classe tem que ser NA, pois se necessita da age futura para estimar a B futura)
  ##
  ## 2. A primeira area basal de cada classe deve ser calculada utilizando o modelo baseado no site,
  ## para isso utiliza-se a segunda condicao. Isso e verificado checando se a elemento anterior e NA;
  ## Se ele for NA, quer dizer que houve troca de Categoria, ou seja, este e o primeiro dado da Categoria, portanto,
  ## deve-se fazer o calculo com esse modelo.
  ## Caso contrario, ou seja, se os dados forem da mesma category, realizar o calculo
  ## utilizando o modelo de Clutter para Area Basal.
  
  categ <-EST %>% dplyr::pull(!!category_sym)
  
  for(i in 2:nrow(EST)){
    
    
    if(is.na(categ[i+1]) | categ[i] != categ[i+1] ){
      
      list2[[i]]  <- NA
      
      
    }else if(is.na(list2[[i-1]]) ){
      
      
      # Estimar a area basal inicial de cada classe
      # utilizando o model ou a average, dependendo do usuario
      if(method == "model"){    
        
        list2[[i]] <- log(tab_coef_B2$b0 + 
                            tab_coef_B2$b1*EST$Site[i] + 
                            tab_coef_B2$b2*(EST$Site[i]^2) )
        
        
      }else if(method == "average"){
        
        list2[[i]] <- log( EST$G_mean[i] )
        
      }
      
      
    } else{
      
      list2[[i]]  <- list2[[i-1]] * (EST$Age[i] / EST$Age[i+1] )  + 
        a0 * (1 - (EST$Age[i] / EST$Age[i+1]  ) ) + 
        a1 * (1 - (EST$Age[i] / EST$Age[i+1]  ) ) * EST$Site[i]  
      
      
      
    }
    
  }
  
  ## Agora converte-se a lista em um vetor, e salva-se o vetor como
  ## uma variavel no dataframe:
  EST$LN_B2_EST <- as.vector(do.call(rbind, list2))
  EST$B2_EST <- exp(EST$LN_B2_EST)
  return(EST)
  
}
