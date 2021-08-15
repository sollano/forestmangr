#' @title 
#' Estimate future and present basal area, volume, TCA, CMI and MMI values of the Clutter Growth and Yield Model
#' @description 
#' This function estimates the present the present value of basal area for each class
#' using either the class mean, or a linear quadratic model, and then uses it's value to calculate
#' the basal area from Clutter's growth and yield model.
#'
#' @param df A data frame.
#' @param age A numeric vector with the desired age range to be used in the estimation, or a Quoted name for the age variable.
#' @param basal_area Quoted name for the basal area variable.
#' @param site Quoted name for the average site variable.
#' @param category Quoted name for the category variable.
#' @param coeffs Numeric vector or a data frame with the fitted values of Clutter's growth and yield model. It must be a named vector, with b0,b1,b2,b3,a0 and a1 as names. a1 is not obligatory.
#' @param method Method used for estimating the present basal area of each class. It can either be the class' average basal area \code{"average"}, or an estimated value from a linear quadratic model of site as a function of basal area \code{"model"}. Default: \code{"average"}.
#' @param annual_increment If \code{TRUE}, changes the labels from Mean Monthly Increment (MMI) and Current Monthly Increment (CMI) to Mean Annual Increment (MAI) and Current Annual Increment (CAI). Default \code{FALSE}.
#' @param gray_scale If \code{TRUE}, the plot will be rendered in a gray scale. Default: \code{"TRUE"}.
#' @param output Type of output the function should return. This can either be \code{"plot"}, for the estimation plots, \code{"table"}, for a data frame with the estimated values, and \code{"full"} for a list with the plot and 2 more data frames. \code{"table"}.
#' @return A data frame, a ggplot object or a list, according to output.
#' @seealso other sampling functions: 
#'   \code{\link{fit_clutter}} for fitting the clutter growth and Yield model, and
#'   \code{\link{classify_site}} for classifying data according to site.
#' @export
#' 
#' @examples
#' 
#' library(forestmangr)
#' data("exfm17")
#' head(exfm17)
#' 
#' clutter <- fit_clutter(exfm17, "age", "DH", "B", "V", "S", "plot")
#' clutter
#' 
#' # Classify data into 3 classes:
#' ex_class <- classify_site(exfm17, "S", 3, "plot")
#' head(ex_class ,15)
#' 
#' # Estimate basal area using the average basal area as the initial basal area,
#' # volume,  Mean Monthly Increment (MMI) and Current Monthly Increment (CMI)
#' # values using Clutter's model:
#' est_clutter(ex_class,20:125, "B","S","category_",clutter,"average")
#' 
#' # For a more detailed output, including a plot, use output="full":
#' est_clutter(ex_class,20:125, "B","S","category_",clutter, output="full")
#'
#' # Estimate basal area using an estimated basal area as the initial basal area:
#' est_clutter(ex_class,20:125,"B","S","category_",clutter,"model") 
#' 
#' # age can be a variable:
#' est_clutter(ex_class,"age","B","S","category_", clutter,"model")  
#'   
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' @importFrom ggplot2 ggplot after_stat
#' 
est_clutter <- function(df, age, basal_area, site, category, coeffs, method = "average", annual_increment=FALSE, gray_scale=TRUE, output="table"){
  # ####
  LN_B2_EST<-Age<-V2_EST<-CMI<-MMI<-category_<-CAI<-MAI<-TAC<-TAC_Y<-Index<-Value<-NULL
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
  }else if(is.numeric(age)){
    age_name <- "Age"
    age_sym <- rlang::sym("Age")
  }else if( is.character(age)){
    if(forestmangr::check_names(df, age)==F)  stop(forestmangr::check_names(df, age, boolean=F), call.=F)
    age_name <- age
    age_sym <- rlang::sym(age)
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
  
  # Se coeffs nao for numerico, nao for de tamanho 3, ou nao estiver dentro dos padroes, parar
  if(!length(coeffs)%in% c(5,6)){
    stop("Length of 'coeffs' must be 5 or 6", call.=F)
  }else if(! is.vector(coeffs) & ! is.data.frame(coeffs) ){
    stop("'coeffs' must be a named vector or data frame following the model 'c(b0=a,b1=b,b2=c,b3=d,a0=e,a1=f) ", call.=F)
  }else if(length(coeffs)==5){
    if(all(names(coeffs) !=c("b0","b1","b2","b3","a0")) ){
      stop("'coeffs' must be a named vector or data frame following the pattern 'c(b0=a,b1=b,b2=c,b3=d,a0=e) ", call.=F)
    }
    # add a1 as zero, if the dataframe doesn't have it.
    coeffs$a1 <- 0
    
  }else if(length(coeffs)==6){
    if(all(names(coeffs) !=c("b0","b1","b2","b3","a0","a1")) ){
      stop("'coeffs' must be a named vector or data frame following the pattern 'c(b0=a,b1=b,b2=c,b3=d,a0=e,a1=f) ", call.=F)
    }
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
  
  # Se method nao for character,ou nao for de tamanho 1, parar
  if(!is.character( method )){
    stop( "'method' must be character", call.=F)
  }else if(length(method)!=1){
    stop("Length of 'method' must be 1", call.=F)
  }else if(! method %in% c('model', 'average') ){ 
    stop("'method' must be equal to 'model' ou 'average' ", call. = F) 
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c("plot", "table", "table_plot", "full") ){ 
    stop("'output' must be equal to 'plot', 'table', 'table_plot' or 'full' ", call. = F) 
  }
  
  site_sym <- rlang::sym(site)
  basal_area_sym <- rlang::sym(basal_area)
  category_sym <- rlang::sym(category)
  
  # se annual_increment nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! annual_increment %in% c(TRUE, FALSE) ){ 
    stop("'annual_increment' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(annual_increment)!=1){
    stop("Length of 'annual_increment' must be 1", call.=F)
  }
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
    
    tab_coef_B2 <- data.frame(b0 = stats::coef(reg_B2_inicial)[[1]],
                              b1 = stats::coef(reg_B2_inicial)[[2]],
                              b2 = stats::coef(reg_B2_inicial)[[3]] )
    
    
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
        coeffs$a0 * (1 - (EST$Age[i] / EST$Age[i+1]  ) ) + 
        coeffs$a1 * (1 - (EST$Age[i] / EST$Age[i+1]  ) ) * EST$Site[i]  
      
      
      
    }
    
  }
  
  ## Agora converte-se a lista em um vetor, e salva-se o vetor como
  ## uma variavel no dataframe:
  EST$LN_B2_EST <- as.vector(do.call(rbind, list2))
  #EST$B2_EST <- exp(EST$LN_B2_EST)
  #return(EST)

  final_table <- EST %>% 
    dplyr::group_by(!!category_sym) %>% 
    dplyr::mutate(
      B2_EST = exp(LN_B2_EST),
      V2_EST = exp(coeffs$b0 + 
                     (coeffs$b1 * 1 / Age) + 
                     coeffs$b2 * Site + 
                     coeffs$b3 * LN_B2_EST  ),
      CMI = abs(V2_EST - dplyr::lag(V2_EST) ),
      MMI = V2_EST/ Age,
      CMI_MMI = CMI - MMI) 
  
  itc_table <- final_table  %>% 
    dplyr::group_by(!!category_sym) %>% 
    dplyr::filter(round(CMI,1) == round(MMI,1) ) %>% 
    dplyr::summarise( 
      TAC = mean(Age, na.rm=T),
      TAC_Y = mean(MMI, na.rm=T)       )  
  
  final_table <- final_table %>%  dplyr::rename(!!age_name := Age)
  
  if(annual_increment){
    
    final_table <- final_table %>% dplyr::rename(CAI=CMI,MAI=MMI)
    
    suppressMessages(
      plot_table <- dplyr::left_join(final_table,itc_table ) %>% 
        stats::na.omit() %>% 
        dplyr::select(Category=category_, CAI, MAI, age_name, TAC, TAC_Y) %>% 
        tidyr::gather(Index,Value, CAI, MAI) )
  }else if(annual_increment==FALSE){
    
    suppressMessages(
      plot_table <- dplyr::left_join(final_table,itc_table ) %>% 
        stats::na.omit() %>% 
        dplyr::select(Category=category_, CMI, MMI, age_name, TAC, TAC_Y) %>% 
        tidyr::gather(Index,Value, CMI, MMI) )
  }
  
  
  
  grafico <- ggplot2::ggplot(plot_table, ggplot2::aes_string( x = age_name, color = "Index" ) ) + 
    ggplot2::facet_wrap(~Category) + 
    ggplot2::geom_line(ggplot2::aes_string(y = "Value"), size = 1.5) +
    ggplot2::geom_point(ggplot2::aes_string(x = "TAC", y = "TAC_Y"), color = "black")+
    ggplot2::geom_text(ggplot2::aes_string(x = "TAC", y = "TAC_Y", label = "TAC"),
                       vjust = 0, 
                       nudge_y = 0.2, 
                       color = "black", size = 5) +
    ggplot2::labs(y = "Production") +
    {
      if(gray_scale) ggplot2::scale_color_grey(start = 0.6, end = 0.2)
    } +
    ggthemes::theme_igray(base_family = "serif") +
    ggplot2::theme(legend.position = "bottom",
                   legend.title = ggplot2::element_text(size=12,face="bold"),
                   legend.text = ggplot2::element_text(size=12),
                   panel.grid.major = ggplot2::element_blank(), 
                   panel.grid.minor = ggplot2::element_blank(),
                   panel.border = ggplot2::element_blank(),
                   axis.title   = ggplot2::element_text(size = 17), 
                   axis.text    = ggplot2::element_text(size = 15),
                   axis.line.x = ggplot2::element_line(color="black"),
                   axis.line.y = ggplot2::element_line(color="black"),
                   strip.text.x = ggplot2::element_text(size = 19) )
  
  
  if(output=="table"){
    return(final_table)
  }else if(output=="plot"){
    return(grafico)
  }else if(output == "table_plot"){
    
    print(grafico)
    return(final_table)
    
  }else if(output=="full"){
    return(list(itc_plot=grafico,plot_table=plot_table,tac_summary=itc_table,final_table = final_table))
  }
  
}
