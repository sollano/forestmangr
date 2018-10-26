#' @title 
#' Fit the Clutter model for growth and yield
#' 
#' @description 
#' Fit the Clutter model for growth and yield using the two stage least squares method (2SLS).
#'
#' @param df A data frame.
#' @param age Quoted name for the age variable.
#' @param dh Quoted name for the dominant height variable.
#' @param basal_area Quoted name for the basal area variable.
#' @param volume Quoted name for the volume area variable.
#' @param site Quoted name for the site variable.
#' @param plot Quoted name for the plot variable.
#' @param .groups Optional argument. Quoted name(s) of grouping variables used to fit multiple regressions, one for each level of the provided variable(s). Default \code{NA}.
#' @param model Character variable for the type of the model fitted. If \code{"full"}, the full model will be used. if \code{"mod"}, a modified model will be fitted, where the X3 variable is excluded from the regression. Default: \code{full}.
#' @param keep_model If \code{TRUE} a variable with the regression model will be kept in the data frame. Default: \code{FALSE}.
#'
#' @return A data frame with the regression coefficients.
#'  
#' @seealso other sampling functions: 
#'   \code{\link{est_clutter}} for estimating the Clutter growth and yield model variables, and
#'   \code{\link{classify_site}} for classifying data according to site.
#'   
#' @keywords Clutter, 2SLS
#' @references 
#' Clutter, J. L. (1963) Compatible Growth For Loblolly by the Southeastern, Forest Science, 9(3), pp. 354–371.
#' Sullivan, A. D. and Clutter, J. L. (1972) A Simultaneous Growth and Yield for Loblolly Pine, Forest Science, 18(1), pp. 76–86.
#' Campos, J. C. C. and Leite, H. G. (2017) Mensuracao Florestal: Perguntas e Respostas. 5a. Vicosa: UFV.
#' 
#' @export
#' @examples 
#' 
#' library(forestmangr)
#' data("exfm17")
#' exfm17
#' 
#' # To fit the Clutter model we just need to define the data, and age, dominant height,
#' # basal area, volume, site and plot variables:
#' fit_clutter(exfm17, "age", "DH", "B", "V", "S", "plot")
#' 
#' # To fit the alternate model (without a1) just use model="mod":
#' fit_clutter(exfm17, "age", "DH", "B", "V", "S", "plot",model="mod")
#' 
#' # To keep the regression model, use keep_model=TRUE:
#' fit_clutter(exfm17, "age", "DH", "B", "V", "S", "plot", keep_model=TRUE) 
#'              
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#'
fit_clutter <- function(df, age, dh, basal_area, volume, site, plot, .groups=NA, model = "full", keep_model = FALSE){
  # ####
  basal_area2<-basal_area1<-I1<-I2<-volume2<-.<-Reg<-NULL
  # checagem de variaveis ####

  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
    # se age nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(age) ){  
      stop("age not set", call. = F) 
    }else if( !is.character(age) ){
      stop("'age' must be a character containing a variable name", call.=F)
    }else if(length(age)!=1){
      stop("Length of 'age' must be 1", call.=F)
    }else if(forestmangr::check_names(df, age)==F){
      stop(forestmangr::check_names(df, age, boolean=F), call.=F)
    }
    
    # se dh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(dh) ){  
      stop("dh not set", call. = F) 
    }else if( !is.character(dh) ){
      stop("'dh' must be a character containing a variable name", call.=F)
    }else if(length(dh)!=1){
      stop("Length of 'dh' must be 1", call.=F)
    }else if(forestmangr::check_names(df, dh)==F){
      stop(forestmangr::check_names(df, dh, boolean=F), call.=F)
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
    
    # se volume nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(volume) ){  
      stop("volume not set", call. = F) 
    }else if( !is.character(volume) ){
      stop("'volume' must be a character containing a variable name", call.=F)
    }else if(length(volume)!=1){
      stop("Length of 'volume' must be 1", call.=F)
    }else if(forestmangr::check_names(df, volume)==F){
      stop(forestmangr::check_names(df, volume, boolean=F), call.=F)
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
    
    # sitee plot nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
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
    if(missing(.groups)||is.null(.groups)||is.na(.groups)||.groups==F||.groups==""){
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
    
  
  # Se model nao for character,ou nao for de tamanho 1, parar
  if(!is.character( model )){
    stop( "'model' must be character", call.=F)
  }else if(length(model)!=1){
    stop("Length of 'model' must be 1", call.=F)
  }else if(! model %in% c('full', 'mod') ){ 
  stop("'model' must be equal to 'full' or 'mod' ", call. = F) 
  }

  # se keep_model nao for igual a TRUE ou FALSE
  if(! keep_model %in% c(TRUE, FALSE) ){ 
    stop("keep_model must be equal to TRUE or FALSE", call. = F) 
  }
  
  age_sym <- rlang::sym(age)
  dh_sym <- rlang::sym(dh)
  basal_area_sym <- rlang::sym(basal_area)
  volume_sym <- rlang::sym(volume)
  site_sym <- rlang::sym(site)
  
  # ####
    
    suppressMessages(
      
      struct_form_data <- df %>% 
        dplyr::group_by(!!!.groups_syms, !!!plot_syms, add=T ) %>% 
        dplyr::transmute(
          I1 = !!age_sym, I2  = dplyr::lead(!!age_sym), 
          dh = !!dh_sym, dh2 = dplyr::lead(!!dh_sym), 
          basal_area1 = !!basal_area_sym,  basal_area2  = dplyr::lead(!!basal_area_sym), 
          volume1 = !!volume_sym,  volume2  = dplyr::lead(!!volume_sym),
          site  = !!site_sym   ) %>% 
        stats::na.omit() %>% 
        dplyr::mutate(
          Y1 = log(basal_area2)          ,
          X1 = log(basal_area1) * (I1/I2),
          X2 = 1 - I1/I2        ,
          X3 = (1 - I1/I2) * site  ,
          Y2 = log(volume2)          ,
          X4 = 1 / I2           ,
          X5 = site
        ) %>% 
        dplyr::ungroup()
      
    )
    
  
  
  if(model == "full"){
    
    eq1 <- Y2 ~ X4 + X5 + Y1
    eq2 <- Y1 ~ X1 + X2 + X3
    system <- list(Volume = eq1, AreaBasal = eq2)
    
    inst <- ~ X4 + X5 + X1 + X2 + X3
    
    restrict <- matrix(0, nrow=2, ncol=8)
    
    restrict[1,5] <- 1
    restrict[2,6] <- 1
    
    restrict.rhs <- c(0, 1)
    
    model_fit <-  struct_form_data %>%   
      dplyr::group_by( !!!.groups_syms, add=T ) %>% 
      dplyr::do(Reg = systemfit::systemfit(system, "2SLS", inst = inst, data = ., 
                                           restrict.matrix = restrict, 
                                           restrict.rhs = restrict.rhs)) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(
        b0 = stats::coef(Reg)[[1]],
        b1 = stats::coef(Reg)[[2]], 
        b2 = stats::coef(Reg)[[3]],
        b3 = stats::coef(Reg)[[4]],
        a0 = stats::coef(Reg)[[7]],
        a1 = stats::coef(Reg)[[8]] ) %>% 
      dplyr:: ungroup()
    
    
  }else if(model == "mod" ){
    
    eq1 <- Y2 ~ X4 + X5 + Y1
    eq2 <- Y1 ~ X1 + X2
    system <- list(Volume = eq1, AreaBasal = eq2)
    
    inst <- ~ X4 + X5 + X1 + X2
    
    restrict <- matrix(0, nrow=2, ncol=7)
    
    restrict[1,5] <- 1
    restrict[2,6] <- 1
    
    restrict.rhs <- c(0, 1)
    
    model_fit <-  struct_form_data %>%   
      dplyr::group_by( !!!.groups_syms, add=T ) %>% 
      dplyr::do(Reg = systemfit::systemfit(system, "2SLS", inst = inst, data = ., 
                                           restrict.matrix = restrict, 
                                           restrict.rhs = restrict.rhs)) %>% 
      dplyr::rowwise() %>% 
      dplyr::mutate(
        b0 = stats::coef(Reg)[[1]],
        b1 = stats::coef(Reg)[[2]], 
        b2 = stats::coef(Reg)[[3]],
        b3 = stats::coef(Reg)[[4]],
        a0 = stats::coef(Reg)[[7]] ) %>% 
      dplyr::ungroup()
    
  } 
  
  if(keep_model == F){ 
    model_fit <- as.data.frame(model_fit)
    model_fit$Reg <- NULL
    }
  model_fit$A <- NULL
  
  return(model_fit)
  
  
}
