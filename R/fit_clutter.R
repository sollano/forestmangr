#' @title 
#' Fit Clutter's model for growth and yield
#' 
#' @description 
#' Fit clutter's model for growth and yield using the two stage least squares method (2SLS).
#'
#' @param df A dataframe.
#' @param age Quoted name for the age variable.
#' @param DH Quoted name fof the dominant height variable.
#' @param B Quoted name for the basal area variable.
#' @param V Quoted name for the volume area variable.
#' @param S Quoted name for the site variable.
#' @param plot Quoted name for the plot variable.
#' @param .groups Optional argument. Quoted name(s) of grouping variables used to fit multiple regressions, one for each level of the provided variable(s). Default \code{NA}.
#' @param model Character variable for the type of the model fitted. If \code{"full"}, the full model will be used. if \code{"mod"}, a modified model will be fitted, weere the X3 variable is excluded from the regression. Default: \code{full}.
#' @param keep_model If \code{TRUE} a variable with the regression model will be kept in the dataframe. Default: \code{FALSE}.
#'
#' @return  Dataframe contendo os coeficientes do ajuste, e uma coluna contendo o ajuste em si.
#' 
#' @keywords Clutter,  2SLS
#' @references 
#' Clutter, J. L. (1963) ‘Compatible Growth For Loblolly by the Southeastern’, Forest Science, 9(3), pp. 354–371.
#' Sullivan, A. D. and Clutter, J. L. (1972) ‘A Simultaneous Growth and Yield for Loblolly Pine’, Forest Science, 18(1), pp. 76–86.
#' Campos, J. C. C. and Leite, H. G. (2017) Mensuração Florestal: Perguntas e Respostas. 5a. Viçosa: UFV.
#' 
#' @export
#' @examples 
#' 
#' library(forestmangr)
#' data("exfm17")
#' 
#' head(exfm17)
#' 
#' # To fit Clutter's model we just need to define the data, and age, dominant height,
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
fit_clutter <- function(df, age, DH, B, V, S, plot, .groups=NA, model = "full", keep_model = F){
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
    
    # se DH nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(DH) ){  
      stop("DH not set", call. = F) 
    }else if( !is.character(DH) ){
      stop("'DH' must be a character containing a variable name", call.=F)
    }else if(length(DH)!=1){
      stop("Length of 'DH' must be 1", call.=F)
    }else if(forestmangr::check_names(df, DH)==F){
      stop(forestmangr::check_names(df, DH, boolean=F), call.=F)
    }
    
    # se B nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(B) ){  
      stop("B not set", call. = F) 
    }else if( !is.character(B) ){
      stop("'B' must be a character containing a variable name", call.=F)
    }else if(length(B)!=1){
      stop("Length of 'B' must be 1", call.=F)
    }else if(forestmangr::check_names(df, B)==F){
      stop(forestmangr::check_names(df, B, boolean=F), call.=F)
    }
    
    # se V nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(V) ){  
      stop("V not set", call. = F) 
    }else if( !is.character(V) ){
      stop("'V' must be a character containing a variable name", call.=F)
    }else if(length(V)!=1){
      stop("Length of 'V' must be 1", call.=F)
    }else if(forestmangr::check_names(df, V)==F){
      stop(forestmangr::check_names(df, V, boolean=F), call.=F)
    }
    
    # se S nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(S) ){  
      stop("S not set", call. = F) 
    }else if( !is.character(S) ){
      stop("'S' must be a character containing a variable name", call.=F)
    }else if(length(S)!=1){
      stop("Length of 'S' must be 1", call.=F)
    }else if(forestmangr::check_names(df, S)==F){
      stop(forestmangr::check_names(df, S, boolean=F), call.=F)
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
  DH_sym <- rlang::sym(DH)
  B_sym <- rlang::sym(B)
  V_sym <- rlang::sym(V)
  S_sym <- rlang::sym(S)
  
  # ####
    
    suppressMessages(
      
      struct_form_data <- df %>% 
        dplyr::group_by(!!!.groups_syms, !!!plot_syms, add=T ) %>% 
        dplyr::transmute(
          I1 = !!age_sym, I2  = dplyr::lead(!!age_sym), 
          DH = !!DH_sym, DH2 = dplyr::lead(!!DH_sym), 
          B1 = !!B_sym,  B2  = dplyr::lead(!!B_sym), 
          V1 = !!V_sym,  V2  = dplyr::lead(!!V_sym),
          S  = !!S_sym   ) %>% 
        stats::na.omit() %>% 
        dplyr::mutate(
          Y1 = log(B2)          ,
          X1 = log(B1) * (I1/I2),
          X2 = 1 - I1/I2        ,
          X3 = (1 - I1/I2) * S  ,
          Y2 = log(V2)          ,
          X4 = 1 / I2           ,
          X5 = S
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
  
  if(keep_model == F){ model_fit$Reg <- NULL }
  model_fit$A <- NULL
  
  return(model_fit)
  
  
}
