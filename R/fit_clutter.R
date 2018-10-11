#' @title 
#' Ajuste do Modelo de Clutter (MQ2O)
#' 
#' @description 
#' Ajuste do Modelo de Clutter para Crescimento e Producao pelo metodo dos Minimos Quadrados em 2 Estagios.
#' 
#' @details 
#' 
#' @param df Dataframe a ser utilizado.
#' @param Id Nome com ou sem aspas da variavel Idade.
#' @param HD Nome com ou sem aspas da variavel Altura Dominante.
#' @param B Nome com ou sem aspas da variavel Area Seccional.
#' @param V Nome com ou sem aspas da variavel Volume.
#' @param S Nome com ou sem aspas da variavel Site.
#' @param .groups Nome com ou sem aspas de uma ou mais variáveis classificatorias,  utilizadas para criacao da forma estrutural e, caso haja mais de uma, para ajuste do modelo por grupo. Pelo menos uma deve ser inserida, referente a Parcela.
#' @param struct_form_df Caso seja verdadeira, o dataframe inserido sera considerado como um dado ja padronizado para a forma estrutural. Este deve conter as variaveis Y1, X1, X2, X3, Y2, Y4 E Y5. Padrao: \code{F}.
#' @param model Selecao do tipo de modelo ajustado. Caso seja igual 1, ajusta-se o modelo completo. Caso seja igual a 2, ajusta-se o modelo retirando-se X3 do ajuste. Padrao: \code{1}.
#' @param keep_model Indica se a variavel contendo o ajuste deve ser mantida ou nao. Padrao \code{FALSE}.
#'
#' @return  Dataframe contendo os coeficientes do ajuste, e uma coluna contendo o ajuste em si.
#' 
#' @keywords Clutter, MQ20, 2SLS
#' @references 
#' CLUTTER, J. L. Compatible Growth For Loblolly by the Southeastern. Forest Science, v. 9, n. 3, p. 354–371, 1963. 
#' SULLIVAN, A. D.; CLUTTER, J. L. A Simultaneous Growth and Yield for Loblolly Pine. Forest Science, v. 18, n. 1, p. 76–86, 1972. 
#' CAMPOS, J. C. C.; LEITE, H. G. Mensuracao florestal: perguntas e respostas. 3a. ed. Vicosa: Editora UFV, 2013. 605 p.
#' 
#' @export
#' @examples 
#' 
#' library(forestmangr)
#' library(dplyr)
#' data("ex16_mfr")
#' data("ex17_mfr")
#' 
#' # Primeiro iremos estimar o Site utilizando para a idade indice de 64 meses:
#' Idade_I <- 64
#' 
#' dados <- ex16_mfr %>% 
#'   lm_table(log(HD) ~ inv(Idade), output = "merge") %>% 
#'   mutate(S = exp(log(HD) - b1 * (1/Idade - 1/Idade_I))  ) %>% 
#'   select(-b0, -b1, -Rsqr, -Rsqr_adj, -Std.Error)
#' dados 
#' 
#' # Agora para rodar o modelo de Clutter basta inserir os nomes das variáveis
#' # Idade, altura dominante, area basal, volume, site e parcela:
#' coefs_clutter <- fit_clutter(dados, "Idade", "HD", "B", "V", "S", "Parcela")
#' coefs_clutter
#' 
#' # Caso os seus dados ja estejam no formato estrutural, basta utilizar
#' # o argumento struct_form_df para realizar o ajuste:
#' ex17_mfr
#' 
#' coefs_clutter <- fit_clutter(ex17_mfr, struct_form_df = T)
#' coefs_clutter
#' # obs: Neste caso, a nomemclatura deve ser respeitada
#'              
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}


fit_clutter <- function(df, Id, HD, B, V, S, .groups, struct_form_df = F, model = 1, keep_model = F){
  # checagem de variaveis ####

  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se struct_form_df nao for igual a TRUE ou FALSE
  if(!   struct_form_df %in% c(TRUE, FALSE) ){ 
    stop("struct_form_df must be equal to TRUE or FALSE", call. = F) 
  }
  
  # Se struct_form_df nao for verdadeiro, testar os nomes das variaveis
  if(struct_form_df == F){
    
    # se Id nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(Id) ){  
      stop("Id not set", call. = F) 
    }else if( !is.character(Id) ){
      stop("'Id' must be a character containing a variable name", call.=F)
    }else if(length(Id)!=1){
      stop("Length of 'Id' must be 1", call.=F)
    }else if(forestmangr::check_names(df, Id)==F){
      stop(forestmangr::check_names(df, Id, boolean=F), call.=F)
    }
    
    # se HD nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
    if(  missing(HD) ){  
      stop("HD not set", call. = F) 
    }else if( !is.character(HD) ){
      stop("'HD' must be a character containing a variable name", call.=F)
    }else if(length(HD)!=1){
      stop("Length of 'HD' must be 1", call.=F)
    }else if(forestmangr::check_names(df, HD)==F){
      stop(forestmangr::check_names(df, HD, boolean=F), call.=F)
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
      # Parar se algum nome nao existir, e avisar qual nome nao existe
      stop(forestmangr::check_names(df,.groups, boolean=F), call.=F) 
      # se os grupos forem fornecidos e forem nomes dos dados
      # Transformar o objeto em simbolo, para que dplyr entenda
      # e procure o nome das variaveis dentro dos objetos
    }else{
      .groups_syms <- rlang::syms(.groups) 
    }
    
    Id_sym <- rlang::sym(Id)
    HD_sym <- rlang::sym(HD)
    B_sym <- rlang::sym(B)
    V_sym <- rlang::sym(V)
    S_sym <- rlang::sym(S)
    
  }
  
  # Se model nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( model )){
    stop( "'model' must be numeric", call.=F)
  }else if(length(model)!=1){
    stop("Length of 'model' must be 1", call.=F)
  }else if(! model %in%  c(1, 2) ){
    stop("'model' must be equal to 1 or 2", call.=F)
  }
  
  # se keep_model nao for igual a TRUE ou FALSE
  if(! keep_model %in% c(TRUE, FALSE) ){ 
    stop("keep_model must be equal to TRUE or FALSE", call. = F) 
  }
  
  # ####
  
  if(struct_form_df == T){
    
    struct_form_data <- df
    
  }else if(struct_form_df == F){
    
    suppressMessages(
      
      struct_form_data <- df %>% 
        dplyr::group_by( !!!.groups_syms, add=T ) %>% 
        dplyr::transmute(
          I1 = !!Id_sym, I2  = dplyr::lead(!!Id_sym), 
          HD = !!HD_sym, HD2 = dplyr::lead(!!HD_sym), 
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
    
  }
  
  if(model == 1){
    
    eq1 <- Y2 ~ X4 + X5 + Y1
    eq2 <- Y1 ~ X1 + X2 + X3
    system <- list(Volume = eq1, AreaBasal = eq2)
    
    inst <- ~ X4 + X5 + X1 + X2 + X3
    
    restrict <- matrix(0, nrow=2, ncol=8)
    
    restrict[1,5] <- 1
    restrict[2,6] <- 1
    
    restrict.rhs <- c(0, 1)
    
    model_fit <-  struct_form_data %>%   
      dplyr::group_by( !!!(.groups_syms[-length(.groups)]), add=T ) %>% 
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
    
    
  }else if(model == 2 ){
    
    eq1 <- Y2 ~ X4 + X5 + Y1
    eq2 <- Y1 ~ X1 + X2
    system <- list(Volume = eq1, AreaBasal = eq2)
    
    inst <- ~ X4 + X5 + X1 + X2
    
    restrict <- matrix(0, nrow=2, ncol=7)
    
    restrict[1,5] <- 1
    restrict[2,6] <- 1
    
    restrict.rhs <- c(0, 1)
    
    model_fit <-  struct_form_data %>%   
      dplyr::group_by( !!!(.groups_syms[-length(.groups)]), add=T ) %>% 
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
