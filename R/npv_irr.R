#' @title 
#' Calculate Net Present Value and other economic variables
#' @description 
#' Get the net present value, internal rate of return, and other 
#' economic variables, given cost and revenue values.
#' 
#' @param df A data frame.
#' @param year Quoted name of the year variable.
#' @param cost Quoted name of the costs variable.
#' @param revenue Quoted name of the revenue variable.
#' @param rate Numeric value of the yearly rate to be used.
#' @param output Selects different output options. It can be either \code{"horizontal"} for a data frame with a single observation and one column for each variable, or \code{vertical} for a two column data frame with one observation for each calculated variable. Default: \code{"vertical"}.
#' @param big_mark Selects thousands separator. Can be either \code{"."}, \code{" "} or \code{","}. Default: \code{,}.
#' @param dec_mark Selects decimal separator. Can be either \code{","} or \code{"."}. Default: \code{.}.
#'
#' @export
#' @examples 
#' library(forestmangr)
#' 
#' data(exfm22)
#' 
#' npv_irr(exfm22,"year","cost","revenue",rate=8.75)
#' 
#' npv_irr(exfm22,"year","cost","revenue",rate=8.75, "horizontal")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

npv_irr <- function(df, year, cost, revenue, rate,output="vertical", big_mark=",",dec_mark="."){
  # ####
  . <- VoCT_total <- VoRT_total <- VnCT_total <- VnRT_total <- n <- IRR <- Value <- Variable <- NULL
  # ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se year nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(year) ){  
    stop("year not set", call. = F) 
  }else if( !is.character(year) ){
    stop("'year' must be a character containing a variable name", call.=F)
  }else if(length(year)!=1){
    stop("Length of 'year' must be 1", call.=F)
  }else if(forestmangr::check_names(df, year)==F){
    stop(forestmangr::check_names(df, year, boolean=F), call.=F)
  }
  
  # se cost nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(cost) ){  
    stop("cost not set", call. = F) 
  }else if( !is.character(cost) ){
    stop("'cost' must be a character containing a variable name", call.=F)
  }else if(length(cost)!=1){
    stop("Length of 'cost' must be 1", call.=F)
  }else if(forestmangr::check_names(df, cost)==F){
    stop(forestmangr::check_names(df, cost, boolean=F), call.=F)
  }
  
  # se revenue nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(revenue) ){  
    stop("revenue not set", call. = F) 
  }else if( !is.character(revenue) ){
    stop("'revenue' must be a character containing a variable name", call.=F)
  }else if(length(revenue)!=1){
    stop("Length of 'revenue' must be 1", call.=F)
  }else if(forestmangr::check_names(df, revenue)==F){
    stop(forestmangr::check_names(df, revenue, boolean=F), call.=F)
  }
  
  # Se rate nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( rate )){
    stop( "'rate' must be numeric", call.=F)
  }else if(length(rate)!=1){
    stop("length of 'rate' must be 1", call.=F)
  }else if(! rate > 0 | ! rate <= 1000){
    stop("'rate' must be a number between 0 and 1000", call.=F)
  }
  
  # Se output nao for character,ou nao for de tamanho 1, parar
  if(!is.character( output )){
    stop( "'output' must be character", call.=F)
  }else if(length(output)!=1){
    stop("Length of 'output' must be 1", call.=F)
  }else if(! output %in% c('horizontal', 'vertical') ){ 
    stop("'output' must be equal to 'horizontal' or 'vertical' ", call. = F) 
  }
  
  # Se big_mark nao for character,ou nao for de tamanho 1, parar
  if(!is.character( big_mark )){
    stop( "'big_mark' must be character", call.=F)
  }else if(length(big_mark)!=1){
    stop("Length of 'big_mark' must be 1", call.=F)
  }else if(! big_mark %in% c('.', ',', " ") ){ 
    stop("'big_mark' must be equal to '.', ',' or ' ' ", call. = F) 
  }
  
  # Se dec_mark nao for character,ou nao for de tamanho 1, parar
  if(!is.character( dec_mark )){
    stop( "'dec_mark' must be character", call.=F)
  }else if(length(dec_mark)!=1){
    stop("Length of 'dec_mark' must be 1", call.=F)
  }else if(! dec_mark %in% c('.', ',') ){ 
    stop("'dec_mark' must be equal to '.' or ',' ", call. = F) 
  }
  
  # ####
  year_name <- year
  year_sym <- rlang::sym(year)
  cost_sym <- rlang::sym(cost)
  revenue_sym <- rlang::sym(revenue)
  
  df[is.na(df)] <- 0
  
  tab1 <- df %>% 
    dplyr::mutate(
      !!year_name := as.integer(!!year_sym), # converter pra integer, pra diferenciar dos outros numeros, que sao  double
      saldo = (!!revenue_sym) - (!!cost_sym),
      VoCT = (!!cost_sym)/(1+rate/100)^(!!year_sym),
      VoRT = (!!revenue_sym)/(1+rate/100)^(!!year_sym), 
      VnCT = (!!cost_sym)*(1+rate/100)^(max(!!year_sym,na.rm=TRUE) - (!!year_sym) ),
      VnRT = (!!revenue_sym)*(1+rate/100)^(max(!!year_sym,na.rm=TRUE) - (!!year_sym) )) %>% 
    dplyr::mutate_if(is.double,formattable::comma,big.mark=big_mark,decimal.mark=dec_mark) # formatar todos os numeros que sao double
  
  tab1
  
  # Calcula tir
  irr <- FinCal::irr2(as.numeric(tab1$saldo))
  
  
  tab2 <- tab1 %>%
    dplyr::summarise_all(dplyr::funs(total=sum(.,na.rm=TRUE))) %>% 
    dplyr::mutate(n=max(df[[year_name]]),
                  #rate = formattable::percent(rate/100),
                  !!paste(year_name,"total",sep="_"):=NULL,
                  NPV  = VoRT_total - VoCT_total, # Net Present Value
                  NFV  = VnRT_total - VnCT_total, # Net Future Value
                  CB   = VoRT_total / VoCT_total, # Cost benefit Ratio
                  ELV  = (VnRT_total - VnCT_total) / ((1+rate/100)^n - 1 ), # Expected Land Value
                  YNPV = ((VnRT_total - VnCT_total)*rate/100) / ((1+rate/100)^n - 1 ), # Yearly Net Present Value
                  IRR  = formattable::percent(irr,decimal.mark=dec_mark)  ) #Internal Rate of Return
  
  if(output=="horizontal"){
    
    return(dplyr::select(tab2, -dplyr::contains("total"), -n) )
    
  }else if(output=="vertical"){
    suppressWarnings(
      
      tab2_mod <- tab2 %>% 
        dplyr::mutate_at(
          dplyr::vars(
            #rate,
            IRR),~.*100) %>% 
        dplyr::select(-dplyr::contains("total"),
                      #-rate,
                      -n) %>% 
        tidyr::gather("Variable", "Value") %>% 
        dplyr::mutate_at(
          dplyr::vars(Value),
          formattable::comma,big.mark=big_mark,decimal.mark=dec_mark )
    )
    
    tab2_mod$Variable <- c("Net Present Value (NPV)",
                           "Net Future Value (NFV)",
                           "Cost benefit Ratio (CB)",
                           "Expected Land Value (ELV)",
                           "Yearly Net Present Value (YNPV)",
                           "Internal Rate of Return (IRR)")
    
    return(tab2_mod)
    
  }
  
  
}

