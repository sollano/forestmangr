#' @title
#' Generate the curve of a forest's average tree using the Kozak taper model
#' @description 
#' Generate a ggplot curve of a forest's average tree using the Kozak taper model (Kozak, Munro and Smith, 1969).
#'  
#' @param df A data frame.
#' @param d Quoted name of the section diameter variable, in cm.
#' @param dbh Quoted name of the diameter at breast height variable, in cm.
#' @param h Quoted name of the section height variable, in meters.
#' @param th Quoted name of the total height variable, in meters.
#' @param facet Optional argument. If supplied with the Quoted name of a factor variable(s), this variable is used to divide the plot into facets. Default: \code{NA}.
#' @param color Quoted name of a variable. If supplied, this variable will be used to classify the data by color. Default: \code{NA}.
#' @param mirror if \code{TRUE}, the plot will be mirrored, to resemble the shape of a tree. Default: \code{TRUE}
#' @param eq if \code{TRUE}, Kozak's taper model is adjusted and the equation is shown on the plot. Default \code{TRUE}
#' 
#' @return A ggplot object.
#' 
#' @references 
#' Kozak, A., Munro, D. D. and Smith, J. H. G. (1969) Taper Functions and their Application in Forest Inventory, The Forestry Chronicle, 45, pp. 278–283.
#' 
#' @export
#' @examples 
#' library(forestmangr)
#' data("exfm7")
#' head(exfm7)
#' 
#' # Get the data's average tree curve inserting the section diameter and height, total height
#' # and dbh variables: 
#' average_tree_curve(df = exfm7, d = "di_wb", dbh = "DBH", h = "hi", th = "TH")
#' 
#' # It's possible to get the average tree curve of each strata with the facet argument,
#' # and divide the data by color with the color argument:
#' average_tree_curve(df=exfm7,d="di_wb",dbh="DBH",h="hi",th="TH",facet="STRATA",color="GENCODE")
#'
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
#' 
#' @importFrom ggplot2 stage
average_tree_curve <- function(df, d, dbh, h, th, facet=NA,color=NA,mirror=TRUE,eq=TRUE){
  # ####
  ..rr.label..<-..eq.label..<-d_sob_dbh<-h_sob_th <- d_sob_dbh_negative <- d_sob_dbh_positive <-NULL
  # checagem de variaveis ####
  
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se d nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(d) ){  
    stop("d not set", call. = F) 
  }else if( !is.character(d) ){
    stop("'d' must be a character containing a variable name", call.=F)
  }else if(length(d)!=1){
    stop("Length of 'd' must be 1", call.=F)
  }else if(forestmangr::check_names(df, d)==F){
    stop(forestmangr::check_names(df, d, boolean=F), call.=F)
  }
  
  # se dbh nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dbh) ){  
    stop("dbh not set", call. = F) 
  }else if( !is.character(dbh) ){
    stop("'dbh' must be a character containing a variable name", call.=F)
  }else if(length(dbh)!=1){
    stop("Length of 'dbh' must be 1", call.=F)
  }else if(forestmangr::check_names(df, dbh)==F){
    stop(forestmangr::check_names(df, dbh, boolean=F), call.=F)
  }
  
  # se h nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(h) ){  
    stop("h not set", call. = F) 
  }else if( !is.character(h) ){
    stop("'h' must be a character containing a variable name", call.=F)
  }else if(length(h)!=1){
    stop("Length of 'h' must be 1", call.=F)
  }else if(forestmangr::check_names(df, h)==F){
    stop(forestmangr::check_names(df, h, boolean=F), call.=F)
  }
  
  # se th nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(th) ){  
    stop("th not set", call. = F) 
  }else if( !is.character(th) ){
    stop("'th' must be a character containing a variable name", call.=F)
  }else if(length(th)!=1){
    stop("Length of 'th' must be 1", call.=F)
  }else if(forestmangr::check_names(df, th)==F){
    stop(forestmangr::check_names(df, th, boolean=F), call.=F)
  }
  
  # Se facet nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(facet)||is.null(facet)||is.na(facet)||facet==F||facet==""){
    facet <-  NULL
  }else if(!is.character(facet)){
    stop("facet must be a character", call. = F)
  }else if(! length(facet)%in% 1:10){
    stop("Length of 'facet' must be between 1 and 10", call.=F)
  }else if(forestmangr::check_names(df,facet)==F){
    stop(forestmangr::check_names(df,facet, boolean=F), call.=F)
  }
  
  # se mirror nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! mirror %in% c(TRUE, FALSE) ){ 
    stop("'mirror' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(mirror)!=1){
    stop("Length of 'mirror' must be 1", call.=F)
  }
  
  # se eq nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! eq %in% c(TRUE, FALSE) ){ 
    stop("'eq' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(eq)!=1){
    stop("Length of 'eq' must be 1", call.=F)
  }
  
  if(missing(color) || is.null(color) || is.na(color) || color == "" ){
    df$COLOR <- NULL
    COLORgg <- NULL
  }else if(!is.character(color)){
    stop("'color' must be a character containing a variable name", call.=F)
  }else if(length(color)!=1){
    stop("Length of 'color' must be 1", call.=F)
  }else if(forestmangr::check_names(df, color)==F){
    stop(forestmangr::check_names(df, color, boolean=F), call.=F)
  }else{
    COLOR <- color
    COLORgg <- paste("`",COLOR,"`",sep="")
  }
  
  
  d_sym <- rlang::sym(d)
  dbh_sym <- rlang::sym(dbh)
  h_sym <- rlang::sym(h)
  th_sym <- rlang::sym(th)
  
  # ####
  
  df_plot <- df %>% 
    dplyr::mutate(d_sob_dbh = (!!d_sym)/(!!dbh_sym),
                  h_sob_th = (!!h_sym)/(!!th_sym), 
                  h_sob_th_quad = h_sob_th^2 )
  
  if(mirror==TRUE){
    
    df_plot <- df_plot %>% 
      dplyr::rename(d_sob_dbh_positive = d_sob_dbh) %>% 
      dplyr::mutate(d_sob_dbh_negative = d_sob_dbh_positive-d_sob_dbh_positive*2) %>% 
      tidyr::gather(mirror,d_sob_dbh,d_sob_dbh_positive, d_sob_dbh_negative)
    
  }else{
    df_plot$mirror <- "d_sob_dbh_positive"
  }
  
  
  p <- ggplot2::ggplot(df_plot, ggplot2::aes_string(x="d_sob_dbh", y="h_sob_th",color=COLORgg)) + 
    ggplot2::geom_point(size = 2, alpha = .4) + 
    # coord_fixed(ratio=2) +
    ggplot2::labs(x=expression(italic(frac(d,DBH))), 
                  y=expression(italic(frac(h,TH)))
    ) + {
      if(eq==TRUE)
        
        ggpmisc::stat_poly_eq(data=df_plot[df_plot$mirror=="d_sob_dbh_positive",],
                              formula = x ~ stats::poly(y, 2, raw=T),
                              size = 3,
                              eq.x.rhs    = "italic(frac(h,TH))",
                              eq.with.lhs = "italic(hat(frac(d,DBH)))~`=`~", 
                              ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
                              label.x.npc="right",
                              parse = TRUE  )
      
    } + {
      if(mirror==TRUE) ggplot2::geom_vline(xintercept=0)
    } + 
    ggthemes::theme_igray(base_family = "serif") +
    ggplot2::theme(
      axis.title.y     = ggplot2::element_text(angle = 0, vjust =.5),
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(),
      panel.border     = ggplot2::element_blank(),
      axis.title       = ggplot2::element_text(size = 14,face="bold"), 
      axis.text        = ggplot2::element_text(size = 14),
      axis.line.x      = ggplot2::element_line(color="black"),
      axis.line.y      = ggplot2::element_line(color="black"),
      strip.text.x     = ggplot2::element_text(size = 14)   )
  
  if(!is.null(facet) ){p <- p + ggplot2::facet_wrap(facet) }
  
  return(p)  
}
