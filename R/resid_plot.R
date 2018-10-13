#' @title 
#' Calculate residual values and create plots
#'
#' @description 
#' Function for creating plots and tables for residual values from observed and estimated values.
#' 
#' @param df A dataframe.
#' @param obs Quoted name of the observed values variable.
#' @param ... Quoted name(s) for the estimated values variable(s). Multiple variables must be separated by comma.
#' @param type Character object for the type of plot created, The available plots are: \code{"scatterplot"}, \code{"histogram"}, \code{"histogram_curve"} and \code{"versus"}. Default: \code{"scatterplot"}.
#' @param point_size Numeric value for the point size in scatterplots. Default: \code{3}.
#' @param color Quoted name of a variable. If supplied, this variable will be used to classify the data by color. Default: \code{NA}.
#' @param nrow Numeric value for number of rows in the plot matrix. If not supplied, the plots will be automatically sorted. Default: \code{NA}.
#' @param ncol Numeric value for number of columns in the plot matrix. If not supplied, the plots will be automatically sorted. Default: \code{NA}.
#' @param lim_y Numeric value for the y axis upper and lower limit. If \code{NA}, the biggest residual value is used. Default: \code{NA}.
#' @param xlab Character value for the x label used in some plots. Default: \code{"Observed values"}.
#' @param clab Character value for the color label used, if a color variable is supplied. If not supplied, the \code{color} variable name will be used. Default: \code{NA}.
#' @param font Type of font used in the plot. Default: \code{"serif"}.
#' @param legend_pos Position of legend, when a color variable is supplied. This can either be \code{"left"}, \code{"right"}, \code{"top"} or \code{"bottom"}. Default: \code{"bottom"}.
#' @param grey_scale If \code{TRUE}, the plot will be rendered in a grey scale. Default: \code{"TRUE"}.
#' @param res_table If \code{TRUE}, the function will return a dataframe with observed, estimated, and residual values. Default: \code{FALSE}.
#' @return A ggplot object, or if \code{res_table = TRUE}, a dataframe.
#' 
#' @export
#' @examples 
#' library(forestmangr)
#' data("exfm11")
#'
#' head(exfm11)
#'
#' # Specifying the observed and estimated variables, we get a scatterplot
#' # for the percentual residuals:
#' resid_plot(exfm11, "TH", "TH_EST1")
#' 
#' # It's possible to change the size of points in a scatterplot with point_size:
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", point_size=1)
#' 
#' # It's possible to get other types of plots, with the type argument:
#' resid_plot(exfm11, "TH", "TH_EST1", type = "histogram")
#' resid_plot(exfm11, "TH", "TH_EST1", type = "histogram_curve")
#' resid_plot(exfm11, "TH", "TH_EST1", type = "versus")
#' 
#' # It's possible to add a factor variable as color in the plots:
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", color="STRATA")
#'
#' # It's possible to change the color legend position: 
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", color="STRATA", legend_pos="top")
#' 
#' # A colored plot is also available:
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", color="STRATA", grey_scale=FALSE)
#' 
#' # It's possible to change xlabels and color labels:  
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", color="STRATA",xlab="Total Height (m)", clab="Strata")
#' 
#' # It's póssible to change the font of the plot. R natively supports
#' # sans, serif and mono, but these can be expanded using packages:
#' resid_plot(exfm11, "TH", "TH_EST1", type = "histogram_curve", font="sans")
#' resid_plot(exfm11, "TH", "TH_EST1", type = "histogram_curve", font="mono")
#' 
#' # If there are more estimated values variables, they can also be used
#' # in the comparisson:
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2")
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", "TH_EST3")
#' 
#' # It's possible to rearange the plots with ncol and nrow:
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", "TH_EST3", ncol=1)
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", "TH_EST3", nrow=2)
#' 
#' # It's possible to specify the y axis limit with lim_y:
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", "TH_EST3", lim_y = 80)
#' resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", "TH_EST3", lim_y = 40)
#' 
#' # It's possible to get the residuals table used to generate these plots, with res_table=TRUE:
#' head( resid_plot(exfm11, "TH", "TH_EST1", "TH_EST2", res_table = T) )
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}
resid_plot <- function (df, obs, ..., type = "scatterplot",point_size = 3,color = NA, nrow = NA, ncol = NA, 
                          lim_y = NA, xlab = "Observed values", clab=NA, font = "serif", legend_pos = "bottom", grey_scale=T, res_table = F){
  # ####
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se obs nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(obs) ){  
    stop("obs not set", call. = F) 
  }else if( !is.character(obs) ){
    stop("'obs' must be a character containing a variable name", call.=F)
  }else if(length(obs)!=1){
    stop("Length of 'obs' must be 1", call.=F)
  }else if(forestmangr::check_names(df, obs)==F){
    stop(forestmangr::check_names(df, obs, boolean=F), call.=F)
  }
  
  # Se type nao for character,ou nao for de tamanho 1, parar
  if(!is.character( type )){
    stop( "'type' must be character", call.=F)
  }else if(length(type)!=1){
    stop("Length of 'type' must be 1", call.=F)
  }else if(! type %in% c('scatterplot', 'histogram', 'histogram_curve', 'versus') ){ 
  stop("'type' must be equal to 'scatterplot', 'histogram', 'histogram_curve' or 'versus' ", call. = F) 
  }
  
  # Se point_size nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( point_size )){
    stop( "'point_size' must be numeric", call.=F)
  }else if(length(point_size)!=1){
    stop("Length of 'point_size' must be 1", call.=F)
  }else if(! point_size %in%  seq(from=0,to=10,by=0.01) ){
    stop("'point_size' must be a number between 0 and 10", call.=F)
  }
  
  if(missing(color) || is.null(color) || is.na(color) || color == "" ){
    COLOR <- NULL
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
  
  # Se nrow nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar

  
  if(missing(nrow) || is.null(nrow) || is.na(nrow) || nrow == "" ){
    nrow <- NULL
  } else if(!is.numeric( nrow )){
    stop( "'nrow' must be numeric", call.=F)
  }else if(length(nrow)!=1){
    stop("length of 'nrow' must be 0", call.=F)
  }else if(! nrow > 0 | ! nrow <= 500){
    stop("'nrow' must be a number between 0 and 500", call.=F)
  }
  
  if(missing(ncol) || is.null(ncol) || is.na(ncol) || ncol == "" ){
    ncol <- NULL
  } else if(!is.numeric( ncol )){
    stop( "'ncol' must be numeric", call.=F)
  }else if(length(ncol)!=1){
    stop("length of 'ncol' must be 0", call.=F)
  }else if(! ncol > 0 | ! ncol <= 500){
    stop("'ncol' must be a number between 0 and 500", call.=F)
  }
  
  if(missing(lim_y) || is.null(lim_y) || is.na(lim_y) || lim_y == "" ){
    lim_y <- NULL
  } else if(!is.numeric( lim_y )){
    stop( "'lim_y' must be numeric", call.=F)
  }else if(length(lim_y)!=1){
    stop("length of 'lim_y' must be 1", call.=F)
  }
  
  if(missing(xlab) || is.null(xlab) || is.na(xlab) || xlab == "" ){
    XLAB <- "Observed values" 
  } else if(!is.character( xlab )){
    stop( "'xlab' must be character", call.=F)
  }else if(length(xlab)!=1){
    stop("Length of 'xlab' must be 1", call.=F)
  }else{
    XLAB <- xlab
  }
  
  if(missing(clab) || is.null(clab) || is.na(clab) || clab == "" ){
    CLAB <- COLOR 
  } else if(!is.character( clab )){
    stop( "'clab' must be character", call.=F)
  }else if(length(clab)!=1){
    stop("Length of 'clab' must be 1", call.=F)
  }else{
    CLAB <- clab
  }
  
  # Se font nao for character,ou nao for de tamanho 1, parar
  if(!is.character( font )){
    stop( "'font' must be character", call.=F)
  }else if(length(font)!=1){
    stop("Length of 'font' must be 1", call.=F)
  }
  
  # se grey_scale nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! grey_scale %in% c(TRUE, FALSE) ){ 
    stop("'grey_scale' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(grey_scale)!=1){
    stop("Length of 'grey_scale' must be 1", call.=F)
  }
  
  
  # Se legend_pos nao for character,ou nao for de tamanho 1, parar
  if(!is.character( legend_pos )){
    stop( "'legend_pos' must be character", call.=F)
  }else if(length(legend_pos)!=1){
    stop("Length of 'legend_pos' must be 1", call.=F)
  }else if(! legend_pos %in% c('left', 'right', 'top', 'bottom') ){ 
  stop("'legend_pos' must be equal to 'left', 'right', 'top or 'bottom' ", call. = F) 
  }
  
  # se res_table nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! res_table %in% c(TRUE, FALSE) ){ 
    stop("'res_table' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(res_table)!=1){
    stop("Length of 'res_table' must be 1", call.=F)
  }
  
  # ####
  ARGS <- list(...)
  DF <- as.data.frame(df)
  OBS <- obs 
  OBSgg <- paste("`",OBS,"`",sep="") #Adiciona "`" para o comeco do nome, para caso a variavel tenha caracteres especiais
  ARGS <- list(...)
  YLAB <- "Estimated values"
  YLIM <- lim_y
 
  # se a variavel nao existir, gerar erro    
  if( !any(names(DF) == OBS) ) stop(paste("Wrong variable name. Variable '", OBS, "' not found", ".",sep=""))
  
  
  lista <- vector("list", length(ARGS) )
  
  for(i in 1:length(ARGS) ){
    
    # se a variavel nao existir, gerar erro    
    if( !any(names(DF) == ARGS[[i]] ) ) {
      
      warning(paste("Variable '", ARGS[[i]], "' not found", ".",sep=""), call. = F )
      ARGS[[i]] <- NULL
      
    }else{
      
      lista[[i]] <- data.frame(ID = names(DF[ ARGS[[i]] ]), 
                               DF[OBS],
                               DF[COLOR],
                               EST = DF[[ ARGS[[i]] ]],
                               ERROR = ((DF[[ ARGS[[i]] ]] - DF[[OBS]])/DF[[OBS]]) * 100,
                               check.names = F)
      # check names=F garante que variaveis com nomes contendo caracteres especiais nao sejam renomeadas
    }
  }
  # check names+F garante que variaveis com nomes contendo caracteres especiais nao sejam renomeadas
  df_graph <- data.frame(do.call(rbind, lista),check.names = F)
  
  lista2 <- vector("list", length(ARGS))
  for (i in 1:length(ARGS)) {
    lista2[[i]] <- df_graph[df_graph$ID == ARGS[[i]], c("EST", "ERROR")]
    names(lista2[[i]])[1] <- ARGS[[i]]
    names(lista2[[i]])[2] <- paste("ERROR", i, sep = "_")
    # names(lista2[[i]])[2] <- paste(ARGS[[i]], "ERROR", sep = "_")
  }
  
  # Se o usuario utilizar histograma, converter a cor pra fator
  if(is.null(COLOR) || is.na(COLOR) || COLOR==""){}else if(type == "histogram" | type == "histogram_curve"){
    
    df_graph[[COLOR]] <- as.factor(df_graph[[COLOR]])
  }
  
  lista2
  df_res <- cbind(DF[OBS], lista2)
  
  if (res_table) {return(df_graph)}
  
  if (is.null(YLIM)) {YLIM <- round(max(df_graph["ERROR"]), -1) + 10}
  #if( is.null(XLIM) ){ XLIM <- round(max(df_graph[OBS]), -1) + 10 }
  
  p <- ggplot2::ggplot(df_graph) + {
    if (type == "scatterplot")ggplot2::geom_point(ggplot2::aes_string(OBSgg, "ERROR", color=COLORgg), size = point_size, alpha = 0.9)
    else if (type == "histogram" | type == "histogram_curve")  ggplot2::geom_histogram(ggplot2::aes_string(x = "ERROR", y = "..density..", fill=COLORgg), color = "gray50", binwidth = 3, position = "dodge")
    else if (type == "versus") ggplot2::geom_point(ggplot2::aes_string(OBSgg, "EST", color=COLORgg), size = point_size, alpha = 0.9)
  } + {
    if (type == "histogram_curve") ggplot2::geom_density(ggplot2::aes_string("ERROR"), size = 1, color = "gray10")
  } + {
    if (type == "scatterplot") ggplot2::geom_hline(yintercept = 0, color = "gray45")
    else if (type == "histogram" | type == "histogram_curve")ggplot2::geom_vline(xintercept = 0, linetype="dashed",color = "gray45")
    else if (type == "versus") ggplot2::geom_smooth(ggplot2::aes_string(OBSgg, "EST"), color = "gray45", method = "lm", se = F)
  } + {
    if (type == "scatterplot") ggplot2::scale_y_continuous(breaks = seq(-YLIM, YLIM, 20), limits = c(-YLIM, YLIM))
    else if (type == "histogram" | type == "histogram_curve") ggplot2::scale_x_continuous(breaks = seq(-YLIM, YLIM, 20), limits = c(-YLIM, YLIM))
  } + {
    if (type == "scatterplot") ggplot2::labs(x = XLAB, y = "Residuals (%)", color = CLAB)
    else if (type == "histogram" | type == "histogram_curve") ggplot2::labs(x = "Residuals (%)", y = "Relative Density", fill = CLAB)
    else if (type == "versus") ggplot2::labs(x = XLAB, y = YLAB, color = CLAB)
  } + {
    if(is.null(COLOR)){
      
    }else if(grey_scale==FALSE){
      
    }else if(is.numeric(DF[[COLOR]]) )ggplot2::scale_colour_gradient(low = "light gray", high = "gray20")
    else( ggplot2::scale_colour_grey(start = 0.8, end = 0.2) )
    
  }  +{
    if(grey_scale) ggplot2::scale_fill_grey(start = 0.8, end = 0.2) 
  }+
    ggthemes::theme_igray(base_family = font) +
    ggplot2::theme(
      legend.position = legend_pos,
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(), panel.border = ggplot2::element_blank(), 
      axis.title = ggplot2::element_text(size = 24, face="bold"), 
      axis.text = ggplot2::element_text(size = 22), 
      axis.line.x = ggplot2::element_line(color = "black"), 
      axis.line.y = ggplot2::element_line(color = "black"), 
      strip.text.x = ggplot2::element_text(size = 18, face = "bold"),
      legend.text = ggplot2::element_text(size=20), 
      legend.title = ggplot2::element_text(size=20) ) + 
    ggplot2::guides(
      color=ggplot2::guide_legend(nrow=1,byrow=TRUE),
      fill=ggplot2::guide_legend(nrow=1,byrow=TRUE)) 
  
  if (length(ARGS) > 1) { p <- p + ggplot2::facet_wrap(~ID, nrow = nrow, ncol = ncol) }
  return(p)
}

