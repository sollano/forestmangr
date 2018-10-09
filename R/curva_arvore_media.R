#' @export

curva_arvore_media <- function(df, d, dap, h, ht, facet){
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
  
  # se d nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(d) ){  
    stop("d not set", call. = F) 
  }else if( !is.character(d) ){
    stop("'d' must be a character containing a variable name", call.=F)
  }else if(length(d)!=1){
    stop("Length of 'd' must be 1", call.=F)
  }else if(forestr::check_names(df, d)==F){
    stop(forestr::check_names(df, d, boolean=F), call.=F)
  }
  
  # se dap nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(dap) ){  
    stop("dap not set", call. = F) 
  }else if( !is.character(dap) ){
    stop("'dap' must be a character containing a variable name", call.=F)
  }else if(length(dap)!=1){
    stop("Length of 'dap' must be 1", call.=F)
  }else if(forestr::check_names(df, dap)==F){
    stop(forestr::check_names(df, dap, boolean=F), call.=F)
  }
  
  # se h nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(h) ){  
    stop("h not set", call. = F) 
  }else if( !is.character(h) ){
    stop("'h' must be a character containing a variable name", call.=F)
  }else if(length(h)!=1){
    stop("Length of 'h' must be 1", call.=F)
  }else if(forestr::check_names(df, h)==F){
    stop(forestr::check_names(df, h, boolean=F), call.=F)
  }
  
  # se ht nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(ht) ){  
    stop("ht not set", call. = F) 
  }else if( !is.character(ht) ){
    stop("'ht' must be a character containing a variable name", call.=F)
  }else if(length(ht)!=1){
    stop("Length of 'ht' must be 1", call.=F)
  }else if(forestr::check_names(df, ht)==F){
    stop(forestr::check_names(df, ht, boolean=F), call.=F)
  }
  
  # Se facet nao for fornecido, criar objeto que dplyr::group_by ignora, sem causar erro
  if(missing(facet)||is.null(facet)||is.na(facet)||facet==F||facet==""){
    facet <-  NULL
  }else if(!is.character(facet)){
    stop("facet must be a character", call. = F)
  }else if(! length(facet)%in% 1:10){
    stop("Length of 'facet' must be between 1 and 10", call.=F)
  }else if(forestr::check_names(df,facet)==F){
    stop(forestr::check_names(df,facet, boolean=F), call.=F)
  }
  
  d_sym <- rlang::sym(d)
  dap_sym <- rlang::sym(dap)
  h_sym <- rlang::sym(h)
  ht_sym <- rlang::sym(ht)
  
  # se facet nao for fornecido, for igual "", nulo, ou  nao existir no dataframe, definir como nulo
  if(  missing(facet) || is.null(facet) || is.na(facet) || facet == "" || is.null(df[facet]) ){  
    facet <- NULL
  }
  
 # ####
  
 p <- df %>% 
    dplyr::mutate(d_sob_dap = (!!d_sym)/(!!dap_sym),
           h_sob_ht = (!!h_sym)/(!!ht_sym), 
           h_sob_ht_quad = h_sob_ht^2 ) %>% 
    ggplot2::ggplot(ggplot2::aes(x=d_sob_dap, y=h_sob_ht)) + 
    ggplot2::geom_point(size = 2, alpha = .4) + 
    # coord_fixed(ratio=2) +
    ggplot2::labs(x=expression(italic(frac(d,DAP))), 
         y=expression(italic(frac(h,HT)))
    ) +
    ggpmisc::stat_poly_eq(
      formula = x ~ stats::poly(y, 2, raw=T),
      size = 3,
      eq.x.rhs    = "italic(frac(h,HT))",
      eq.with.lhs = "italic(hat(frac(d,DAP)))~`=`~", 
      ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
      label.x.npc="right",
      parse = TRUE  ) +
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
