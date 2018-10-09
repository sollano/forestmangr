#' @export
residuos_exp <- function (df, obs, ..., type = "scatterplot",point_size = 3,color = NULL, nrow = NULL,ncol = NULL, 
                          lim_y = NULL, xlab = NULL, clab=NULL, font = "serif",legend_pos = "bottom",res_table = F){
  DF <- as.data.frame(df)
  OBS <- obs 
  OBSgg <- paste("`",OBS,"`",sep="") #Adiciona "`" para o comeco do nome, para caso a variavel tenha caracteres especiais
  COLOR <- color
  if(is.null(COLOR)||is.na(COLOR)||COLOR==""){COLOR <- NULL;COLORgg <- NULL}else(COLORgg <- paste("`",COLOR,"`",sep="")) #Adiciona "`" para o comeco do nome, para caso a variavel tenha caracteres especiais
  ARGS <- list(...)
  XLAB <- xlab
  if (is.null(XLAB)) {XLAB <- "Valor observado"  }
  CLAB <- clab
  if(is.null(CLAB)){CLAB <- COLOR}
  # XLIM <- lim_x
  YLAB <- "Valor estimado"
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
                               ERRO = ((DF[[ ARGS[[i]] ]] - DF[[OBS]])/DF[[OBS]]) * 100,
                               check.names = F)
      # check names=F garante que variaveis com nomes contendo caracteres especiais nao sejam renomeadas
    }
  }
  # check names+F garante que variaveis com nomes contendo caracteres especiais nao sejam renomeadas
  df_graph <- data.frame(do.call(rbind, lista),check.names = F)
  
  lista2 <- vector("list", length(ARGS))
  for (i in 1:length(ARGS)) {
    lista2[[i]] <- df_graph[df_graph$ID == ARGS[[i]], c("EST", "ERRO")]
    names(lista2[[i]])[1] <- ARGS[[i]]
    names(lista2[[i]])[2] <- paste("ERRO", i, sep = "_")
    # names(lista2[[i]])[2] <- paste(ARGS[[i]], "ERRO", sep = "_")
  }
  
  # Se o usuario utilizar histograma, converter a cor pra fator
  if(is.null(COLOR) || is.na(COLOR) || COLOR==""){}else if(type == "histogram" | type == "histogram_curve"){
    
    df_graph[[COLOR]] <- as.factor(df_graph[[COLOR]])
  }
  
  lista2
  df_res <- cbind(DF[OBS], lista2)
  
  if (res_table) {return(df_graph)}
  
  if (is.null(YLIM)) {YLIM <- round(max(df_graph["ERRO"]), -1) + 10}
  #if( is.null(XLIM) ){ XLIM <- round(max(df_graph[OBS]), -1) + 10 }
  
  p <- ggplot2::ggplot(df_graph) + {
    if (type == "scatterplot")ggplot2::geom_point(ggplot2::aes_string(OBSgg, "ERRO", color=COLORgg), size = point_size, alpha = 0.9)
    else if (type == "histogram" | type == "histogram_curve")  ggplot2::geom_histogram(ggplot2::aes_string(x = "ERRO", y = "..density..", fill=COLORgg), color = "gray50", binwidth = 3, position = "dodge")
    else if (type == "versus") ggplot2::geom_point(ggplot2::aes_string(OBSgg, "EST", color=COLORgg), size = point_size, alpha = 0.9)
  } + {
    if (type == "histogram_curve") ggplot2::geom_density(ggplot2::aes_string("ERRO"), size = 1, color = "gray10")
  } + {
    if (type == "scatterplot") ggplot2::geom_hline(yintercept = 0, color = "gray45")
    else if (type == "histogram" | type == "histogram_curve")ggplot2::geom_vline(xintercept = 0, linetype="dashed",color = "gray45")
    else if (type == "versus") ggplot2::geom_smooth(ggplot2::aes_string(OBSgg, "EST"), color = "gray45", method = "lm", se = F)
  } + {
    if (type == "scatterplot") ggplot2::scale_y_continuous(breaks = seq(-YLIM, YLIM, 20), limits = c(-YLIM, YLIM))
    else if (type == "histogram" | type == "histogram_curve") ggplot2::scale_x_continuous(breaks = seq(-YLIM, YLIM, 20), limits = c(-YLIM, YLIM))
  } + {
    if (type == "scatterplot") ggplot2::labs(x = XLAB, y = "Residuo (%)", color = CLAB)
    else if (type == "histogram" | type == "histogram_curve") ggplot2::labs(x = "Residuo (%)", y = "Densidade relativa", fill = CLAB)
    else if (type == "versus") ggplot2::labs(x = XLAB, y = YLAB, color = CLAB)
  } + {
    if(is.null(COLOR)){
    }else if(is.numeric(DF[[COLOR]]) )ggplot2::scale_colour_gradient(low = "light gray", high = "gray20")
    else( ggplot2::scale_colour_grey(start = 0.8, end = 0.2) )
    
  }  + ggplot2::scale_fill_grey(start = 0.8, end = 0.2) +
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
