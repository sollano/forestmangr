#' Graficos de Residuo
#'
#' Funcao para se criar graficos e tabelas de residuo a partir de dados observados e estimados.
#' 
#' @param df Data frame a ser utilizado.
#' @param obs nome entre aspas da variavel observada.
#' @param ... nome(s) entre aspas da(s) variavel(s) estimada(s). Multiplas variaveis devem ser separadas por virgula.
#' @param type tipo de grafico a ser gerado. Pode ser "scatterplot", "histogram", "histogram_curve", "versus". Padrao: \code{"scatterplot"}
#' @param lim_y Limite para o eixo y. Por padrao, este e definido automaticamente, mas pode ser ajustado caso necessario. Entra-se com um valor numero. Padrao: \code{NULL}.
#' @param res_table Caso verdadeiro, sera gerado um dataframe com os dados originais e seus respectivos erros. Padrao: \code{FALSE}.
#' @return objeto ggplot, ou, caso \code{res_table = TRUE}, um dataframe contendo os dados originais e os residuos gerados.
#' 
#' @export
#' @examples 
#' library(forestr)
#' data("ex11_mfr")
#'
#' # Especificando-se apenas as variaveis observadas e estimadas,
#' # gera-se um grafico de dispersao:
#' residuos(ex11_mfr, "HT", "HT_EST1")
#' residuos(ex11_mfr, "HT", "HT_EST1", type = "scatterplot")
#' 
#' # Utilizando o argumento type, pode se criar outros tipos de grafico:
#' residuos(ex11_mfr, "HT", "HT_EST1", type = "histogram")
#' residuos(ex11_mfr, "HT", "HT_EST1", type = "histogram_curve")
#' residuos(ex11_mfr, "HT", "HT_EST1", type = "versus")
#' 
#' # Pode-se utilizar duas ou mais variáveis na comparaçao:
#' residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", type = "scatterplot")
#' residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", type = "histogram")
#' residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", type = "histogram_curve")
#' residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", type = "versus")
#' 
#' residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", "HT_EST3")
#' 
#' #  Pode-se especificar os limites do eixo y com o argumento lim_y:
#' residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", "HT_EST3", lim_y = 80)
#' residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", "HT_EST3", lim_y = 40)
#' 
#' # E possivel gerar a tabela de resíduos com o argumento res_table:
#' head( residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", res_table = T) )
#' 
#' @author Sollano Rabelo Braga \email{sollanorb@@gmail.com}

residuos <- function(df, obs, ..., type = "scatterplot", nrow = NULL, ncol = NULL, lim_y = NULL, xlab = NULL, font = "serif",res_table=F){

  DF <- as.data.frame(df)
  OBS <- obs 
  OBSgg <- paste("`",OBS,"`",sep="") #Adiciona "`" para o comeco do nome, para caso a variavel tenha caracteres especiais
  ARGS <- list(...)
  XLAB <- xlab
  if(is.null(XLAB) ){ XLAB <- "Valor observado" }
  YLAB <- "Valor estimado"
 # XLIM <- lim_x
  YLIM <- lim_y
  
  # se a variavel nao existir, gerar erro    
  if( !any(names(DF) == OBS) ) stop(paste("Wrong variable name. Variable '", OBS, "' not found", ".",sep=""))
  
  
  lista <- vector("list", length(ARGS) )
  
  for(i in 1:length(ARGS) ){
    
    # se a variavel nao existir, gerar erro    
    if( !any(names(DF) == ARGS[[i]] ) ) stop(paste("Wrong variable name. Variable '", ARGS[[i]], "' not found", ".",sep=""))
    
        
    lista[[i]] <- data.frame(ID = names(DF[ ARGS[[i]] ]), 
                             DF[OBS],
                             EST = DF[[ ARGS[[i]] ]],
                             ERRO = ((DF[[ ARGS[[i]] ]] - DF[[OBS]])/DF[[OBS]]) * 100,
                             check.names = F)
                              # check names garante que variaveis com nomes contendo caracteres especiais nao sejam renomeadas
    
  }
  # check names garante que variaveis com nomes contendo caracteres especiais nao sejam renomeadas
  df_graph <- data.frame(do.call(rbind, lista),check.names = F)
  
  lista2 <- vector("list", length(ARGS) )
  for(i in 1:length(ARGS) ){
    
    lista2[[i]] <- df_graph[df_graph$ID==ARGS[[i]], c("EST", "ERRO")  ]
    
    names(lista2[[i]])[1] <- ARGS[[i]]
     names(lista2[[i]])[2] <- paste("ERRO", i, sep = "_")
   # names(lista2[[i]])[2] <- paste(ARGS[[i]], "ERRO", sep = "_")
  }
  
  lista2
  df_res <- cbind(DF[OBS], lista2)
  
  
  if(res_table){return(df_res)}
  
  if( is.null(YLIM) ){ YLIM <- round(max(df_graph["ERRO"]), -1) + 10 }
  #if( is.null(XLIM) ){ XLIM <- round(max(df_graph[OBS]), -1) + 10 }
  
  p <- ggplot2::ggplot(df_graph) +  {
    
    if(type == "scatterplot") ggplot2::geom_point(ggplot2::aes_string(OBSgg, "ERRO"), size=2.5, alpha = .9) 
    else if(type == "histogram" | type == "histogram_curve") ggplot2::geom_histogram(ggplot2::aes_string(x = "ERRO", y = "..density.."), color = "gray50", position = "identity",  binwidth = 1) 
    else if(type == "versus") ggplot2::geom_point(ggplot2::aes_string(OBSgg, "EST"), size=2.5, alpha = .9) 
  } + {
    
    if(type == "histogram_curve") ggplot2::geom_density(ggplot2::aes_string("ERRO"), size = 1, color = "gray10" )
    
  } + {
    
    if(type == "scatterplot") ggplot2::geom_hline(yintercept = 0, color = "gray45") 
    else if(type == "histogram" | type == "histogram_curve") ggplot2::geom_vline(xintercept = 0, linetype="dashed", color = "gray45") 
    else if(type == "versus") ggplot2::geom_smooth(ggplot2::aes_string(OBSgg, "EST"), color = "gray45", method = "lm", se=F) 
    
    
    
  } + {
    if(type == "scatterplot") ggplot2::scale_y_continuous(breaks = seq( -YLIM, YLIM, 20), limits = c( - YLIM, YLIM) ) 
    else if(type == "histogram" | type == "histogram_curve") ggplot2::scale_x_continuous(breaks = seq( -YLIM, YLIM, 20), limits = c( - YLIM, YLIM) ) 
    #else if(type == "versus") ggplot2::scale_x_continuous(breaks = seq( 0, XLIM, 10), limits = c( 0, XLIM) ) 
    
  } + {
    if(type == "scatterplot") ggplot2::labs(x = XLAB, y = "Residuo (%)")
    else if(type == "histogram" | type == "histogram_curve") ggplot2::labs(x = "Residuo (%)", y = "Densidade relativa")
    else if(type == "versus") ggplot2::labs(x = XLAB, y = YLAB  )
    
  } + 
    ggthemes::theme_igray(base_family = font) +
    
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(), 
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.title   = ggplot2::element_text(size = 22, face = "bold"), 
      axis.text    = ggplot2::element_text(size = 17),
      axis.line.x = ggplot2::element_line(color="black"),
      axis.line.y = ggplot2::element_line(color="black"),
      strip.text.x = ggplot2::element_text(size = 19)   )  
  
  if(length(ARGS) > 1){p <- p + ggplot2::facet_wrap(~ID, nrow = nrow, ncol = ncol) }
  
  return(p)
  
}
