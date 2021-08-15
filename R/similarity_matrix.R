#' @title
#' Get the similarity matrix of an area
#' @description 
#' Calculates the Jaccard similarity index and Sorensen similarity index.
#' @param df A data frame.
#' @param species Quoted name of the scientific names variable, or any variable used to differentiate the different species found in data. If supplied, will be used to classify the species in the diameter data.
#' @param comparison Quoted name of the variable containing levels to be compared with each other.
#' @param NI_label Label used for Species not identified. This parameter works along with species. The level supplied here will not be considered in the classification. Default \code{""}.
#' @param index Character value for the desired index to be used. Can be either \code{"Jaccard"}, for a matrix based on the Jaccard index of similarity, \code{"Sorensen"}, for a matrix based the Sorensen index of similarity, or \code{"all"}, for a list with matrices for both indexes. Default: \code{"Sorensen"}.
#' @param dendrogram If \code{TRUE}, a dendrogram will also be created. Default: \code{FALSE}.
#' @param n_groups Number of groups in the dendrogram. Default \code{3}.
#' @return a matrix object with a similarity matrix, or a list, according to the \code{"index"} and \code{"dendrogram"} arguments.
#'
#' @references 
#' Souza, A. L. and Soares, C. P. B. (2013) Florestas Nativas: estrutura, dinamica e manejo. Vicosa: UFV.
#' 
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data("exfm20")
#' head(exfm20)
#' 
#' # To get the similarity matrix of an area, we simply need to provide
#' # the species variable name, and a subdivision variable name, like
#' # transect. By default we get a a matrix based on the Sorensen index:
#' similarity_matrix(exfm20, "scientific.name", "transect")
#' 
#' # To get the similarity matrix of Jaccard, use the index argument:
#' similarity_matrix(exfm20, "scientific.name", "transect", index = "Jaccard")
#' 
#' # To get a dendrogram with the matrix, use dendrogram=TRUE:
#' similarity_matrix(exfm20, "scientific.name", "transect", index = "Jaccard", dendrogram = TRUE)
#' 
#' # To get a list with both matrices, use index="all":
#' similarity_matrix(exfm20, "scientific.name", "transect", index = "all")
#' 
#' # If the data supplied only has 2 levels, a paired comparison is made instead:
#' ex_pair <- exfm20[exfm20$transect %in% c("T01", "T02") , ]
#' ex_pair
#' 
#' similarity_matrix(ex_pair, "scientific.name", "transect", index = "all")
#'
#' @author Eric Bastos Gorgens \email{e.gorgens@@gmail.com}
#' @importFrom ggplot2 ggplot after_stat
#'
similarity_matrix <- function(df, species, comparison, NI_label = "", index = "Sorensen", dendrogram = FALSE, n_groups=3){
  data <- label <- cluster <- NULL
  # ####
  # se df nao for fornecido, nulo, ou  nao for dataframe, ou nao tiver tamanho e nrow maior que 1,parar
  if(  missing(df) ){  
    stop("df not set", call. = F) 
  }else if(!is.data.frame(df)){
    stop("df must be a dataframe", call.=F)
  }else if(length(df)<=1 | nrow(df)<=1){
    stop("Length and number of rows of 'df' must be greater than 1", call.=F)
  }
  
  # se species nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(species) ){  
    stop("species not set", call. = F) 
  }else if( !is.character(species) ){
    stop("'species' must be a character containing a variable name", call.=F)
  }else if(length(species)!=1){
    stop("Length of 'species' must be 1", call.=F)
  }else if(forestmangr::check_names(df, species)==F){
    stop(forestmangr::check_names(df, species, boolean=F), call.=F)
  }
  
  # se comparison nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(comparison) ){  
    stop("comparison not set", call. = F) 
  }else if( !is.character(comparison) ){
    stop("'comparison' must be a character containing a variable name", call.=F)
  }else if(length(comparison)!=1){
    stop("Length of 'comparison' must be 1", call.=F)
  }else if(forestmangr::check_names(df, comparison)==F){
    stop(forestmangr::check_names(df, comparison, boolean=F), call.=F)
  }
  
  # Se NI_label nao for character,ou nao for de tamanho 1, parar
  if(!is.character( NI_label )){
    stop( "'NI_label' must be character", call.=F)
  }else if(length(NI_label)!=1){
    stop("Length of 'NI_label' must be 1", call.=F)
  }
  
  # Se index nao for character,ou nao for de tamanho 1, parar
  if(!is.character( index )){
    stop( "'index' must be character", call.=F)
  }else if(length(index)!=1){
    stop("Length of 'index' must be 1", call.=F)
  }else if(! index %in% c('all', 'Sorensen', 'Jaccard') ){ 
    stop("'index' must be equal to 'all', 'Sorensen' or 'Jaccard' ", call. = F) 
  }
  
  # se dendrogram nao for igual a TRUE ou FALSE,ou nao for de tamanho 1, parar
  if(! dendrogram %in% c(TRUE, FALSE) ){ 
    stop("'dendrogram' must be equal to TRUE or FALSE", call. = F) 
  }else  if(length(dendrogram)!=1){
    stop("Length of 'dendrogram' must be 1", call.=F)
  }
  
  # Se n_groups nao for numerico, nao for de tamanho 1, ou nao estiver dentro dos limites, parar
  if(!is.numeric( n_groups )){
    stop( "'n_groups' must be numeric", call.=F)
  }else if(length(n_groups)!=1){
    stop("length of 'n_groups' must be 1", call.=F)
  }else if(! n_groups > 0 | ! n_groups <= 9999){
    stop("'n_groups' must be a number between 0 and 9999", call.=F)
  }
  
  # ####
  df <- as.data.frame(df)
  # Remover NA
  df = df[!is.na(df[species]),]
  df = df[!is.na(df[comparison]),]
  
  # converter rotulos NI (aplicativo)
  if(is.null(NI_label)||NI_label==""){NI_label <- ""}
  
  # Remover observações cuja espécie é desconhecida
  # modifiquei para aceitar multiplas entradas
  semNI = df[! df[ ,species] %in% NI_label,]
  
  # Converter variaveis categoricas em fatores
  df[,species] <- as.factor(df[,species])
  df[,comparison] <- as.factor(df[,comparison])
  
  #drop levels para quando remover niveis que nao estao mais nos dados
  compair = levels(droplevels(df[,comparison]))
  
  # se so existirem 2 niveis, fazer comparacao pareada
  if(length(compair)==2){
    
    x <- df[ df[[comparison]]==compair[1], ]
    y <- df[ df[[comparison]]==compair[2], ]
    
    semNI1 = x[! x %in% NI_label]
    semNI1 = x[!is.na(x)]
    
    # Encontrar o número de espéciue que ocorrem na parcela
    a = length(unique(semNI1))
    
    # modifiquei para aceitar multiplas entradas
    semNI2 = y[! y %in% NI_label]
    
    b = length(unique(semNI2))
    
    c = length(intersect(unique(semNI1), unique(semNI2)))
    
    SJ = round(c / (a+b-c), 2)
    
    SO = round(2*c/(a+b), 2)
    
    
  }else{
    
    SO = matrix(1, nrow = length(compair), ncol = length(compair))
    SJ = matrix(1, nrow = length(compair), ncol = length(compair))
    for (p in seq(1, length(compair)-1,1)){
      for (r in seq(p+1, length(compair),1)){
        # Encontrar o número de espéciue que ocorrem na parcela
        a = length(unique(semNI[semNI[,comparison] == compair[p], species]))
        
        b = length(unique(semNI[semNI[,comparison] == compair[r], species]))
        
        c = length(intersect(unique(semNI[semNI[,comparison] == compair[p], species]),
                             unique(semNI[semNI[,comparison] == compair[r], species])))
        
        SJ[p, r] = round(c / (a+b-c), 2)
        SJ[r, p] = round(c / (a+b-c), 2)
        
        SO[p, r] = round(2 * c / (a+b), 2)
        SO[r, p] = round(2 * c / (a+b), 2)
      }
    }
    
  }
  
  # dendrogram part ####
  # Cria um dataframe com as duas matrizes
  icomb <- dplyr::bind_rows("Sorensen"=as.data.frame(SO),"Jaccard"=as.data.frame(SJ),.id = "index")
  if(all(SO==0)|all(SJ==0)){
    dendro_done <- data.frame(A=1,B=2,C=c("couldn't make dendrogram","couldn't make dendrogram"),stringsAsFactors=FALSE)  
  }else{
    
    dendro_done <- icomb %>% 
      dplyr::group_by(index) %>% #agrupa por matriz
      tidyr::nest() %>% 
      # Roda a funcao pra criar o dendrograma e criar o grafico em cada matriz com map
      dplyr::mutate(dendrogram = purrr::map(data, function(x, parc=levels( as.factor( droplevels(df[[comparison]]) ) ), n.groups=n_groups){
        x <- as.data.frame(x)
        rownames(x) <- parc
        hc    <- stats::hclust(stats::dist(x), "complete") # heirarchal clustering
        dendr <- ggdendro::dendro_data(hc) # convert for ggplot
        clust    <- stats::cutree(hc,k=n.groups)         # number of clusters
        clust.df <- data.frame(label=names(clust), cluster=factor(clust))
        ## dendr[["labels"]] has the labels, merge with clust.df based on label column
        dendr[["labels"]] <- merge(dendr[["labels"]],clust.df, by="label")
        
        ggdendro::ggdendrogram(dendr) +
          ggplot2::geom_text(data=ggdendro::label(dendr), ggplot2::aes(x, y, label=label, hjust=.5,color=cluster), size=4) +
          ggdendro::theme_dendro()
        
      } )) 
  }
  # return part ####
  
  if(index == "all" & dendrogram == TRUE){
    
    if(all(SO==0)|all(SJ==0)){
      bind_plot <- "couldn't make dendrogram"  
    }else{
      bind_plot <- gridExtra::arrangeGrob(dendro_done[[1,3]]+ggplot2::ggtitle("Sorensen"),
                                          dendro_done[[2,3]]+ggplot2::ggtitle("Jaccard")
      )
      
      # funcao pra converter a grob em ggplot... se nao nao tem como
      # salvar como objeto. Tirada do pacote ggplotify
      as_ggplot <- function(plot, scale = 1){
        ymin <- xmin <- 1 - scale
        xmax <- ymax <- scale
        ggplot2::ggplot(data.frame(x = 0:1, y = 0:1), ggplot2::aes_(x = ~x, y = ~y)) + 
          ggplot2::geom_blank() + 
          ggplot2::scale_x_continuous(limits = c(0, 1),expand = c(0, 0)) + 
          ggplot2::scale_y_continuous(limits = c(0,1), expand = c(0, 0)) + 
          ggplot2::annotation_custom(plot,xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax) + 
          ggplot2::theme_void()
      }
      
      bind_plot <- as_ggplot(bind_plot)
    }
    return(list(Dendrograms = bind_plot, Jaccard=SJ, Sorensen=SO))
    
    
  } else if(index == "all" & dendrogram == FALSE){
    
    return(list(Jaccard=SJ, Sorensen=SO))
    
  } else if (index == "Sorensen" & dendrogram == TRUE ){
    
    return(list(Dendrogram = dendro_done[[1,3]], Matrix = SO) )
    
  } else if (index == "Sorensen" & dendrogram == FALSE ){
    
    return(SO) 
    
  } else if (index == "Jaccard" & dendrogram == TRUE ){
    
    return(list(Dendrogram = dendro_done[[2,3]], Matrix = SJ) )
    
  } else if (index == "Jaccard" & dendrogram == FALSE ){
    
    return(SJ) 
    
  } else {
    
    return(list(Jaccard=SJ, Sorensen=SO))
    
  }
  
}