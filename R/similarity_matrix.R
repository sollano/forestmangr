#' @title
#' Get the similarity matrix of an area
#' @description 
#' Calculates the Jaccard similarity index and Sorensen similarity index.
#' @param df A data frame.
#' @param species Quoted name of the scientific names variable, or any variable used to differentiate the different species found in data. If supplied, will be used to classify the species in the diameter data.
#' @param comparison Quoted name of the variable containing levels to be compared with each other.
#' @param NI_label Label used for Species not identified. This parameter works along with species. The level supplied here will not be considered in the classification. Default \code{""}.
#' @param index Character value for the desired index to be used. Can be either \code{Jaccard}, for a matrix based on the Jaccard index of similarity, \code{"Sorensen"}, for a matrix based Sorensen's index of similarity, or \code{"all"}, for a list with matrices for both indexes. Default: \code{"Sorensen"}.
#' @return a matrix object with a similarity matrix, or a list with two similarity matrices, one for each index, according to the index argument.
#' 
#' @references 
#' Souza, A. L. and Soares, C. P. B. (2013) Florestas Nativas: estrutura, dinamica e manejo. Vicosa: UFV.
#' 
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data(exfm20)
#' 
#' # To get the similarity matrix of an area, we simply need to provide
#' # the species variable name, and a subdivision variable name, like
#' # transect. By default we get a a matrix based on the Sorensen index:
#' similarity_matrix(exfm20, "scientific.name", "transect")
#' 
#' # To get the similarity matrix of Jaccard, use the index argument:
#' similarity_matrix(exfm20, "scientific.name", "transect", index = "Jaccard")
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
#'
similarity_matrix <- function(df, species, comparison, NI_label = "", index = "Sorensen"){
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
  if(index == "all"){
    
    return(list(Jaccard=SJ, Sorensen=SO))
    
  } else if (index == "Sorensen"){
    
    return(SO)
    
  } else if (index == "Jaccard"){
    
    return(SJ)
    
  } else {
    
    return(list(Jaccard=SJ, Sorensen=SO))
    
  }
}