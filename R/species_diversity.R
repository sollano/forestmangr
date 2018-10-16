#' @title
#' Get the species diversity indexes
#' @description 
#' Calculate the diversity of species for the following indexes:
#' Shannon, Simpson, Equitability, Pielou and Jentsch.
#' 
#' @param df A data frame.
#' @param species Quoted name of the scientific names variable, or any variable used to differentiate the different species found in data. If supplied, will be used to classify the species in the diameter data.
#' @param plot Optional parameter. Quoted name of the plot variable. used to differentiate the plots, and calculate the indexes by plot, or other subdivision variable.
#' @param NI_label Label used for Species not identified. This parameter works along with species. The level supplied here will not be considered in the classification. Default \code{""}.
#' @param index Character value for the desired index to be used. Can be either \code{"H"} for Shannon's diversity index, \code{"S"} for Total number of species in the community, \code{"Hmax"} for the maximum equitability, \code{"J"} for Pielou's evenness, \code{"QM"} for the mixture coefficient of Jentsch, or \code{"all"}, to get all indexes. Default: \code{"all"}.
#' @return a data frame with the indexes, or a numeric value of the desired index specified in the index argument.
#' 
#' @references 
#' Souza, A. L. and Soares, C. P. B. (2013) Florestas Nativas: estrutura, dinamica e manejo. Viçosa: UFV.
#' 
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data(exfm20)
#' 
#' # By default, the function returns all indexes:
#' species_diversity(exfm20, "scientific.name")
#' 
#' # It's possible to use a subdivision variable, like plot, to get
#' # the indexes for each subdivision:
#' species_diversity(exfm20, "scientific.name", "transect") 
#'
#' # To only get one specific index, use the index argument:
#' species_diversity(exfm20, "scientific.name", index = "H")
#' species_diversity(exfm20, "scientific.name", index = "S")
#' species_diversity(exfm20, "scientific.name", index = "Hmax")
#' species_diversity(exfm20, "scientific.name", index = "J")
#' 
#' 
#' @author Eric Bastos Gorgens \email{e.gorgens@@gmail.com}
#'
species_diversity <- function(df, species, plot=NA, NI_label = "", index="all"){
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
  }else if(! index %in% c('all', 'H', 'S', 'Hmax', 'J', 'QM') ){ 
  stop("'index' must be equal to 'all', 'H', 'S', 'Hmax', 'j' or 'QM' ", call. = F) 
  }
  # ####
  # Remover NA
  df <- as.data.frame(df)
  df <- df[!is.na(df[species]),]
  
  # converter rotulos NI (aplicativo)
  if(is.null(NI_label)||NI_label==""){NI_label <- ""}
  
  # Remover NI (modifiquei para aceitar multiplas)
  #semNI = df[ ! df[,species] %in% NI_label, species]
  semNI = df[ ! df %in% NI_label ]
  
  ESPECIES <- semNI[species]
  
  # Condicional: Se o usuario nao fornecer a variavel parcela,
  # cria-se um vetor vazio com o mesmo numero de linhas chamado parcela
  if(missing(plot) || is.null(plot) || is.na(plot) || plot == ""){
    
    PARCELAS <- vector("character", nrow(ESPECIES) )
    
    # transformar argumento vazio em NA, para evitar erros
    plot <- NA
    
  }else{ # caso contrario, cria-se um objeto que contem a variavel parcela
    PARCELAS <- semNI[plot]
  }
  
  # Com a funcao by calcula-se os indexes por PARCELAS;
  # caso plot nao tenha sido fornecido, PARCELAS sera um vetor vazio,
  # e o calculo sera feito considerando todo o dado.
  tab_indices <- by(ESPECIES, PARCELAS , function(x){
    
    tableFreq = table(x)
    tableP = data.frame(tableFreq)
    names(tableP) = c("especie", "freq")
    
    # Calcula número de indivíduos na amostra
    #N = length(semNI)
    N = sum(tableP$freq)
    
    # Calcula a proporção de cada espécie
    tableP$p = tableP$freq / N
    
    # Calcula o log da proporção de cada espécie
    tableP$lnp = log(tableP$p)
    tableP[tableP$lnp  == "-Inf", "lnp"] = 0
    
    # Número de espécies amostradas
    Sesp = length(tableP[tableP$freq > 0, "especie"])
    
    # Calcula Shannon
    H = round(- sum(tableP$p * tableP$lnp), 2)
    
    #Calcula Simpson
    S = round(1 - (sum(tableP$freq*(tableP$freq - 1))/(N*(N-1))), 2)
    
    # Diversidade Máxima
    Hmax = round(log(length(tableP$freq[tableP$freq>0])), 2)
    
    # Equabilidade de Pielou
    J = round(H / Hmax, 2)
    
    # Coeficiente de mistura de Jentsch
    QM = round(Sesp / N, 2)
    
    tab_final <- data.frame(Shannon = H, Simpson = S, EqMaxima = Hmax, Pielou = J, Jentsch = QM)
    
    return(tab_final)
    
  } )
  
  # transforma-se o objeto de classe by criado em um dataframe
  tab_indices <- data.frame(do.call(rbind, tab_indices))
  
  
  # converter nomes das linhas em coluna, caso os calculos tenham sido feitos por PARCELAS
  if( !is.na(plot) ){
    
    tab_indices <- cbind(aux = row.names(tab_indices), tab_indices)
    
    names(tab_indices)[names(tab_indices) == "aux"] <- plot
    row.names(tab_indices) <- NULL
  }
  
  
  if (missing(index)|index=="all"){
    return(dplyr::as_tibble(tab_indices))
  } else if (index == "H"){
    return( tab_indices$Shannon )
  } else if (index == "S"){
    return(tab_indices$Simpson)
  } else if (index == "Hmax"){
    return(tab_indices$EqMaxima)
  } else if (index == "J"){
    return(tab_indices$Pielou)
  } else if (index == "QM"){  
    return(tab_indices$Jentsch)
  } else {
    return(dplyr::as_tibble(tab_indices))
  }
}