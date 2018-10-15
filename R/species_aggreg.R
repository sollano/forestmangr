#' @title
#' Get the aggregation state of species
#' @description 
#' Get the aggregation state of species according to Payandeh, Hazen and Morista's methods.
#' 
#' @param df A data frame.
#' @param species Quoted name of the scientific names variable, or any variable used to differentiate the different species found in data.
#' @param plot Quoted name of the plot variable. used to differentiate the plots trees, and calculate the number of sampled plots.
#' @param NI_label Label used for Species not identified. This parameter works along with species. The level supplied here will not be considered in the classification. Default \code{""}.
#' @return a data frame with the aggregation classification.
#' 
#' @references 
#' Souza, A. L. and Soares, C. P. B. (2013) Florestas Nativas: estrutura, dinâmica e manejo. Viçosa: UFV.
#' 
#' @export
#' 
#' @examples 
#' library(forestmangr)
#' data(exfm20)
#' 
#' species_aggreg(exfm20, "scientific.name", "transect")
#' 
#' @author Eric Bastos Gorgens \email{e.gorgens@@gmail.com}
#'
species_aggreg <- function(df, species, plot, NI_label = ""){
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
  
  # se plot nao for fornecido nao for character, ou nao for um nome de variavel,ou nao for de tamanho 1, parar
  if(  missing(plot) ){  
    stop("plot not set", call. = F) 
  }else if( !is.character(plot) ){
    stop("'plot' must be a character containing a variable name", call.=F)
  }else if(length(plot)!=1){
    stop("Length of 'plot' must be 1", call.=F)
  }else if(forestmangr::check_names(df, plot)==F){
    stop(forestmangr::check_names(df, plot, boolean=F), call.=F)
  }
  
  # Se NI_label nao for character,ou nao for de tamanho 1, parar
  if(!is.character( NI_label )){
    stop( "'NI_label' must be character", call.=F)
  }else if(length(NI_label)!=1){
    stop("Length of 'NI_label' must be 1", call.=F)
  }
  # ####
  SPECIES = species
  PLOTS = plot
  NI = NI_label  
  df <- as.data.frame(df)
  # Remover NA
  df = df[!is.na(df[SPECIES]),]
  
  # converter rotulos NI (aplicativo)
  if(is.null(NI)||NI==""){NI <- ""}
  
  # Remover NI
  # modifiquei para aceitar multiplas entradas
  df = df[! df[,SPECIES] %in% NI,]
  espList = levels(factor(df[,SPECIES]))
  
  # Converter variaveis categoricas em fatores
  df[,PLOTS] <- as.factor(df[,PLOTS])
  df[,SPECIES] <- as.factor(df[,SPECIES])
  
  
  # Constroi tabela de frequencia
  pivot = data.frame(table(df[SPECIES]))
  names(pivot) = c("especie", "sum")
  pivot = pivot[which(pivot$especie %in% espList),]
  
  # Calcula número de parcelas na área de estudo
  nplots = length(unique(df[,PLOTS]))
  
  # Qui-quadrado tabelado para indice de Hazen
  chisq75 = qchisq(0.75, nplots - 1)
  chisq99 = qchisq(0.99, nplots - 1)
  
  for (i in levels(df[,PLOTS])){
    tableFreq = data.frame(table(df[df[PLOTS] == i,SPECIES]))
    pivot = cbind(pivot, tableFreq[which(tableFreq[,1] %in% espList),2])
    names(pivot)[ncol(pivot)] = i
  } 
  
  agreg = pivot[1]
  if(nplots > 3){
    for (i in seq(1, length(pivot[,1]))){ 
      Si = var(as.numeric(pivot[i, seq(3, (2 + nplots), 1)]))
      Mi = mean(as.numeric(pivot[i, seq(3, (2 + nplots), 1)]))
      agreg[i,"Payandeh"] = round(Si/Mi, 1)
      if(round(Si/Mi, 1) == 1){
        agreg[i, "Pay.res"] = "Random"
      } else if(round(Si/Mi, 1) < 1) {
        agreg[i, "Pay.res"] = "Regular"
      } else {
        agreg[i, "Pay.res"] = "Aggregated"
      }
      
      agreg[i,"Hazen"] = round(Si/Mi * (nplots - 1), 1)
      if(round(Si/Mi * (nplots - 1), 1) > chisq99){
        agreg[i, "Haz.res"] = "Aggregated"
      } else if(round(Si/Mi * (nplots - 1), 1) < chisq75) {
        agreg[i, "Haz.res"] = "Not aggregated"
      } else {
        agreg[i, "Haz.res"] = "Tends to aggregate"
      }
      
      if ( (as.numeric(pivot[i, 2]) * (as.numeric(pivot[i, 2])-1)) != 0){
        agreg[i,"Morisita"] = round((sum(as.numeric(pivot[i, seq(3, (2 + nplots), 1)]) * (as.numeric(pivot[i, seq(3, (2 + nplots), 1)]) - 1))) / (as.numeric(pivot[i, 2]) * (as.numeric(pivot[i, 2])-1)) * nplots, 1)
      } else {
        agreg[i,"Morisita"] = round(0, 0)
      }
      if(agreg[i,"Morisita"] == 1){
        agreg[i, "Mor.res"] = "Random"
      } else if(agreg[i,"Morisita"] < 1 & agreg[i,"Morisita"] > 0) {
        agreg[i, "Mor.res"] = "Regular"
      } else if(agreg[i,"Morisita"] == 0){
        agreg[i, "Mor.res"] = "Rare"
      } else {
        agreg[i, "Mor.res"] = "Aggregated"
      }
    }
    return(dplyr::as_tibble(agreg))
  } else {
    stop("Low number of plots", .call=FALSE)
  }
}