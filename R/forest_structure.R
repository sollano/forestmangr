#' @title
#' Get the forest horizontal, vertical and internal structure
#' @description 
#' This function calculates the horizontal structure of a given forest inventory data,
#' with information like absolute frequency, relative frequency, absolute density,
#' relative density, absolute dominance, relative dominance, importance value index, and coverage
#' value index. If additional variables are supplied, the vertical and internal strucures are
#' also provided.
#' 
#' @param df A data frame.
#' @param species Quoted name of the scientific names variable, or any variable used to differentiate the different species found in data.
#' @param dbh Quoted name of the diameter at breast hight variable, in cm.
#' @param plot Quoted name of the plot variable. used to differentiate the plot's trees, and calculate the number of sampled plots.
#' @param plot_area Quoted name of the plot area variable, or a numeric vector with the plot area value. The plot area value must be in square meters.
#' @param vertical_est Optional argument. Quoted name of the vertical strata variable, or the height variable. If this is a factor variable, it's levels will be used to classify the forest vertically. If it's a height variable, the vertical strata will be created based on it's mean and standard deviation values. Default: \code{NA}.
#' @param internal_est Optional argument. Quoted name of the internal strata variable. Default: \code{NA}.
#' @param NI_label Label used for Species not identified. This parameter works along with species. The level supplied here will not be considered in the classification. Default \code{""}.
#' @return a data frame with the forest's structure.
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
#' # Get the forest's horizontal structure:
#' forest_structure(exfm20, "scientific.name", "dbh", "transect", 10000)
#' 
#' # area plot as a variable name:
#' forest_structure(exfm20, "scientific.name", "dbh", "transect", "plot.area") 
#' 
#' # Get the forest's horizontal and vertical structure.
#' # The vertical structure variable can either be the height variable,
#' # or a factor variable with the horizontal strata:
#' forest_structure(exfm20, "scientific.name", "dbh", "transect", 10000, "canopy.pos") 
#' 
#' # Get the forest's horizontal, vertical and internal structure:
#' forest_structure(exfm20, "scientific.name", "dbh", "transect", 10000, "canopy.pos", "light") 
#' 
#' @author Eric Bastos Gorgens \email{e.gorgens@@gmail.com}
#'
forest_structure <- function(df, species, dbh, plot, plot_area, vertical_est = NA, internal_est = NA, NI_label = ""){
  # ####
  df <- as.data.frame(df)
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
  
  # se plot_area nao for fornecido, nao for numerico nem character, ou nao existir no dataframe,ou nao for de tamanho 1, parar
  if(  missing(plot_area) ){  
    stop("plot_area not set", call. = F) 
  }else if( is.null(plot_area) || is.na(plot_area) || plot_area == "" ){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(is.numeric(plot_area) & length(plot_area)==1){
    
    AREA.PLOT = plot_area
    
  }else if(!is.character(plot_area)){
    stop("'plot_area' must be a character containing a variable name or a numeric value", call.=F)
  }else if(length(plot_area)!=1){
    stop("Length of 'plot_area' must be 1", call.=F)
  }else if(forestmangr::check_names(df, plot_area)==F){
    stop(forestmangr::check_names(df, plot_area, boolean = F), call.=F)
  }else if(is.character(plot_area)){
    
    AREA.PLOT = mean(df[,plot_area],na.rm = T )
    
  }
  
  # se vertical_est nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(vertical_est) || is.null(vertical_est) || is.na(vertical_est) || vertical_est == "" ){
    
    vertical_est <- NA
    
  }else if(!is.character(vertical_est)){
    stop("'vertical_est' must be a character containing a variable name", call.=F)
  }else if(length(vertical_est)!=1){
    stop("Length of 'vertical_est' must be 1", call.=F)
  }else if(forestmangr::check_names(df, vertical_est)==F){
    stop(forestmangr::check_names(df, vertical_est, boolean=F), call.=F)
  }
  
  
  # se internal_est nao for fornecido, for igual "", nulo ou NA, criar variavel vazia 
  # se existir e nao for character,  parar
  if(missing(internal_est) || is.null(internal_est) || is.na(internal_est) || internal_est == "" ){
    
    internal_est <- NA
    
  }else if(!is.character(internal_est)){
    stop("'internal_est' must be a character containing a variable name", call.=F)
  }else if(length(internal_est)!=1){
    stop("Length of 'internal_est' must be 1", call.=F)
  }else if(forestmangr::check_names(df, internal_est)==F){
    stop(forestmangr::check_names(df, internal_est, boolean=F), call.=F)
  }
  
  # Se NI_label nao for character,ou nao for de tamanho 1, parar
  if(!is.character( NI_label )){
    stop( "'NI_label' must be character", call.=F)
  }else if(length(NI_label)!=1){
    stop("Length of 'NI_label' must be 1", call.=F)
  }
  # ####
  SPECIES = species
  DBH = dbh
  PLOTS = plot
  # alterei aqui para areaplot poder ser uma coluna do data frame
  #if(is.numeric(plot_area) ){AREA.PLOT = plot_area}else(AREA.PLOT = mean(df[,plot_area],na.rm = T ) )
  
  # Se pos.vertical for a variavel HT (numerica)
  if(is.numeric(df[[vertical_est]])){
    df$canopy <- ifelse(df[[vertical_est]] <= (mean(df[[vertical_est]], na.rm=T) - stats::sd(df[[vertical_est]], na.rm=T) ), "Inferior", 
                          ifelse(df[[vertical_est]] >= (mean(df[[vertical_est]], na.rm=T) - stats::sd(df[[vertical_est]], na.rm=T) ) & df[[vertical_est]] < (mean(df[[vertical_est]], na.rm=T) + stats::sd(df[[vertical_est]], na.rm=T) ), "Medio", 
                                 ifelse(df[[vertical_est]] >= (mean(df[[vertical_est]], na.rm=T) + stats::sd(df[[vertical_est]], na.rm=T) ), "Superior", NA
                                 )
                          ) 
    )
    vertical_est <- "canopy"
    
  }
  
  VERTICAL = vertical_est
  INTERNA = internal_est
  NI = NI_label
  
  # Converter variaveis categoricas em fatores
  df[,PLOTS] <- as.factor(df[,PLOTS])
  df[,SPECIES] <- as.factor(df[,SPECIES])
  
  # converter rotulos NI (aplicativo)
  if(is.null(NI)){NI <- ""}
  
  # Ajustar formato categorico
  
  # tive que colocar estes if statements aqui tambem,
  # para caso as variaveis opcionais nao sejam inseridas
  if(!is.na(vertical_est)){
    
    df[,VERTICAL] = as.factor(df[,VERTICAL])
  }
  
  if(!is.na(internal_est)){
    
    df[,INTERNA] = as.factor(df[,INTERNA])
    
  }
  
  # Remover NA
  df = df[!is.na(df[SPECIES]),]
  df = df[!is.na(df[DBH]),]
  
  # Remover NI
  # modifiquei para aceitar multiplas entradas
  df = df[!df[,SPECIES] %in% NI,]
  espList = levels(factor(df[,SPECIES]))
  
  # Constroi tabela de frequencia
  pivot = data.frame(table(df[SPECIES]))
  names(pivot) = c("especie", "sum")
  pivot = pivot[which(pivot$especie %in% espList),]
  
  # Calcula número de parcelas na area de estudo
  nplots = length(unique(df[,PLOTS]))
  
  # Estrutura horizontal
  # Calcula frequencia absoluta e relativa
  for (i in levels(df[,PLOTS])){
    tableFreq = data.frame(table(df[df[PLOTS] == i,SPECIES]))
    pivot = cbind(pivot, tableFreq[which(tableFreq[,1] %in% espList),2])
    names(pivot)[ncol(pivot)] = i
  }    
  
  AcAFi = 0
  AF = 0
  for (i in seq(1, nrow(pivot), 1)){
    contagem = pivot[i,-c(1,2)] > 0
    cplots = length(contagem[contagem == TRUE])
    AFi = cplots/nplots * 100
    AcAFi = AcAFi + AFi
    AF[i] = AFi
  }
  
  result = pivot[1]
  result["AF"] = round(AF, 4)
  
  RF = AF / AcAFi * 100
  result["RF"] = round(RF, 4)
  
  # Calcula densidade absoluta e relativa
  # Alterei aqui para a area poder ser inserida em m2
  AD = pivot[2] / (nplots * (AREA.PLOT/10000) )
  result["AD"] = round(AD, 4)
  AcADi = sum(AD)    
  DR = AD / AcADi * 100
  result["DR"] = round(DR, 4)
  
  # Calcula dominância absoluta e relativa
  
  df["AB"] = df[DBH]^2 * pi / 40000
  AB = tapply(df[,"AB"], df[,SPECIES], sum)
  AB = AB[which(names(AB) %in% espList)]
  
  # Alterei aqui para a area poder ser inserida em m2
  ADo = AB / (nplots * (AREA.PLOT/10000) )
  result["ADo"] = round(ADo, 6)
  
  AcADoi = sum(ADo)
  RDo = ADo / AcADoi * 100
  result["RDo"] = round(RDo, 6)
  
  # Calcula Valor de Cobertura
  IVC = (DR + RDo)/2
  result["IVC"] = round(IVC, 6)
  
  # Calcula valor de Importancia
  IVI = (RF + DR + RDo)/3
  result["IVI"] = round(IVI, 6)
  
  
  rm(AB, AcADi, AcADoi, AcAFi, cplots, ADo, RDo, AF, AFi, RF, AD, DR, IVC, IVI, tableFreq, i, contagem)
  
  if (!is.na(vertical_est)){
    # Estrutura vertical
    
    vert = pivot["especie"]
    for (j in levels(df[,VERTICAL])){
      daVert = data.frame(table(df[df[VERTICAL] == j, SPECIES]))
      vert = cbind(vert, daVert[which(daVert[,1] %in% espList),2])
    }
    names(vert)[-1] = levels(df[,VERTICAL])
    
    VFj = data.frame()
    for (j in levels(df[,VERTICAL])){
      VFj[1,j] = sum(vert[, j]) / sum(vert[, seq(2,length(levels(df[,VERTICAL]))+1,1)]) * 100
    }
    
    for (j in levels(df[,VERTICAL])){
      for (i in seq(1, nrow(vert), 1)){
        vert[i, paste("VF", j, sep = "")] = vert[i, j] * VFj[1, j]
        result[i, paste("VF", j, sep = "")] = vert[i, j] * VFj[1, j]
      }
    }
    
    AcPSAi = 0
    for (i in seq(1, nrow(vert), 1)){
      PSAi = 0
      for (j in levels(df[,VERTICAL])){
        
        PSAi = PSAi + VFj[1, j] * vert[i, j] 
      }
      vert[i, "PSA"] = PSAi
      AcPSAi = AcPSAi + PSAi
    }
    
    result["PSA"] = vert["PSA"]
    result["PSR"] = vert["PSA"] / AcPSAi * 100
    rm(AcPSAi, i, j, PSAi, VFj, daVert, vert)
  }
  
  if (!is.na(internal_est)){
    
    # Estrutura Interna
    intern = pivot["especie"]
    for (j in levels((df[,INTERNA]))){
      daInter = data.frame(table(df[df[INTERNA] == j, SPECIES]))
      intern = cbind(intern, daInter[which(daInter[,1] %in% espList),2])
    }
    names(intern)[-1] = levels(df[,INTERNA])
    
    for (j in levels(df[,INTERNA])){
      for (i in seq(1, nrow(intern), 1)){
        intern[i, paste("QF", j, sep = "")] = intern[i, j] * (sum(intern[,j]) / sum(intern[, seq(2,length(levels(df[,INTERNA]))+1,1)]))
        result[i, paste("QF", j, sep = "")] = intern[i, j] * (sum(intern[,j]) / sum(intern[, seq(2,length(levels(df[,INTERNA]))+1,1)]))
      }
    }
    
    AcQAFi = 0
    for (i in seq(1, nrow(intern), 1)){
      intern[i, "QAF"] = sum(intern[i, seq(2+length(levels(df[,INTERNA])),2*length(levels(df[,INTERNA]))+1,1)])
      AcQAFi = AcQAFi + intern[i, "QAF"]
    }
    
    result["QAF"] = intern["QAF"]
    result["QRF"] = intern["QAF"] / AcQAFi * 100
    rm(daInter, AcQAFi, i, j, intern)
  }
  rm(pivot)
  return(dplyr::as_tibble(result))
}    
