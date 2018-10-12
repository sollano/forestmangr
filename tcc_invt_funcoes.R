# TCC Script para processamento de Inventario no R utilizando as funcoes

# Script criado por: Sollano Rabelo Braga & Marcio Leles Romarco de Oliveira

# Parte I: Carregar Pacotes e Dados ==============================================================================================================================

# 1) Carregar pacotes e funcoes =======================================================================================================

# Neste wobript iremos rodar o inventario utilizando funcoes criadas
# para facilitar a execucao do inventario no R.
# A maioria dessas funcoes foram criadas utilizando os pacotes tidyr, dplyr e lazyeval como base.

# Agora, carregamos as funcoes com:

library(forestmangr)

# em todas as funcoes, o primeiro argumento e o dataframe a ser utilizado,
# e os demais sao os nomes das variaveis que serao utilizadas, entre aspas.
# entao quando se pede a variavel altura, e a variavel altura do seu dataframe 
# e denominada TH, no argumento utiliza-se "TH". Isto sera exemplificado no wobript.

# 2) carregar dados ==================================================================================================

# para se carregar os dados pode-se especificar o caminho onde o objeto se encontra, por exemplo
# "D:/Documentos/R/dados/dados_sma.csv"

# caso este esteja no diretorio de trabalho, nao e preciso especificar o caminho ate este ponto,
# sendo apenas "dados/dados_sma.csv"
# Neste caso existe uma pasta chamada dados, no diretorio de trabalho
data("exfm7")
data("exfm8")
data("exfm9")
data("exfm4")

dados_sma  <- exfm7
dados_hub  <- exfm8
dados_invt  <- exfm9

head(dados_sma)
head(dados_hub)
head(dados_invt)

tab_invt <- exfm4

# para se trabalhar melhor no R, transformamos 0 em NA
dados_sma[dados_sma==0]   <- NA 
dados_hub[dados_hub==0]   <- NA 
dados_invt[dados_invt==0] <- NA 

# Parte II: Cubagem ==============================================================================================================================================

# Para a cubagem iremos utilizar os metodos de Smalian e Huber 

# 1) Cubagem - metodo de Smalian -------------------------------------------------------------------------------------------------------------------------------

 # Vwb                    #dataframe #diametro #altura da secao # grupo 
dados_smalian <- smalianwb(dados_sma,"di_wb", "hi","TREE")
dados_smalian 

#Vwob                 #dataframe #diametro #altura da secao #espessura da casca  # grupo  
dados_smalian <- smalianwob(dados_sma, "di_wb", "hi", "bark_t","TREE", bt_mm_to_cm = T)
dados_smalian

# VWB e VWOB direto, utilizando %>% 
dados_smalian <- dados_sma %>% 
  smalianwb("di_wb", "hi", "TREE") %>% 
  smalianwob("di_wb", "hi", "bark_t", "TREE", bt_mm_to_cm = T)

dados_smalian

# 2) Cubagem - metodo de Huber ---------------------------------------------------------------------------------------------------------------------------------

# Vwb                    #dataframe # grupo  #diametro #comprimento da secao
dados_huber <- huberwb(dados_hub, "di_wb", "sec_length", "TREE")
dados_huber

#Vwob                 #dataframe # grupo  #diametro #comprimento da secao #espessura da cawoba
dados_huber <- huberwob(dados_hub, "di_wb", "sec_length", "bark_t", "TREE", bt_mm_to_cm = T)
dados_huber

# VWB e VWOB direto, utilizando %>% 
dados_huber <- dados_hub %>% 
  huberwb("di_wb", "sec_length", "TREE") %>% 
  huberwob("di_wb", "sec_length", "bark_t", "TREE", bt_mm_to_cm = T)
dados_huber

# Visualzizar curva media ----------------------------------------------

library(tidyverse)

dados_smalian %>% 
  mutate(d_by_dbh = di_wb/DBH,h_by_th = hi/TH, h_by_th_quad = h_by_th^2 ) %>% 
  ggplot(aes(x=d_by_dbh, y=h_by_th)) + 
  geom_point(size = 2, alpha = .4) + 
 # coord_fixed(ratio=2) +
  labs(x=expression(italic(frac(d,DBH))), 
       y=expression(italic(frac(h,TH)))
       ) +
  ggpmisc::stat_poly_eq(
    formula = x ~ poly(y, 2, raw=T),
    size = 3,
    eq.x.rhs    = "italic(frac(h,TH))",
    eq.with.lhs = "italic(hat(frac(d,DBH)))~`=`~", 
    ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "*plain(\",\")~")),
    label.x.npc="right",
    parse = TRUE  ) +
  ggthemes::theme_igray(base_family = "serif") +
  theme(
    axis.title.y = element_text(angle = 0, vjust =.5),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    axis.title   = element_text(size = 14,fstrs="bold"), 
    axis.text    = element_text(size = 14),
    axis.line.x = element_line(color="black"),
    axis.line.y = element_line(color="black"),
    strip.text.x = element_text(size = 14)   )

# ou ####
  
average_tree_curve(df = dados_smalian, d = "di_wb", dbh = "DBH", h = "hi", th = "TH")
average_tree_curve(df = dados_smalian, d = "di_wb", dbh = "DBH", h = "hi", th = "TH", fstrst = "STRATA")


# 3) Cubagem - Compilar os dados por Arvore e Calcular do Fator de Forma -----------------------------------------------------------------------------------------

dados_vol_secao <- dados_smalian

dados_vol_arvore <- vol_summarise(dados_smalian, dbh = "DBH", th = "TH", vwb = "VWB", tree = "TREE", .groups = "STRATA", vwob = "VWOB")
dados_vol_arvore

# ####

diameter_class(df = dados_vol_arvore, dbh = "DBH", ci = 5, dbhmin = 5, volume = "VWB") %>%
  rename(VWB= volume) %>% 
  mutate(VWOB = diameter_class(df = dados_vol_arvore, dbh = "DBH", ci = 5, dbhmin = 5, volume = "VWOB") %>%rename(VWOB= volume) %>% pull(VWOB) )

# 4) Metodo de estimacao de volume ---------------------------------------------------------------------------------------------------------------------------------

# Para se estimar o volume, podemos utilizar o metodo do fator de forma medio,
# ou equacoes volumetricas


# 4.1) Fator de forma ---------------------------------------------------------------------------------------------------------------------------------------------

dados_vol_arvore <- dados_vol_arvore %>% 
  mutate(VFFCC = mean(FFWB_medio) * (pi * DBH^2 / 4000 * TH), 
         VFFSC = mean(FFWOB_medio) * (pi * DBH^2 / 4000 * TH) )
dados_vol_arvore

# 4.2) Ajus eq de volume ------------------------------------------------------------------------------------------------------------------------------------------

tabcoef_vwb <- dados_vol_arvore %>% 
  lm_table( log(VWB) ~  log(DBH) + log(TH), "STRATA" )
tabcoef_vwb

tabcoef_vwob <- dados_vol_arvore %>% 
  lm_table( log(VWOB) ~  log(DBH) + log(TH), "STRATA" )
tabcoef_vwob

# PARTE III INVENTARIO ===========================================================================================================================================

# PARTE III SECAO I - Processamento de Altura ====================================================================================================================

# 1) Ajuste de modelos Hipsometricos 

# Alguns modelos de altura utilizam a variável Altura Dominante (DH),
# o que se torna um impecilho na hora do calculo da mesma, pois seu calculo
# em alguns softwares pode ser oneroso.

# Vamos calcular a altura dominante por talhao/parcela
# e ajustar o modelo de Campos e Leite

# 1.1) Calculo da Altura dominante (DH) --------------------------------------------------------------------------------------------------------------------------

#a funcao dom_height nos da a tabela de altura dominante
dom_height(df=dados_invt,th="TH",dbh="DBH",plot="PLOT",obs="OBS",dom="D",.groups="STRATA")

# podemos uni-la diretamente aos dados utilizando o argumento merge_data:
dados_hd <- dom_height(df=dados_invt,th="TH",dbh="DBH",plot="PLOT",obs="OBS",dom="D",.groups="STRATA", merge_data=T)

head(dados_hd)

# 1.2) Proces de altura - Ajuste ---------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
# se o objetivo for ajustar o modelo no R, 
# pode-se utilizar a funcao lm_table_group, para ajustar o mesmo modelo para varios talhoes, e/ou parcelas

tabcoef_th <- dados_hd %>% # dataframe q sera utilizado
  mutate(LN_TH = log(TH), INV_DBH = 1/DBH, LN_DH = log(DH) ) %>% # variaveis necessarias para reg
  lm_table( LN_TH ~ INV_DBH + LN_DH, "STRATA" )
tabcoef_th 

#ou utilizando inv(), nao e preciso calcular as variaveis previamente
tabcoef_th <- dados_hd %>% 
  lm_table( log(TH) ~ inv(DBH) + log(DH), "STRATA" )
tabcoef_th

# 2) Proces de altura - calculo da TH para arvores nao medidas ####

dados_invt2 <-  dados_hd %>%  
  lm_table(log(TH) ~ inv(DBH) + log(DH),output = "merge_est" ) %>% 
  mutate( TH_EST = ifelse(is.na(TH), est, TH ), est=NULL )

head(dados_invt2,20)


# PARTE III SECAO II - Estimativa de Volume / Quantificacao de parcelas =========================================================================================

# 1) Calculo de CSA, AGE, VWB e VWOB ----------------------------------------------------------------------------------------------------------------------------

#dados_invt$MEASUREMENT_DATE <- as.Date(dados_invt$MEASUREMENT_DATE, format = "%d/%m/%Y")
#dados_invt$PLANTING_DATE <- as.Date(dados_invt$PLANTING_DATE, format = "%d/%m/%Y")

# nao ha a necessidade de se utilizar funcoes compostas nesta parte,
# portanto utilzamos o dplyr direto
dados_invt3 <- dados_invt2 %>% # remocao das var criadas
  left_join(tabcoef_vwb, by = "STRATA" ) %>% # uniao com coefs de vwb
  mutate(CSA = pi*DBH^2/40000,
         AGE = as.numeric(MEASUREMENT_DATE - PLANTING_DATE) / 30,
         VWB = exp(b0 + b1*log(DBH) + b2*log(TH_EST) ) ) %>%
  select(-b0,-b1,-b2,-Rsqr, -Rsqr_adj, -Std.Error) %>% # remocao das var criadas # caso nao se tenha valores para VWOB, deve-se parar por aqui
  left_join(tabcoef_vwob, by = c("STRATA") ) %>% # uniao com coefs de vwob
  mutate(VWOB = exp(b0 + b1*log(DBH) + b2*log(TH_EST) ) )%>%
  select(-b0,-b1,-b2,-Rsqr,-Rsqr_adj, -Std.Error)
head(dados_invt3)

# 3) Quantificacao de parcelas ------------------------------------------------------------------------------------------------------------------------------------
  
# Obs: Os calculos daqui em diante consideram AREA_STRATA em ha, e PLOT_AREA em m2;
# Isto deve ser respeitado para o funcionamento do wobript;
# Caso contrario as conversoes a seguir devem ser feitas antes de se prosseguir:
  
# Area do talhao de m2 para ha
# dados_invt3$AREA_STRATA <- dados_invt3$STRATA / 10000
  
# Area da parcela de ha para m2
# dados_invt3$PLOT_AREA <- dados_invt3$PLOT_AREA * 10000
  
# Agora faremos a totalizacao das parcelas com a funcao plot_summarise
#Area total, idade,  VWOB, e DH sao opcionais. Caso a altura dominante nao seja fornecida, ela sera calculada com base nas 2 maiores arvores da parcela. 
tab_invt <- plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA", "VWB", "VWOB", "DH", "AGE")
tab_invt   

plot_summarise(dados_invt3, "PLOT", 810)
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA", "VWB")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA", "VWB", "VWOB")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA", "VWB", "VWOB", "DH")
plot_summarise(dados_invt3, "PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA", "VWB", "VWOB", "DH", "AGE")


# 4) Parte III secao I e II - Processo direto ----------------------------------------------------------------------------------------------------------------------------------------------
data("exfm4")
dados_invt <- exfm4

dados_invt$MEASUREMENT_DATE <- as.Date(dados_invt$MEASUREMENT_DATE, format = "%d/%m/%Y")
dados_invt$PLANTING_DATE <- as.Date(dados_invt$PLANTING_DATE, format = "%d/%m/%Y")

tab_invt  <- dados_invt %>% 
  dom_height(th="TH",dbh="DBH",plot="PLOT",obs="OBS",dom="D",.groups="STRATA", merge_data=T, dh_name = "DH") %>% 
  lm_table(log(TH) ~ inv(DBH) + log(DH),output = "merge_est" ) %>% 
  mutate( TH_EST = ifelse(is.na(TH), est, TH ), est=NULL ) %>% 
  left_join(tabcoef_vwb, by = "STRATA") %>% # uniao com coefs de vwb
  mutate(AGE = as.numeric(MEASUREMENT_DATE - PLANTING_DATE) / 30,
         VWB = exp(b0 + b1*log(DBH) + b2*log(TH_EST) ) ) %>%
  select(-matches("^b|Rs|Std")) %>% 
  left_join(tabcoef_vwob, by = "STRATA" ) %>% # uniao com coefs de vwob
  mutate(VWOB = exp(b0 + b1*log(DBH) + b2*log(TH_EST) ) )%>% # estimar VWOB
  select(-matches("^b|Rs|Std")) %>% 
  plot_summarise("PLOT", "PLOT_AREA", "DBH", "TH_EST", "STRATA", "STRATA_AREA", "VWB", "VWOB", "DH", "AGE")
tab_invt 

# PARTE III SECAO III - Estatisticas do Inventario ===============================================================================================================

# Relembrando novamente que se deve respeitar as unidades de STRATA_AREA(ha) e PLOT_AREA (m2)

# Caso necessario, as conversoes podem ser feitas com

# Area do talhao de m2 para ha
# dados_invt$STRATA_AREA <- dados_invt3$STRATA / 10000

# Area da parcela de ha para m2
# dados_invt$PLOT_AREA <- dados_invt$PLOT_AREA * 10000

# Os Inventarios seram feitos por talhao
# O procedimento pode ser feito por codigo genetico / talhao, basta adicionar o grupo a mais

# Caso o objetivo seja importar os dados de inventario:
# tab_invt <- read.csv2("tabelas/tab_invt.csv")

# 1) Amostragem Casual Simples --------------------------------------------------------------------------------------------------------------------------------------

# O inventario e feito coma funcao sprs, que tem como argumentos obrigatorios
# o data frame, a coluna de area total, area da parcela e vwb. os demais sao opcionais.
# segue abaixo alguns exemplos:

data("exfm1")

tab_invt <- exfm1

# vars obrigatorias pop infinita
tab_cs <- sprs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", pop = "inf")
tab_cs

# vars obrigatorias pop infinita + 5 casas decimais
tab_cs <- sprs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", pop = "inf", dec_places = 5)
tab_cs

# vars obrigatorias pop infinita, erro (piloto) 20%
tab_cs <- sprs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", pop = "inf", error = 20)
tab_cs

# vars obrigatorias pop infinita, significancia 0.1
tab_cs <- sprs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", pop = "inf", alpha = 0.1)
tab_cs

# vars obrigatorias pop finita
tab_cs <- sprs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", pop = "fin")
tab_cs

# vars obrigatorias pop finita + idade
#tab_cs <- sprs(tab_invt, "STRATA_AREA", "PLOT_AREA", "VWB","AGE", pop = "fin")
#tab_cs

# vars obrigatorias pop finita + idade + grupos
# um inventario casual simples para cada talhao
tab_cs <- sprs(tab_invt, "STRATA_AREA", "PLOT_AREA", "VWB",.groups = "STRATA", pop = "fin")
tab_cs

# Exemplo: Livro Dendrometria & Inventario Florestal (SOARES, NETO & SOUZA, 2012), pag 163, cap 4.

ex_livro_dendro_ACS_def <- data.frame("STRATA_AREA" = 46.8, 
                                      "PLOT_AREA" = 0.3*10000, 
                                      "VWB" = c(41,33,24,31,10,32,62,16,66,25,44,7,57,22,31,40,43,27,17,50,38,20,35,31,26) )
ex_livro_dendro_ACS_def
ex_livro_dendro_ACS_piloto <- ex_livro_dendro_ACS_def[1:10,]        
ex_livro_dendro_ACS_piloto

sprs(ex_livro_dendro_ACS_piloto, "VWB", "PLOT_AREA", "STRATA_AREA", error = 20, pop = "fin")
sprs(ex_livro_dendro_ACS_def,  "VWB", "PLOT_AREA", "STRATA_AREA", error = 20, pop = "fin")



# 2) Amostragem Casual Estratificada -----------------------------------------------------------------------------------------------------------------------------------

# O inventario e feito coma funcao strs, que tem como argumentos obrigatorios
# o data frame, a coluna de area total, area da parcela, vwb e estrato. os demais sao opcionais.
# a funcao gera uma lista, com dois dataframes, um com os resultados por estrato, e outro com os resultados finais da amostragem.
# segue abaixo alguns exemplos:

data("exfm1")

tab_invt <- exfm1
head(tab_invt)

# vars obrigatorias pop infinita
tab_list_strat <- strs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", pop = "inf")
tab_list_strat

# vars obrigatorias pop infinita + 5 casas decimais
tab_list_strat <- strs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", dec_places = 5)
tab_list_strat


# vars obrigatorias pop infinita, erro (piloto) 20%
tab_list_strat <- strs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", pop = "inf", error = 20)
tab_list_strat

# vars obrigatorias pop infinita, significancia 0.1
tab_list_strat <- strs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", pop = "inf", alpha = 0.1)
tab_list_strat

# vars obrigatorias pop finita
tab_list_strat <- strs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", pop = "fin")
tab_list_strat

# vars obrigatorias pop finita + idade
#tab_list_strat <- strs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", "STRATA", "AGE",pop = "fin")
#tab_list_strat

# Exemplo: Livro Dendrometria & Inventario Florestal (SOARES, NETO & SOUZA, 2012), pag 175, cap 5.

ex_livro_dendro_ACE_piloto <- data.frame("STRATA"     = c( rep(1, 7), rep(2, 8), rep(3, 7) ),
                                         "STRATA_AREA" = c(rep(14.4, 7), rep(16.4, 8), rep(14.2, 7) ),
                                         "PLOT_AREA" = 0.1*10000, 
                                         "VWB" = c(7.9,3.8,4.4,6.25,5.55,8.1,6.1,  10.2,15.25,13.4,13.6,14.2,9.85,10.2,11.55,   10.65,12.15,14.6,10.9,16.55,17.9,13.35 ) )
ex_livro_dendro_ACE_piloto

ex_livro_dendro_ACE_def <- data.frame("STRATA"     = c( rep(1, 14), rep(2, 20), rep(3, 23) ),
                                      "STRATA_AREA" = c(rep(14.4, 14), rep(16.4, 20), rep(14.2, 23) ),
                                      "PLOT_AREA" = 0.1*10000, 
                                      "VWB" = c(7.9,3.8,4.4,6.25,5.55,8.1,6.1,6.6,7.4,5.35,5.9,4.65,4.25,8.25,    10.2,15.25,13.4,13.6,14.2,9.85,10.2,11.55,9.25,11.3,13.95,12.7,10.15,14.9,10.8,11.55,13.9,9.2,12.45,11.9,   10.65,12.15,14.6,10.9,16.55,17.9,13.35,14.9,9.7,15.2,13.45,12.4,14.45,13.55,12.3,15.65,14.2,17.8,14.8,9.35,12.6,13.8,15.85 ) )

ex_livro_dendro_ACE_def

strs(ex_livro_dendro_ACE_piloto, "VWB", "PLOT_AREA", "STRATA_AREA", strata = "STRATA", error = 5, pop = "fin")
strs(ex_livro_dendro_ACE_def, "VWB", "PLOT_AREA", "STRATA_AREA",  strata = "STRATA", error = 5, pop = "fin")
strs(ex_livro_dendro_ACE_def, "VWB", 1000, 14.4,  strata = "STRATA", error = 5, pop = "fin")



# 3) Amostragem Sistematica --------------------------------------------------------------------------------------------------------------------------------------

# O inventario e feito coma funcao strs, que tem como argumentos obrigatorios
# o data frame, a coluna de area total, area da parcela e vwb. os demais sao opcionais.
# o calculo do erro padrao da media e feito utilizando o metodo das diferencas sucessivas,
# portanto, espera-se que os dados estejam orderados.
# segue abaixo alguns exemplos:
data("exfm2")
tab_invt <- exfm2
tab_invt

# vars obrigatorias
tab_as <- ss_diffs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA")
tab_as

# vars obrigatorias + 5 casas decimais
tab_as <- ss_diffs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", dec_places = 5)
tab_as

# vars obrigatorias, erro (piloto) 20%
tab_as <- ss_diffs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", error = 20)
tab_as

# vars obrigatorias, significancia 0.1
tab_as <- ss_diffs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", alpha = 0.1)
tab_as

# vars obrigatorias 
tab_as <- ss_diffs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA")
tab_as

# vars obrigatorias + idade
# tab_as <- ss_diffs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", "AGE")
# tab_as

# vars obrigatorias + grupos
# um inventario casual simples para cada talhao
tab_as <- ss_diffs(tab_invt, "VWB", "PLOT_AREA", "STRATA_AREA", .groups = "STRATA")
tab_as

# Exemplo: Livro Dendrometria & Inventario Florestal (SOARES, NETO & SOUZA, 2012), pag 197, cap 6.

ex_livro_dendro_AS <- data.frame("STRATA_AREA" = 10, 
                                 "PLOT_AREA" = 0.02*10000, 
                                 "VWB" = c(6,8,9,10,13,12,18,19,20,20,24,23,26,30,31,31,33,32) )
ex_livro_dendro_AS

ss_diffs(ex_livro_dendro_AS, "VWB", "PLOT_AREA", "STRATA_AREA")

ss_diffs(ex_livro_dendro_AS, "VWB", 200, 10) # argumentos numericos ok

