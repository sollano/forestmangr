
library(forestr)

data("ex7_mfr")
head(ex7_mfr)

dados_smalian <- smaliancc(ex7_mfr,"ARVORE","di_cc", "hi")
dados_smalian

#' Rodar regressao, e gerar tabela com coeficientes, R2 e erro:
 
dados_smalian %>% 
  do(Reg = lm(log(VCC) ~ log(DAP) + log(HT), data = .  ) ) %>% 
  rowwise %>% 
  transmute(
    bo        = coef(Reg)[[1]],
    b1        = coef(Reg)[[2]],
    b2        = coef(Reg)[[3]],
    Rsqr      = summary(Reg)[[8]],
    Rsqr_adj  = summary(Reg)[[9]],
    Std.Error = summary(Reg)[[6]] ) 

#' ou

lm_table(dados_smalian, log(VCC) ~ log(DAP) + log(HT) )
#' ou
dados_smalian %>% lm_table(log(VCC) ~ log(DAP) + log(HT) )


#' Rodar regressao, e gerar tabela com coeficientes, R2 e erro,
#' e anexar tabela aos dados originais:

head(
  lm_table(dados_smalian, log(VCC) ~ log(DAP) + log(HT), output = "merge" )
)

#' lm_group

#' Rodar regressao, e gerar tabela com coeficientes, R2 e erro por grupo:
 
dados_smalian %>% 
  group_by(TALHAO) %>% 
  do(Reg = lm(log(VCC) ~ log(DAP) + log(HT), data = .  ) ) %>% 
  mutate(
    bo        = coef(Reg)[[1]],
    b1        = coef(Reg)[[2]],
    b2        = coef(Reg)[[3]],
    Rsqr      = summary(Reg)[[8]],
    Rsqr_adj  = summary(Reg)[[9]],
    Std.Error = summary(Reg)[[6]] ) %>% 
  select(-Reg)

#' ou

dados_smalian %>% 
  group_by(TALHAO) %>% 
  lm_table_group(log(VCC) ~  log(DAP) + log(HT) )

#' ou

lm_table_group(dados_smalian, log(VCC) ~  log(DAP) + log(HT), "TALHAO")


#' Rodar regressao, e gerar tabela com coeficientes, R2 e erro,
#' e anexar tabela aos dados originais:
lm_table_group(dados_smalian, log(VCC) ~  log(DAP) + log(HT), "TALHAO", output = "merge")

#' ou
dados_smalian %>% 
  group_by(TALHAO) %>% 
  lm_table_group(log(VCC) ~  log(DAP) + log(HT), output = "merge")

#' Rodar regressao, e gerar dataframe com variaveis agrupadas
dados_smalian %>% 
  group_by(TALHAO) %>% 
  lm_table_group(log(VCC) ~  log(DAP) + log(HT), output = "nest")

