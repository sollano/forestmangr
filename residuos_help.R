data("ex10_mfr")
dados_hd <- ex10_mfr 

tabcoef_ht1 <- dados_hd %>% # dataframe q sera utilizado
  group_by(TALHAO) %>% 
  lm_table_group( log(HT) ~ inv(DAP) + log(HD) )
tabcoef_ht1 

tabcoef_ht2 <- dados_hd %>% # dataframe q sera utilizado
  group_by(TALHAO ) %>% 
  lm_table_group( log(HT) ~ log( DAP ) )
tabcoef_ht2

tabcoef_ht3 <- dados_hd %>% # dataframe q sera utilizado
  group_by(TALHAO ) %>% 
  lm_table_group( log(HT) ~ inv( DAP ) )
tabcoef_ht3

dados_res <- dados_hd %>% 
  na.omit %>% 
  left_join(tabcoef_ht1) %>% 
  mutate(HT_EST1 = exp(b0 + b1 * inv(DAP) + b2 * log(HD) ) ) %>% 
  select(-b0,-b1, -b2, -Rsqr, -Rsqr_adj, -Std.Error) %>% 
  left_join(tabcoef_ht2) %>% 
  mutate(HT_EST2 = exp(b0 + b1 * log(DAP) ) ) %>% 
  select(-b0,-b1, -Rsqr, -Rsqr_adj, -Std.Error) %>% 
  left_join(tabcoef_ht3) %>% 
  mutate(HT_EST3 = exp(b0 + b1 * inv(DAP) ) ) %>% 
  #mutate(erro1 = ((HT_EST1 - HT)/HT)*100) %>% 
  #filter(erro1 < 50) %>% 
  select(TALHAO, PARCELA, contains("HT")  ) 
  
head(dados_res)

ex11_mfr <- dados_res
#save(ex11_mfr, file = "D:/Documents/R/packages/forestr/data/ex11_mfr.rda")


data("ex11_mfr")
residuos(ex11_mfr, "HT", "HT_EST1")

residuos(ex11_mfr, "HT", "HT_EST1", type = "scatterplot")
residuos(ex11_mfr, "HT", "HT_EST1", type = "histogram")
residuos(ex11_mfr, "HT", "HT_EST1", type = "versus")

residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", type = "scatterplot")
residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", type = "histogram")
residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", type = "versus")

residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", "HT_EST3")
residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", "HT_EST3", lim_y = 80)
residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", "HT_EST3", lim_y = 40)



head( residuos(ex11_mfr, "HT", "HT_EST1", "HT_EST2", res_table = T) )

