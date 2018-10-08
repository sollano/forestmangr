dados <- readxl::read_excel("dados.xlsx")
head(dados)

names(dados) <- c("talhao","parcela","Id","Hd1")
library(tidyverse)
# Ajustar modelo linear de schummacher (Não tem que escrever os betas)
# Função do pacote, já dá os coeficientes direto
sch <- lm_table(dados, log(Hd1) ~ inv(Id) )
sch
lm_table()
dados %>%
  group_by(talhao) %>% 
  nest() %>% 
  mutate(Reg = map(data, ~try(nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, ., 
                                   start = c( b0=23, b1=0.03, b2 = 1.3  ) ) )),
         Coefs = map(Reg, ~try(tidy_(.))) ) %>% 
  unnest(Coefs)


tidy_ <- function(x) {
  tibble(b = broom::tidy(x)$term, estimate = broom::tidy(x)$estimate) %>% 
    mutate(b = factor(b, labels = 0:(length(b) - 1))) %>% 
    tidyr::spread(b, estimate, sep = "")
}

safe_nls <- safely(nls)
q_nls <-  quietly(nls)
pos_tidy <- possibly(tidy_, data.frame(1))
pos_resid <- possibly(resid, data.frame(1))

dados$A <- "dummy"

dados %>%
  group_by(talhao) %>% 
  nest() %>% 
  mutate(Reg = map(data, ~safe_nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, ., 
                                    start = c( b0=23, b1=0.03, b2 = 1.3  ) )[[1]] ),
         Coefs = map(Reg, pos_tidy)
  ) %>% unnest(Coefs, .drop = T) %>% select(-X1)


nls( Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, dados,
     start = c( b0=23, b1=0.03, b2 = 1.3  ) )

library(forestr)

nls_table(dados, Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, 
          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ) )

nls_table(dados,Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, 
          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),
          "talhao")

nls_table(dados,Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, 
          mod_start = c( b0=23, b1=0.03, b2 = 1.3  ),
          "talhao",
          replace = T )

tab_coef <- data.frame(talhao = c(1:20, 24,25,27,28,30,31,33,35,36,37), 
                       rbind(data.frame(b0 = rep(23, 20), b1 = rep(0.03, 20), b2 = rep(1.3, 20) ), 
                             data.frame(b0 = rep(23, 10), b1 = rep(0.03, 10), b2 = rep(.5, 10) )  )  )


nls_table(dados, Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, 
          mod_start = tab_coef ,
          "talhao",
          replace = F )


nls_table(dados, Hd1 ~ b0 * (1 - exp(1)^( -b1 * Id )  )^b2, 
          mod_start = tab_coef ,
          "talhao",
          replace = T )

