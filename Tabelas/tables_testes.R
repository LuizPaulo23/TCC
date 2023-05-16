#' @title Teste de robustez - separados 
#' @author Luiz Paulo T. Gonçalves 

# Salvando base de dados -------------------------------------------------------

# db_robustez <- bd_model
# 
# saveRDS(db_robustez, "db_robustez.rds")
# getwd()

rm(list = ls())

setwd("~/Github/Projetos/TCC/DBs")

# Importando e renomeando as colunas 

db_robustez = read_rds("db_robustez.rds") %>% 
              dplyr::rename("Preço médio" = "log_price", 
                            "Exportações de Café" = "log_export", 
                            "Exportações de Açúcar" = "log_acucar", 
                            "Taxa de Câmbio" = "log_tax")

# Filtrando os períodos definidos no teste de Chow -----------------------------

model_dois = db_robustez %>% 
             dplyr::filter(ano < 1851)

model_tres = db_robustez %>% 
                dplyr::filter(ano >= 1851)


# Modelos subdivididos ---------------------------------------------------------

model_step_complet =  lm(formula = `Exportações de Café` ~ `Preço médio` +
                           `Exportações de Açúcar`, data = db_robustez)

model_step_dois =  lm(formula = `Exportações de Café` ~ `Preço médio` +
                        `Exportações de Açúcar`, data = model_dois)


model_step_tres =  lm(formula = `Exportações de Café` ~ `Preço médio` +
                        `Exportações de Açúcar`, data = model_tres)


# Teste de Shapiro-Wilk para o resíduo de cada período/modelo 

shapiro.test(model_step_complet$residuals)
shapiro.test(model_step_dois$residuals)
shapiro.test(model_step_tres$residuals)

# install.packages("nortest")
require(nortest)

# Kolmogorov-Smirnov
lillie.test(model_step_complet$residuals)
lillie.test(model_step_dois$residuals)
lillie.test(model_step_tres$residuals)

# Anderson-Darling
ad.test(model_step_complet$residuals)
ad.test(model_step_dois$residuals)
ad.test(model_step_tres$residuals)

# Presença de heterocedasticidade
# studentized Breusch-Pagan test
library(lmtest)

bptest(model_step_complet)
bptest(model_step_dois)
bptest(model_step_tres)

# durbinWatsonTest(model_step_complet)
