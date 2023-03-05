# Teste de Raiz unitária
rm(list = ls()) # limpando a memória 
graphics.off()
# import dataset 

coffe_raw = readxl::read_excel("db_coffe.xlsx")

# Limpando e organizando dataset 

data_coffe = coffe_raw %>% 
           janitor::clean_names() %>% 
           dplyr::select(ano, export_coffe_saca_mil) %>% 
           stats::na.omit()

exp_coffe = data_coffe %>% 
           stats::ts(start = 1821, 
                     end = 1900, 
                     frequency = 1)

# Dickey-Fuller aumentado

ts_plot(exp_coffe[, 2])

ac(exp_coffe[, 2], max.lag = 30, 
   main = "Autocorrelação: Exportações de café")

gts_ur(exp_coffe[, 2]) # Teste ADF 
# Test ZA

test.za_intercept <- ur.za(exp_coffe[, 2], model = "intercept")
test.za_trend <- ur.za(exp_coffe[, 2], model = "trend")
test.za_both <- ur.za(exp_coffe[, 2], model = "both")

summary(test.za_intercept)
data_coffe %>% slice(76)

summary(test.za_trend)
data_coffe %>% slice(73)

summary(test.za_both)
data_coffe %>% slice(72)

# Visualizando 

plot(test.za_intercept)
plot(test.za_trend)
plot(test.za_both)

# Autocorrelação dos resíduos 

Acf(test.za_intercept@res, type = "correlation", lag.max = 24)
Acf(test.za_trend@res, type = "correlation", lag.max = 24)
Acf(test.za_both@res, type = "correlation", lag.max = 24)





