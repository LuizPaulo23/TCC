#' @title  Teste de Chow 
#' @author Luiz Paulo Tavares Gonçalves 

rm(list = ls()) # Limpando a memória 

# Definindo diretório de trabalho 
# setwd("~/Área de Trabalho/Ciência de Dados: Machine Learning & Deep Learning/TCC/DBs")

# importando dataset

  coffe_raw = readxl::read_excel("db_coffe.xlsx") |>
              janitor::clean_names() %>% 
              dplyr::mutate(log_price = log(price_quilo_mean_reis), 
                            log_coffe = log(coffe_saca_milhoes), 
                            log_export = log(export_coffe_saca_mil),
                            log_tax = log(tax_cambio),
                            index = case_when(ano >= 1821 & ano <= 1830 ~ "1821-1830 - Antes da Lei Feijó", 
                                              ano >= 1831 & ano <= 1850 ~ "1832-1850 - Após a Lei Feijó", 
                                              ano >= 1851 & ano <= 1870 ~ "1851-1870 - Após a Lei Eusébio de Queiroz", 
                                              ano >= 1871 & ano <= 1887 ~ "1871-1887 - Após a Lei do Ventre Livre", 
                                              ano >= 1888 ~ "Após a Lei Áurea")) %>% 
              dplyr::select(!c(prod_mundial, pct_br)) %>% 
              stats::na.omit() 

# importando exportações de açúcar   
  
    exports_raw = readxl::read_excel("exports.xlsx") %>% 
                  janitor::clean_names() %>% 
                  dplyr::mutate(log_acucar = log(acucar))
# concatenando 
  
  bd_model = merge(coffe_raw, exports_raw, by = "ano") %>% 
             dplyr::select(ano, 
                           log_price, # price_quilo_mean_reis
                           log_acucar,
                           log_export, # export_coffe_saca_mil
                           log_tax,
                           index)

# Teste Normalidade - Shapiro-Wilk 
  
  for (i in bd_model[,2:5]) {
    sw = shapiro.test(i)
    print(sw)
  }

# Visualizando a relação entre as variáveis - bidimensional
  
  ggplot2::ggplot(data = bd_model)+
           aes(x = log_price, y = log_export, colour = index)+
           geom_point(pch = 19, size = 2.0)+
           geom_smooth(method = lm, fill = "black", col = "red")+
           labs(y = "Log das exportações de café", colour = "", 
                x = "Log do preço médio do café", 
                caption = "Fonte: Elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
           theme_bw()+
           theme(legend.position = c(.95, .45),
                 legend.justification = c("right", "top"),
                 legend.box.just = "right",
                 legend.margin = margin(6, 6, 6, 6), 
                 plot.caption = element_text(hjust = 0))+
           guides(fill = guide_legend(nrow = 2, ncol = 3))
  
residuo_model = lm(formula = log_price ~ log_export, data = bd_model)
summary(residuo_model)

shapiro.test(residuo_model$residuals)  # Sem distribuição normal 

# getwd()
  
#ggsave(filename = "regrecontrole.png", width = 9.0, 
#         height = 5, dpi = 1000, units = "in", device = "png")  
  
# Câmbio 
  
  ggplot(bd_model) +
    aes(x = ano, y = log_tax) +
    geom_line(size = 1.0, colour = "red") +
    labs(title = "Log da taxa de câmbio (Mil-Réis por Libra)", 
         x = "", y = "Log mil-réis por libra")+
    theme_bw()
  
# modelo log-log completo incluindo taxa de câmbio -----------------------------------------------------
  
model_complet_log <- parsnip::linear_reg() %>%
                          set_engine("lm") %>%
                          fit(log_export ~ log_price + log_acucar + log_tax, data = bd_model)
  
performance::check_model(model_complet_log)

# Passando o modelo para uma tabela ------------------------------------------------------------------------
# Completo sem incluir taxa de câmbio 

bd_model = bd_model %>% 
           dplyr::rename("Preço médio" = "log_price", 
                         "Exportações de Café" = "log_export", 
                         "Exportações de Açúcar" = "log_acucar", 
                         "Taxa de Câmbio" = "log_tax")

model_final = lm(formula = `Exportações de Café` ~ `Preço médio` + 
                               `Exportações de Açúcar`, 
                    data = bd_model)

stargazer::stargazer(model_final, type = "text", title = "Modelo Log-Log")   
shapiro.test(model_final$residuals) # Tem distribuição normal 

# Perfomance model final 

performance::check_model(model_final)
# Testes de validação -----------------------------------------------------------------------------------------------------------------------------------

test_model = function(model){

    if(!is.null(model)){

    distro_residuals =  stats::shapiro.test(model$residuals) 
    print(distro_residuals)
    
    raiz_unitaria = tsm::gts_ur(model$residuals)
    print(raiz_unitaria)
    
    autocorrelation = performance::check_autocorrelation(model) 
    print(autocorrelation)
    
    colineridade = performance::check_collinearity(model)
    print(colineridade)
    
    heterocedasticidade = performance::check_heteroscedasticity(model) 
    print(heterocedasticidade)
    
    normalidade = performance::check_normality(model) 
    print(normalidade)
    
    outliers = performance::check_outliers(model)
    print(outliers)
    
    singular = performance::check_singularity(model)
    print(singular)
    
    results = performance::model_performance(model)
    print(results)

    }else{
  print("ERRO: model nulo!")
}

}

test_model(model = model_final)

# Plots dos testes -----------------------------------------------------------------------------------------------------------------------------

plot_test <- cbind(bd_model, Residual = model_final$residuals) %>% 
             dplyr::relocate(Residual, .after = ano)

# Matriz de correlação // variáveis do modelo com o resíduos 

    ggpairs(plot_test[, 2:5], lower = list(continuos = "smooth"))+
           ggtitle("")+
           theme_bw()

ggsave(filename = "check_cor_Pearson.png", width = 8.0, 
           height = 8, dpi = 1000, units = "in", device = "png")
    
# Autocorrelação residual e heterocedasticidade     
    
tsm::ac(model_final$residuals, max.lag = 30) # Autocorrelação Residual
# plot(check_heteroscedasticity(model_final)) # Plot. Heterocedasticidade 

# Diferença entre os coeficientes ---------------------------------------------------------------

test_chow_step = function(db_model = as.data.frame(), start, breaking){

    chow_step = bd_model %>% 
                dplyr::mutate(model = ifelse(ano >= start & ano <= breaking, "step_one", 
                                             ifelse(ano >= 1851, "step_two", NA))) %>% 
                       relocate(model, .after = ano)
    
    
    model_step_complet =  lm(formula = `Exportações de Café` ~ `Preço médio` +
                               `Exportações de Açúcar`, data = chow_step)
    print("Modelo completo")
    sw_complet = shapiro.test(model_step_complet$residuals)
    print(sw_complet)
    
    step_one = chow_step %>% 
               filter(model == "step_one") 
    
    model_step_one = lm(formula = `Exportações de Café` ~ `Preço médio` +
                          `Exportações de Açúcar`, data = step_one)
    print("Step one model")
    sw_one = shapiro.test(model_step_one$residuals)
    print(sw_one)
     
    step_two = chow_step %>% 
               filter(model == "step_two")
    
    model_step_two = lm(formula = `Exportações de Café` ~ `Preço médio` +
                          `Exportações de Açúcar`,  data = step_two)
    print("Step two model")
    sw_two = shapiro.test(model_step_two$residuals)
    print(sw_two)

# Resultado modelos 
    
stargazer::stargazer(model_step_complet, model_step_one, model_step_two, 
                      type = "latex", title = "Modelo Log-log")

print("----------------------------------------------------------------")
    
print("----------------------TESTE DE RAIZ UNITÁRIA---------------------")    
print("-----------------------MODELO COMPLETO---------------------------")

raiz_complet = gts_ur(model_step_complet$residuals)
print(raiz_complet)


print("---------------------------one step------------------------------")
raiz_one = gts_ur(model_step_one$residuals)
print(raiz_one)


print("---------------------------two step-------------------------------")
raiz_two = gts_ur(model_step_two$residuals)
print(raiz_two)
    

print("##################################################################")


plot_step_one = ggplot2::ggplot(data = step_one)+
                aes(y = `Exportações de Café`, 
                    x = `Preço médio`)+
                geom_point(pch = 19, size = 2.5)+
                geom_smooth(method = "lm", fill = "red", col = "black")+
                labs(y = "Log das exportações de café", 
                     x = "Log do preço médio do quilo de café",
                     title = "Modelo 1821-1850", 
                     subtitle = "P-valor do teste Shapiro-Wilk para os resíduos = 0.6894")+
                theme_bw()

plot_step_two = ggplot2::ggplot(data = step_two)+
                aes(y = `Exportações de Café`, 
                    x = `Preço médio`)+
                geom_point(pch = 19, size = 2.5)+
                geom_smooth(method = "lm", fill = "red", col = "black")+
                labs(y = "Log das exportações de café", 
                     x = "Log do preço médio do quilo de café",
                     title = "Modelo 1851-1900", 
                     subtitle = "P-valor do teste Shapiro-Wilk para os resíduos = 0.4371")+
                theme_bw()

plots_chow = cowplot::plot_grid(plot_step_one, 
                                plot_step_two, ncol = 2)
print(plots_chow)

ggsave(filename = "modelchow.png", width = 9.0, 
       height = 5, dpi = 1000, units = "in", device = "png")

    return(models = list(model_step_complet, 
                         model_step_one, 
                         model_step_two, 
                         chow_step, 
                         coffe_raw))    


 
} 
 
# Resultado dos modelos ----------------------------------------------------------------------------------------------

results_model = test_chow_step(db_model = db_model, 
                               start = 1821, 
                               breaking = 1850)

# --------------------------------------------------------------------------------------------------------

check_model(results_model[[1]]) # complet 

ggsave(filename = "check_model_complet.png", width = 8.0, 
       height = 8, dpi = 1000, units = "in", device = "png")

check_model(results_model[[2]]) # step_one

ggsave(filename = "check_modelo_1821-1850.png", width = 8.0, 
       height = 8, dpi = 1000, units = "in", device = "png")

check_model(results_model[[3]]) # step_two 

ggsave(filename = "check_model_1850-1900.png", width = 8.0, 
       height = 8, dpi = 1000, units = "in", device = "png")


# Teste de Chow com package -----------------------------------------------------------------------------------------------------------------------

    point_break = bd_model %>% 
                  dplyr::filter(ano >= 1821 & ano < 1851) %>% 
                  count() %>% as.numeric()

print(point_break) # Ponto de quebra - em 1851

bd_model %>% slice(point_break)

    if(!is.null(point_break)){
      test_chow = sctest(`Exportações de Café` ~ `Preço médio` +
                          `Exportações de Açúcar` , 
                          type = "Chow", 
                          point = point_break, 
                          data = bd_model)
      
      print(test_chow)
      
    }

table_chow = data.frame("Estatística F" = c(test_chow$statistic), 
                        "P_valor" = "2.2e-16", 
                        "Point" = c(point_break)) %>% 
              dplyr::rename("Estatística F" = "Estatística.F", 
                            "P-valor" = "P_valor")


kable(table_chow, caption = "Teste de Chow", format = "latex")

## Validação e Robustez dos modelos ----------------------------------------------------------------------------------------------

test_model(model = results_model[[1]])
test_model(model = results_model[[2]])
test_model(model = results_model[[3]])

# Shapiro-Wilk normality test

shapiro.test(results_model[[1]]$residuals)
shapiro.test(results_model[[2]]$residuals)
shapiro.test(results_model[[3]]$residuals)

# Anderson-Darling normality test

ad.test(results_model[[1]]$residuals)
ad.test(results_model[[2]]$residuals)
ad.test(results_model[[3]]$residuals)

# Durbin-Watson test

dwtest(results_model[[1]])
dwtest(results_model[[2]])
dwtest(results_model[[3]])

# studentized Breusch-Pagan test

bptest(results_model[[1]])
bptest(results_model[[2]])
bptest(results_model[[3]])




