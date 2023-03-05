# Teste Anova - comparando a médias entre as quebras - Exportações 
# Esse Script é depedente da função (test_chow_step) desenvolvida no script test_chow 
# Chamando a função 
results_model = test_chow_step(db_model = db_model, start = 1821, breaking = 1850)

# Filtrando apenas o dataset limpo e organizando 
    anova_raw = results_model[[4]] %>% 
                mutate(points_break = case_when(model == "step_one" ~ "1821-1850", 
                                          model == "step_two" ~ "1851-1900"))  
class(anova_raw)

# Modelando: ANOVA 
model_anova = lm(formula = `Exportações de Café` ~ factor(points_break), data = anova_raw)

stargazer::stargazer(model_anova, type = "text", 
            title = "Modelo ANOVA: comparando as exportações de Café entre as quebras")

# interpretação: semilogarítmicas 
# tomar o antilogaritmo 
# 6.645 = 768,54 // exportaçoes medianas para o perido 1821-1850
# 1.6421 + 6.6445 = 8,2866 =  3970,312158757 // Exportações medianas 1851-1900
tax = ((3970.312158757-768.54)/3970.312158757)*100
# As exportações medianas de 1821-1850 são cerca 80.64% mais baixas comparadas com o período de 1851-1900 

# Semielasticidade 
# antilogaritmo 
# 1.642 = 5,166006743
semi_log = (5.166006743 - 1)*100

# As exportações medianas de 1851-1900 é chega de 416% mais elevado que nos período de 1821-1850

summario_mediana = anova_raw %>% 
                   dplyr::group_by(points_break) %>% 
                          summarise(mediana = median(`Exportações de Café`, na.rm = T), 
                                    media = mean(`Exportações de Café`, na.rm = T))

print(summario_mediana)
                  
shapiro.test(model_anova$residuals)
check_model(model_anova)

# ANOVA SEM SER EM LOG-LOG - MODELO NOMINAL 

anova_nominal_raw = results_model[[5]] %>% 
                    dplyr::select(ano, export_coffe_saca_mil) %>% 
                           mutate(points_break = case_when(ano >= 1821 & ano <= 1850 ~ "1821-1850", 
                                                           ano >= 1851 & ano <= 1900 ~ "1851-1900"))

# ANOVA nominal 

model_anova_nominal = lm(formula = export_coffe_saca_mil ~ factor(points_break), data = anova_nominal_raw)
summary(model_anova_nominal)


shapiro.test(model_anova_nominal$residuals)
check_model(model_anova_nominal)


