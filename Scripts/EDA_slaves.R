#' @title  Desembarque de escravos 
#' @author Luiz Paulo Tavares Gonçalves 

rm(list = ls()) # Limpando a memória 

# Definindo diretório de trabalho 
# setwd("~/Área de Trabalho/Ciência de Dados: Machine Learning & Deep Learning/TCC/DBs")

# import dataset 
slave_raw = readxl::read_excel("data_slave.xls")

# Organizando e limpando o dataset 

    db_slave <- slave_raw %>% 
                janitor::clean_names() %>% 
                dplyr::select(time, 
                              totals,
                              south_east_brazil) %>% 
                tidyr::pivot_longer(c(totals, south_east_brazil)) %>% 
                       mutate(index = case_when(name == "totals" ~ "Total", 
                                                name == "south_east_brazil" ~ "Sudeste"), 
                              time = as.numeric(time))

db_slave %>% glimpse()

# Visualizando 
    
    db_slave %>%
      filter(time >= 1800L & time <= 1851L) %>%
      ggplot() +
      aes(x = time, y = value, colour = index) +
      geom_line(size = 1.0) +
      scale_color_manual(values = c(Sudeste = "black",
                                    Total = "red", 
                                    Mediana = "blue")) +
      geom_hline(yintercept = median(db_slave$value), col = "blue", lty= "longdash") + # linha horizontal da mediana
      annotate("rect", 
               xmin = 1831, 
               xmax = 1832, 
               ymin = 0, 
               ymax = Inf, 
               fill = "blue", 
               alpha = 0.20)+
      annotate("text", 
               label = "Lei Feijó",
               x = 1831, 
               y = 80000)+
      annotate("rect", 
               xmin = 1850, 
               xmax = 1851, 
               ymin = 0, 
               ymax = Inf, 
               fill = "blue", 
               alpha = 0.20)+
      annotate("text", 
               label = "Lei Eusébio de Queiroz",
               x = 1848, 
               y = 80000)+
      labs(y = "", x = "", colour = "", 
           caption = "Fonte: elaboração do autor (2022) com base nos dados da Database Slave Voyages")+
      ylim(0,80000)+
      theme_bw() +
      theme(legend.position = "bottom", 
            plot.caption = element_text(hjust = 0))
    
# Comparando a média entre as regiões ---------------------------------------------------------------------------------------
# Organizando para modelo ANOVA 
    
    db_anova = slave_raw %>% 
              janitor::clean_names() %>% 
              dplyr::relocate(south_east_brazil, .after = time) %>% 
              tidyr::pivot_longer(cols = south_east_brazil:totals, 
                                  names_to = "regions") %>% 
              dplyr::mutate(index = case_when(regions == "amazonia" ~ "Amazônia", 
                                              regions == "bahia" ~ "Bahia", 
                                              regions == "pernambuco" ~ "Pernambuco", 
                                              regions == "south_east_brazil" ~ "Sudeste", 
                                              regions == "brazil_unspecified" ~ "Região desconhecida", 
                                              regions == "totals" ~ "Total")) %>% 
                    filter(!c(index == "Total")) %>% rename("Desembarque de escravos" = "value", 
                                                            "Regiões" = "index") 
# Ordenando factor 
    
    db_anova = db_anova %>% 
               dplyr::mutate(Regiões = factor(Regiões, levels = c("Sudeste", 
                                                       "Pernambuco", 
                                                       "Bahia", 
                                                       "Região desconhecida", 
                                                       "Amazônia")))
    
# Modelo Anova     
    
modelo_anova = lm(formula = `Desembarque de escravos` ~ factor(Regiões), data = db_anova)
summary(modelo_anova)

# Tabela 
    
stargazer::stargazer(modelo_anova, type = "text", title = "Modelo ANOVA: comparando a média entre as regiões")
    
# Visualizando os resíduos 
    
shapiro.test(modelo_anova$residuals) # Sem normalidade 
plot(modelo_anova, 2) # Normal Q-Q

acf(modelo_anova$residuals, col = "red", xlab = "Lags",
    main = "Autocorrelação dos Resíduos") # Resíduos autocorrelacionados 

# Plot dos coeficientes 

coeficientes <- broom::tidy(modelo_anova, conf.int = T) # Salvando os coeficientes 

coeficientes$term = factor(coeficientes$term, 
                            levels= c("(Intercept)", 
                                      "factor(Regiões)Pernambuco", 
                                      "factor(Regiões)Bahia", 
                                      "factor(Regiões)Região desconhecida", 
                                      "factor(Regiões)Amazônia"), 
                            labels = c("Sudeste", "Pernambuco", "Bahia", 
                                       "Regiões desconhecidas", "Amazônia"))

# Visualizando  

    ggplot(coeficientes, aes(term, estimate))+
      geom_point(size = 3.0)+
      geom_pointrange(aes(ymin = conf.low, ymax = conf.high), 
                      col = "red", 
                      fill = "red")+
      labs(y = "Coeficientes com intervalo de confiança 5%", x = "",
           title = "Modelo ANOVA: Desembarque de escravos entre as regiões do Brasil", 
           subtitle = "Base de referência: Sudeste (1800-1851)", 
           caption = "Fonte: Elaboração e cálculo do autor (2022)")+
      coord_flip()+
      theme_bw()+
      theme(plot.caption = element_text(hjust = 0))
    

        
        