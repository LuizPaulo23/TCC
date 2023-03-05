# Análise exploratória: Exportações 
#' @author Luiz Paulo Tavares Gonçalves 

rm(list = ls()) # Limpando a memória 

# Definindo diretório de trabalho 
# setwd("~/Área de Trabalho/Ciência de Dados: Machine Learning & Deep Learning/TCC/DBs")
# import dataset 

exports_raw = readxl::read_excel("exports.xlsx")

# Limpando e organizando o dataset 

    db_exports = exports_raw %>% 
                 janitor::clean_names() %>% 
                 dplyr::mutate(erva_mate = ifelse(is.na(erva_mate), 0, erva_mate)) %>% 
                 tidyr::pivot_longer(cols = acucar:erva_mate, 
                                     names_to = "pautas") %>% 
                 dplyr::mutate(index = case_when(pautas == "acucar" ~ "Açúcar", 
                                                 pautas == "algodao" ~ "Algodão", 
                                                 pautas == "cacau" ~ "Cacau", 
                                                 pautas == "tabaco" ~ "Tabaco", 
                                                 pautas == "couros_e_peles" ~ "Couros e Peles", 
                                                 pautas == "erva_mate" ~ "Erva-mate"), 
                              value = round(value/100.000, 2)) %>% 
                dplyr::select(-pautas)

# Visualizando - sem incluir as exportaçoes de açucar 

db_exports %>%
      filter(!(index %in% c("Açúcar"))) %>%
      ggplot() +
      aes(x = ano, y = value, colour = index) +
      geom_line(size = 1.0) +
      scale_color_viridis_d(option = "inferno", direction = 1)+ 
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
               y = 800)+
      annotate("rect", 
               xmin = 1850, 
               xmax = 1851, 
               ymin = 0, 
               ymax = Inf, 
               fill = "blue", 
               alpha = 0.20)+
      annotate("text", 
               label = "Lei Eusébio de Queiroz",
               x = 1851, 
               y = 800)+
      annotate("rect", 
               xmin = 1888, 
               xmax = 1889, 
               ymin = 0, 
               ymax = Inf, 
               fill = "blue", 
               alpha = 0.20)+
      annotate("text", 
               label = "Lei Áurea",
               x = 1888, 
               y = 800)+
      labs(x = "",
           y = "Toneldas (em cem mil)",
           color = "", 
           caption = "Fonte: elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
      theme_bw()+
      theme(legend.position = "bottom", 
            plot.caption = element_text(hjust = 0))
      
# Visualizando apenas exportações de açúcar 

    db_exports %>%
      filter(index == "Açúcar") %>%
      ggplot() +
      aes(x = ano, y = value, colour = index) +
      geom_line(size = 1.0) +
      scale_color_viridis_d(option = "inferno", direction = 1)+ 
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
               y = 4000)+
      annotate("rect", 
               xmin = 1850, 
               xmax = 1851, 
               ymin = 0, 
               ymax = Inf, 
               fill = "blue", 
               alpha = 0.20)+
      annotate("text", 
               label = "Lei Eusébio de Queiroz",
               x = 1851, 
               y = 4000)+
      annotate("rect", 
               xmin = 1888, 
               xmax = 1889, 
               ymin = 0, 
               ymax = Inf, 
               fill = "blue", 
               alpha = 0.20)+
      annotate("text", 
               label = "Lei Áurea",
               x = 1888, 
               y = 4000)+
      ylim(0,4000)+
      labs(x = "",
           y = "Toneldas (em cem mil)",
           color = "", 
           caption = "Fonte: elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
      theme_bw()+
      theme(legend.position = "none", 
            plot.caption = element_text(hjust = 0))
    
    
        
        
        