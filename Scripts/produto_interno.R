# Produto interno 

rm(list = ls()) # limpando a memória 

# import dataset 

pib_raw = readxl::read_excel("Maddison_database.xlsx")

# Limpando e organizando o dataset 

db_pib = pib_raw %>% 
         janitor::clean_names() %>% 
         tidyr::pivot_longer(cols = brasil:peru, 
                             names_to = "country") %>% 
         dplyr::mutate(index = case_when(country == "brasil" ~ "Brasil", 
                                         country == "chile" ~ "Chile", 
                                         country == "peru" ~ "Peru"))

# Visualizando 

    ggplot(db_pib) +
     aes(x = ano, y = value, colour = index) +
     geom_line(size = 1.0) +
     scale_color_manual(values = c(Brasil = "red", 
                                   Chile = "black", 
                                   Peru = "#a1a1a1")) +
      annotate("rect", 
               xmin = 1850, 
               xmax = 1851, 
               ymin = 0, 
               ymax = Inf, 
               fill = "blue", 
               alpha = 0.20)+
      annotate("text", 
               label = "Lei Eusébio de Queiroz",
               x = 1855, 
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
     labs(y = "PIB per capita Real (U$ 2011)", 
          x = "", colour = "", 
          caption = "Fonte: Elaboração do autor (2022) com base nos dados do Maddison Project Database (2020)")+
      ylim(0,4000)+
     theme_bw() +
     theme(legend.position = "bottom", 
           plot.caption = element_text(hjust = 0))

# População 
    
    pib_raw %>% 
              janitor::clean_names() %>% 
              dplyr::select(ano, pop_brasil) %>% 
              stats::na.omit() %>% 
    ggplot() +
     aes(x = ano, y = pop_brasil) +
     geom_point(col = "red", size = 2.5)+
     geom_line(size = 1.0, colour = "black") +
     labs(y = "Em milhões", x = "", 
          caption = "Fonte: Elaboração do autor (2022) com base nos dados do Maddison Project Database (2020)")+
     theme_bw()+
     theme(plot.caption = element_text(hjust = 0))
    
    
    
