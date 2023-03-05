#' @title  Análise exploratória: café 
#' @author Luiz Paulo Tavares Gonçalves 

rm(list = ls()) # Limpando a memória 

# Definindo diretório de trabalho 
# setwd("~/Área de Trabalho/Ciência de Dados: Machine Learning & Deep Learning/TCC/DBs")
# import dataset 

coffe_raw = readxl::read_excel("db_coffe.xlsx") |>
            janitor::clean_names()

# Check: estrutura dos dados 

coffe_raw %>% plot_intro() 
coffe_raw %>%  plot_histogram() 

# Testando a normalidade das variáveis do dataset - Shapiro-Wilk 

    for (i in coffe_raw[, 2:6]) {
        sw = shapiro.test(i)
        print(sw)
    }

# EDA --------------------------------------------------------------------------------------------------------

    coffe_br = coffe_raw %>% 
               dplyr::select(ano, export_coffe_saca_mil, pct_br) %>% 
                      mutate(median_export = median(export_coffe_saca_mil, na.rm = T), 
                             median_pct = median(pct_br, na.rm = T)) %>% 
               tidyr::pivot_longer(c(export_coffe_saca_mil, # Exportações brasileiras de café em grão
                                     pct_br, median_export, median_pct)) %>% 
                      mutate(name = case_when(name ==  "median_export" ~ "Mediana",
                                              name == "export_coffe_saca_mil" ~ "Exportações", 
                                              name == "pct_br" ~ "Porcentagem", 
                                              name == "median_pct" ~ "Mediana da Porcentagem"))
                

# Visualizando a trajetória - café exportado 
    
    
    coffe_br %>%
      filter(name %in% c("Exportações") & 
             ano >= "1821") %>%
      ggplot() +
      aes(x = ano, y = value, colour = name) +
      geom_line(size = 1.0) +
      geom_text(data = coffe_br %>% filter(name == "Exportações") %>% 
                                    filter(value == last(value)), 
                aes(x = ano, y = value, label = paste0(round(value, 2))), 
                position = position_dodge(width = 1), 
                vjust = 1.3, 
                hjust = 0.5, 
                size = 3.3,
                fontface = "bold")+
      scale_color_manual(values = c(Exportações = "black")) +
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
               y = 10000)+
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
              y = 10000)+
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
              y = 10000)+
      labs(x = "",
           y = "60kg (mil)",
           color = "", 
           caption = "Fonte: elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
      theme_bw()+
      theme(legend.position = "none", 
            plot.caption = element_text(hjust = 0))

# Distribuição das exportações de café - variável dependente 
    
test_dist = coffe_raw %>% 
          dplyr::select(export_coffe_saca_mil) %>% 
                 filter(export_coffe_saca_mil > 0) %>% 
                 mutate(export_coffe_saca_mil = log(export_coffe_saca_mil))

    
# Teste Shapiro-Wilk 

shapiro.test(test_dist$export_coffe_saca_mil)

    ggplot2::ggplot(data = test_dist, 
                    aes(x = log(export_coffe_saca_mil)))+
             geom_histogram(bins = 10, 
                            fill = "black",
                            alpha = 0.5)+
             labs(y = "Contagem", 
                  x = "Log das exportações de café em sacas de 60 kg", 
                  subtitle = "Teste de Shapiro-Wilk p-valor = 0.0003799",
                  caption = "Fonte: elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
             theme(legend.position = "none",
                   plot.caption = element_text(hjust = 0))+
             theme_bw()
    
    
    
  ggsave(filename = "coffe_dist.png", width = 8.0, 
           height = 4, dpi = 1000, units = "in", device = "png")    
    
# Participação brasileira na produção mundial de café     
    
    coffe_br %>%
      filter(name %in% c("Porcentagem", "Mediana da Porcentagem") & 
             ano >= "1852") %>%
      ggplot() +
      aes(x = ano, y = value, colour = name) +
      geom_line(size = 1.0) +
      geom_text(data = coffe_br %>% filter(name == "Porcentagem") %>% 
                                    filter(value == last(value)), 
                aes(x = ano, y = value, label = paste0(round(value, 2), "%")), 
                position = position_dodge(width = 1), 
                vjust = -0.6, 
                hjust = 0.5, 
                size = 3.3,
                fontface = "bold")+
      scale_color_manual(values = c(Porcentagem = "black",
                                    `Mediana da Porcentagem` = "red")) +
      ylim(0,100)+
      labs(x = "",
           y = "%",
           color = "", 
           caption = "Fonte: elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
      theme_bw()+
      theme(legend.position = "bottom", 
            plot.caption = element_text(hjust = 0))

# Autocorrelação das exportações de café 
    
    coffe_time <- coffe_raw %>% 
                  dplyr::select(ano, export_coffe_saca_mil) %>% 
                  stats::na.omit() %>% 
                         ts(export_coffe_saca_mil, 
                            start = 1821, end = 1900, frequency = 1) %>% glimpse()
    
    acf(log(coffe_time[, 2]), 
        type = "correlation",
        col = "red", lag.max =30, xlab = "Lags", 
        main = "")
      
# EDA séries temporais - TSstudio  
    
# ts_plot(coffe_time[, 2])
# ts_lags(coffe_time[, 2], lags = 1:24)    
# ts_cor(coffe_time[, 2], type = "both", seasonal = T, ci = 0.95)  
# ts_ma(coffe_time[, 2])    

# Testando mudança da média 

  coffe_raw = coffe_raw %>% 
              dplyr::mutate(
                            index = case_when(ano >= 1821 & ano <= 1840 ~ "1821-1840", 
                                              ano >= 1841 & ano <= 1860 ~ "1841-1860", 
                                              ano >= 1861 & ano <= 1880 ~ "1861-1880", 
                                              ano >= 1881 & ano <= 1900 ~ "1881-1900")) %>% 
              dplyr::relocate(index, .after = ano)

    media_index = coffe_raw %>% 
                  dplyr::group_by(index) %>% 
                         summarise(media = base::mean(export_coffe_saca_mil, na.rm = T), 
                                   mediana = stats::median(export_coffe_saca_mil, na.rm = T), 
                                   desvio = stats::sd(export_coffe_saca_mil, na.rm = T), 
                                   var = stats::var(export_coffe_saca_mil, na.rm = T), 
                                   min = base::min(export_coffe_saca_mil), 
                                   max = base::max(export_coffe_saca_mil), 
                                   amplitude = round(min-max, 4)) %>% 
                  stats::na.omit()

# Concatenando as médias com a base 
    
    coffe_raw = coffe_raw  %>% 
                dplyr::mutate(valor_index = ifelse(index == "1821-1840", media_index[1,2], 
                                                   ifelse(index == "1841-1860", media_index[2,2], 
                                                          ifelse(index == "1861-1880", media_index[3,2], 
                                                                  ifelse(index == "1881-1900", media_index[4,2], NA)))),
                              valor_index = as.numeric(valor_index)) %>% 
                              relocate(valor_index, .after = ano) 

# Visualizando a transiçao da media 
    
    coffe_raw %>% 
        dplyr::select(ano, 
                      index, 
                      valor_index, 
                      export_coffe_saca_mil) %>% 
        tidyr::pivot_longer(c(valor_index, export_coffe_saca_mil)) %>% 
        dplyr::mutate(name = case_when(name == "valor_index" ~ "Média", 
                                       name == "export_coffe_saca_mil" ~ "Exportações")) %>% 
                rename(type = name) %>% stats::na.omit() %>% 

   
   ggplot() +
     aes(x = ano, y = value, fill = index, colour = type) +
     geom_line(size = 1.0) +
     scale_fill_hue(direction = 1) +
      scale_color_manual(values = c(Exportações = "black",
                                    Média = "red"))+
     labs(y = "60kg (mil)", x = "", colour = "", 
          caption = "Fonte: elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
     theme_bw() +
     theme(legend.position = "bottom", 
           plot.caption = element_text(hjust = 0))
   
   
# Mediana, min-max 
    
    
    plot_break = coffe_raw  %>% 
                dplyr::mutate(mediana = ifelse(index == "1821-1840", media_index[1,3], 
                                                   ifelse(index == "1841-1860", media_index[2,3], 
                                                          ifelse(index == "1861-1880", media_index[3,3], 
                                                                 ifelse(index == "1881-1900", media_index[4,3], NA)))),
                              mediana = as.numeric(mediana)) %>% 
                        mutate(minimo = ifelse(index == "1821-1840", media_index[1,6], 
                                                ifelse(index == "1841-1860", media_index[2,6], 
                                                       ifelse(index == "1861-1880", media_index[3,6], 
                                                               ifelse(index == "1881-1900", media_index[4,6], NA)))),
                      minimo = as.numeric(minimo)) %>% 
                        mutate(maximo = ifelse(index == "1821-1840", media_index[1,7], 
                                              ifelse(index == "1841-1860", media_index[2,7], 
                                                    ifelse(index == "1861-1880", media_index[3,7], 
                                                          ifelse(index == "1881-1900", media_index[4,7], NA)))),
                       maximo = as.numeric(maximo))
 
   
    plot_break %>% 
      dplyr::select(ano, 
                    index, 
                    mediana, 
                    export_coffe_saca_mil) %>% 
      tidyr::pivot_longer(c(mediana, export_coffe_saca_mil)) %>% 
      dplyr::mutate(name = case_when(name == "mediana" ~ "Mediana",
                                     name == "export_coffe_saca_mil" ~ "Exportações")) %>% 
      rename(type = name) %>% stats::na.omit() %>% 
      
      
      ggplot() +
      aes(x = ano, y = value, fill = index, colour = type) +
      geom_line(size = 1.0) +
      scale_fill_hue(direction = 1) +
      scale_color_manual(values = c(Exportações = "black",
                                    Mediana = "red"))+
      labs(y = "60kg (mil)", x = "", colour = "", 
           caption = "Fonte: elaboração do autor (2022) com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
      theme_bw() +
      theme(legend.position = "bottom", 
            plot.caption = element_text(hjust = 0))
    
    