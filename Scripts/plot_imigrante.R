#' @title Plot imigrantes no Brasil 
#' @author Luiz Paulo Tavares Gonçalves 

rm(list = ls()) # Limpando a memória 

# Definindo diretório de trabalho 
# setwd("~/Área de Trabalho/Ciência de Dados: Machine Learning & Deep Learning/TCC/DBs")

# import dataset e organizando dataset 

imigrantes_raw = readxl::read_excel("db_pop_imigrantes.xlsx") %>% 
                 dplyr::filter(time >= 1821 & time <= 1900) %>% 
                 dplyr::mutate(index = case_when(time >= 1821 & time <= 1830 ~ "1821-1830", 
                                                 time >= 1831 & time <= 1840 ~ "1831-1840", 
                                                 time >= 1841 & time <= 1850 ~ "1841-1850", 
                                                 time >= 1851 & time <= 1860 ~ "1851-1860", 
                                                 time >= 1861 & time <= 1870 ~ "1861-1870", 
                                                 time >= 1871 & time <= 1880 ~ "1871-1880", 
                                                 time >= 1881 & time <= 1890 ~ "1881-1890", 
                                                 time >= 1891 & time <= 1900 ~ "1891-1900")) %>% 
                       group_by(index) %>% 
                       summarise(pop_imigrante = sum(pop)) %>% 
                dplyr::ungroup() %>% 
                       mutate(pop_imigrante = (pop_imigrante/1000), 
                              pop_imigrante = round(pop_imigrante, 1))

# Visualizando 

ggplot(imigrantes_raw) +
  aes(x = index, y = pop_imigrante) +
  geom_col(fill = "black", alpha = 0.8) +
  geom_text(aes(label = pop_imigrante), 
            vjust = -0.5, size = 3.5)+
  labs(y = "Quantidade (em mil)", x = "", 
       caption = "Fonte: elaboração e cálculo do autor com base nos dados do Instituto de Pesquisa Econômica Aplicada (IPEA)")+
  ylim(0,1200)+
  theme_bw()+
  theme(plot.caption = element_text(hjust = 0))



