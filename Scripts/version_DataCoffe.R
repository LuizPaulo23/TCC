


rm(list = ls()) # Limpando a memória 

# instalações Github 
# Packages

library(devtools) # dev. 
library(tidyverse) # Manipulation
library(DataExplorer) # Análise exploratória 
library(strucchange) # Quebra 
library(forecast) # Time Series 
library(Hmisc) # Plotes 
library(econocharts) # Plotes demanda e oferta 
library(ggplot2)

# import database slaves 

data_slave_raw = readxl::read_excel("data_slave.xls", 
                                    col_types = c("numeric", "numeric", "numeric", 
                                                  "numeric", "numeric", "numeric", 
                                                  "numeric")) %>% 
                 janitor::clean_names() %>% 
                 dplyr:: rename("Sudeste" = "south_east_brazil", 
                                "Desconhecida" = "brazil_unspecified", 
                                "Total" = "totals") %>% 
                 dplyr::filter(time >= 1800 & time <= 1851) %>%
                 tidyr::pivot_longer(cols = amazonia:Total, 
                                     names_to = "Regiões")
          

# Plot - time series 
  
esquisse::esquisser(data_slave_raw)


library(dplyr)
library(ggplot2)

data_slave_raw %>%
 filter(Regiões %in% c("Total", "Sudeste")) %>%
 ggplot() +
 aes(x = time, y = value, colour = Regiões) +
 geom_line(size = 1.0) +
 scale_color_manual(values = c(Sudeste = "red", Total = "black")) +
  ylim(0,80000)+
 labs(y = "Número de desembarcados", x = "",
      caption = "Fonte: elaboração do autor (2022) com base nos dados da The Trans-Atlantic Slave Trade Database ") +
 theme_bw() +
 theme(legend.position = "bottom", plot.caption = element_text(size = 10L, hjust = 0))

# Salvar plote 

ggsave(filename = "demand_suppy_slave.png", width = 8.0, 
       height = 5, dpi = 1000, units = "in", device = "png")

# Plote demanda e oferta por mão de obra 

# Custom data
supply1 <- data.frame(x = c(1, 7), y = c(1, 8))
demand1 <- data.frame(x = c(7, 2), y = c(2, 7))
supply2 <- data.frame(x = c(2, 7), y = c(1, 7))

sdcurve(
  demand1,
  supply2, 
  generic = T,
  curve_names = T,
  equilibrium = T, 
  ymax = 8,
  xmax = 8,
  names = c("D[1]", "S[1]","D[2]", "S[2]"), 
  bg.col = "white")+
  geom_path(
    data = supply1, color = "red", size = 1, 
    linetype = "dashed", alpha = 0.8)+
  annotate("segment", x = 4.73, xend = 4.5, 
           y = 4.5, yend = 5, size = 0.5,
           arrow = arrow(length = unit(0.5, "lines")), colour = "grey50")+
  labs(x = "Quantidade", y = "Preço", 
       caption = "Fonte: elaboração do autor (2022).") +
  coord_equal() +
  theme_classic(base_family = "Source Sans Pro")+
  theme(
    plot.title = element_text(family = "Source Sans Pro Semibold", 
                              size = rel(1.3)), 
    legend.position = "bottom", plot.caption = element_text(size = 10L, hjust = 0))


# Salvar plote 

ggsave(filename = "plot_demand_suppy_slave.png", width = 8.0, 
       height = 5, dpi = 1000, units = "in", device = "png")




data_imigrantes = data.frame(date = c("1821-1830", "1831-1840","1841-1850",
"1851-1860","1861-1870","1871-1880","1881-1890","1891-1900"), 
n = c(7.423, 2.838, 6.795,121.747, 97.482,219.128,525.086, 1129.315))

      
ggplot(data = data_imigrantes) +
      aes(x = date, y = n) +
      geom_col(fill = "red", alpha = 0.8) +
      labs(x = "", y = "Quantidade (em mil)", 
           caption = "Fonte: elaboração do autor (2022) com base nos dados do IPEA.")+
      ylim(0,1200)+
      theme_bw()+
      theme(legend.position = "bottom", plot.caption = element_text(size = 10L, hjust = 0))


ggsave(filename = "plot_imigrantes.png", width = 8.0, 
       height = 5, dpi = 1000, units = "in", device = "png")

# Plote - Composição dos desembarques de escravos

data_slave %>% glimpse()

data_slave = data_slave %>% 
  pivot_longer(cols = Amazonia:Desconhecida, 
               names_to = "Regions")


ggplot(data = data_slave)+
  aes(x = value, fill = Regions)+
  geom_boxplot()+
  coord_flip()+
  scale_fill_manual(
    values = c(Amazonia = "#4B4952",
               Bahia = "#1F2024",
               Desconhecida = "#79717A",
               Pernambuco = "#619CFF",
               Sudeste = "black"))+
  labs(y = "Densidade", x = "", fill = "Regiões")+
  theme_bw()+
  theme(legend.position = "bottom")
  


# Custom data
supply1 <- data.frame(x = c(1, 7), y = c(1, 8))
demand1 <- data.frame(x = c(7, 2), y = c(2, 7))
supply2 <- data.frame(x = c(2, 7), y = c(1, 7))

sdcurve(
        demand1,
        supply2, 
        generic = T,
        curve_names = T,
        equilibrium = T, 
        ymax = 8,
        xmax = 8,
        names = c("D[1]", "S[1]","D[2]", "S[2]"), 
        bg.col = "white")+
        geom_path(
                  data = supply1, color = "pink", size = 1, 
                  linetype = "dashed", alpha = 9.0)+
        annotate("segment", x = 4.73, xend = 4.5, 
                 y = 4.5, yend = 5, size = 0.5,
                 arrow = arrow(length = unit(0.5, "lines")), colour = "grey50")+
        labs(x = "Quantidade", y = "Preço") +
        coord_equal() +
        theme_classic(base_family = "Source Sans Pro")+
        theme(
              plot.title = element_text(family = "Source Sans Pro Semibold", 
                                        size = rel(1.3)))



# import Dataset 
data_base = read.csv("data_coffe.csv")

data_base = data_base %>% rename(
  "export_coffe" = "Exportações...café.em.grão...quantidade...Sacas.de.60.kg...Outras.fontes..inclusive.compilação.de.vários.autores...HIST_XCAFEQ", 
  "time" = "Data") %>% 
  select(!c(X)) %>%
  filter(time >= 1821 & time <= 1851)

db_test <- merge(data_base, data_slave, by = "time") %>% 
        pivot_longer(cols = Amazonia:Sudeste, 
                     names_to = "reg") %>% 
        select(time, export_coffe, value)


ggplot2::ggplot(data = db_test)+
  aes(y = log(export_coffe), x = log(value))+
  geom_point()+
  geom_smooth(method = "lm",col = "red")


esquisse::esquisser(db_test)

data_base$roll_mean = rollmean(data_base$export_coffe, 12, fill = NA, align = "right")

data_time = ts(data = db_test[, 2:3],
               start = 1821, 
               end = 1851, 
               frequency = 1)

# Análise Exploratória 

autoplot(data_time)+
  labs(y = "Sacas de 60kg (mil)", x = "", fill = "")+
  theme(legend.position = "bottom", 
        legend.title = element_blank())

data_time %>% 
acf(lag.max = 72, col = "red")+
  labs( title = "ACF - Exportações de Café (1821-1900)", 
    xlab = "Lags")

MFG_bp <- breakpoints(data_time[, 1] ~ 1)
MFG_ci <- confint(MFG_bp)
plot(MFG)
lines(MFG_bp)
lines(MFG_ci)

confid <- confint(Z,breaks = 1)
plot(U)
lines(Z)
lines(confid)

one = diff(data_time[, 1])
two <- diff(data_time[, 2])
Z <- breakpoints(data_time[, 1] ~ 3)
summary(Z) 


plot(Z)


confid <- confint(Z,breaks = 1)
plot(U)
lines(Z)
lines(confid)


fac.ri <- breakfactor(Z, breaks = 1)
fm.ri <- lm(U ~ 0 + fac.ri)
plot(U)
lines(fitted(Z, breaks = 1), col = 2)
