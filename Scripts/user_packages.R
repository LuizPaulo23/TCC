# Dependência de reprodutividade
#' @author Luiz Paulo Tavares Gonçalves 
# Packages/bibliotecas usadas 

# Função para instalação/liberação ------------------------------------------------------
rm(list = ls())
graphics.off()
  
# Set 

    library.packages <- function(pkg){
                        new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
                        if (length(new.pkg)) 
                          install.packages(new.pkg, dependencies = T)
                          sapply(pkg, require, character.only = T)
 }

# Packages Utilizados 
    
#install.packages("remotes")
#remotes::install_github("KevinKotze/tsm")    

packages <- c("tidyverse",  # manipulation dataset
              "DataExplorer", # EDA
              "geobr", # maps 
              "knitr", # Tables
              "ggspatial", # maps
              "RColorBrewer", # Paleta
              "cowplot", # maps
              "broom", # coeficientes
              #"TSstudio", # plotes serie temporal 
              #"econocharts", # plot demanda/oferta
              "stargazer", # Tabela modelo 
              "urca", # Testes de raiz unitária
              "forecast", # Séries temporais 
              "tsm", # teste de raiz unitária 
              "vars", # teste de raiz unitária  
              "strucchange", # Teste de Chow
              "performance", # Validação do modelo 
              "tidymodels", # modelagem 
              "GGally", # matriz correlação 
              "nortest", 
              "lmtest")

# Liberando 

      library.packages(packages)

# Ctrl+Alt+B  


