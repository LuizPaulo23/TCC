# Plot - Demanda e oferta 

rm(list = ls()) # Limpando a memória 

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
      geom_path(data = supply1, color = "pink", size = 1, 
                linetype = "dashed", alpha = 9.0)+
      labs(x = "Quantidade", y = "Preço", 
           caption = "Fonte: Elaboração do autor (2022)") +
      coord_equal() +
      theme_classic(base_family = "Source Sans Pro")+
      theme(plot.title = element_text(family = "Source Sans Pro Semibold", 
                                        size = rel(1.3)), 
            plot.caption = element_text(hjust = 0))
    
