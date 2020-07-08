add_hygge <- function() {
  
  theme_bw() +
    theme(strip.background = element_rect(fill = "black"),
          strip.text = element_text(colour = "white"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.line = element_blank(),
          panel.background = element_rect(fill = "white", 
                                          colour = "black",
                                          size = 1),
          axis.title = element_text(color = "black"),
          axis.text = element_text(color = "black"),
          axis.text.x = element_text(color = "black"),
          axis.text.y = element_text(color = "black"))
  
}