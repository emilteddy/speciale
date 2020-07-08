# EKSEMPEL:
# mitflotteplot <- ggplot(aes(x = antal_år_i_epinion, y = sandsynlighed_for_plads_ved_vinduet)) +
# geom_line() +
# epinion_style()

# HVIS MAN GERNE VIL BRUGE ARIAL, SKAL MAN FØRST INSTALLERE NEDENSTÅENDE:
# install.packages("extrafontdb")
# install.packages("Rttf2pt1")
# install.packages("extrafont")
# library(extrafontdb)
# library(Rttf2pt1)
# library(extrafont)
# 
# font_import()
# loadfonts()

options(warn=-1)

# Funktion, der definerer Epinion-temaet
epinion_style <- function(legend = FALSE) {
  
  skrifttype <- "Arial"
  
  if (legend) {
  ggplot2::theme(
    
    # Grafers titel - skrifttype, størrelse og farve
    plot.title = ggplot2::element_text(family=skrifttype,
                                       size=14,
                                       face="bold",
                                       color="#0F283C"),
    
    # Grafers subtitle - skrifttype, størrelse, farve og margin til titel
    plot.subtitle = ggplot2::element_text(family=skrifttype,
                                          size=12,
                                          color="#0F283C",
                                          margin=ggplot2::margin(9,0,9,0)),
    plot.caption = ggplot2::element_blank(),
    
    #Legend - position, allignment, fjerner tittle og baggrund og sætter tekstinstillinger inde i legend'en
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = ggplot2::element_blank(),
    legend.title = ggplot2::element_blank(),
    legend.key = ggplot2::element_blank(),
    legend.text = ggplot2::element_text(family=skrifttype,
                                        size=10,
                                        color="#0F283C"),
    
    # Akser - fjerner linjer og ticks, sætter margins og tekstinstillinger
    axis.title = ggplot2::element_text(family=skrifttype,
                                       size=12,
                                       color="#0F283C"),
    axis.text = ggplot2::element_text(family=skrifttype,
                                      size=10,
                                      color="#0F283C"),
    axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
    axis.ticks.x = ggplot2::element_line(size = 0.3,
                                         color = "#0F283C"),
    axis.ticks.y = ggplot2::element_blank(),
    axis.line.x = ggplot2::element_line(size = 0.5,
                                        color = "#0F283C"),
    
    #Grid-linjer - fjerner alle linjer
    panel.grid.minor = ggplot2::element_blank(),
    panel.grid.major.y = ggplot2::element_blank(),
    panel.grid.major.x = ggplot2::element_blank(),
    
    # Baggrund - efterlader baggrunden blank og fjerner dermed den grå standardfarve i ggplot
    panel.background = ggplot2::element_blank(),
    
    # Mere baggrund - sætter panelbaggrunden til hvid (relevant, hvis man bruger facet_wrap()) og sætter tekstindstillinger for den.
    strip.background = ggplot2::element_rect(fill="white"),
    strip.text = ggplot2::element_text(family = skrifttype,
                                       size  = 10,
                                       color = "#0F283C",
                                       hjust = 0)
  )
  } else {
    ggplot2::theme(
      
      # Grafers titel - skrifttype, størrelse og farve
      plot.title = ggplot2::element_text(family=skrifttype,
                                         size=14,
                                         face="bold",
                                         color="#0F283C"),
      
      # Grafers subtitle - skrifttype, størrelse, farve og margin til titel
      plot.subtitle = ggplot2::element_text(family=skrifttype,
                                            size=12,
                                            color="#0F283C",
                                            margin=ggplot2::margin(9,0,9,0)),
      plot.caption = ggplot2::element_blank(),
      
      #Legend - Ingen legend
      legend.position = "none",
      
      # Akser - fjerner linjer og ticks, sætter margins og tekstinstillinger
      axis.title = ggplot2::element_text(family=skrifttype,
                                         size=12,
                                         color="#0F283C"),
      axis.text = ggplot2::element_text(family=skrifttype,
                                        size=10,
                                        color="#0F283C"),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(5, b = 10)),
      axis.ticks.x = ggplot2::element_line(size = 0.3,
                                           color = "#0F283C"),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(size = 0.5,
                                          color = "#0F283C"),
      
      #Grid-linjer - fjerner alle linjer
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      
      # Baggrund - efterlader baggrunden blank og fjerner dermed den grå standardfarve i ggplot
      panel.background = ggplot2::element_blank(),
      
      # Mere baggrund - sætter panelbaggrunden til hvid (relevant, hvis man bruger facet_wrap()) og sætter tekstindstillinger for den.
      strip.background = ggplot2::element_rect(fill="white"),
      strip.text = ggplot2::element_text(family = skrifttype,
                                         size  = 10,
                                         color = "#0F283C",
                                         hjust = 0)
    )
  }
}