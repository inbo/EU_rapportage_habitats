plot_statushabitat <- function(data, grenswaarde){
  plot1 <- data %>%
    filter(Grenswaarde == grenswaarde,
           Versie == "Versie 3") %>%
    ggplot(aes(x = Habitattype,
               y = AandeelGunstig,
               ymin = AandeelGunstig_LLCI,
               ymax = AandeelGunstig_ULCI,
               colour = Uitspraak)) +
    geom_point(size = 3) +
    geom_errorbar(width = 0.2) +
    #facet_wrap(~ Versie) +
    coord_flip() +
    labs(x = "Habitattype",
         colour = "Beoordeling",
         y = "Aandeel habitat in gunstige staat") +
    scale_colour_manual(limits = c("Gunstig", "Ongunstig", "Onbekend"),
                        breaks = c("Gunstig", "Ongunstig", "Onbekend"),
                        values = colors_status) +
    geom_hline(aes(yintercept = Grenswaarde),
               colour = "black",
               size = 0.5,
               linetype = "dashed",
               alpha = 0.5)  +
    INBOtheme::theme_inbo(base_size = 9) +
    theme(legend.position = "bottom") +
    scale_y_continuous(breaks = c(0 , 25, 50, 75, 100),
                       limits = c(0,100))
  return(plot1)
}

#----------------------------------------------------------------------------

plot_indicatoren <- function(data, habitat){
  data %>%
    filter(Habitattype == habitat) %>%
    ggplot(aes(x = Indicatorbis,
               y = AandeelGunstig,
               ymin = AandeelGunstig_LLCI,
               ymax = AandeelGunstig_ULCI,
               colour = Uitspraak,
               size = Belang)
    ) +
    geom_point() +
    geom_errorbar(width = 0.2, size = 0.5) +
    #facet_grid(Criterium ~ Versiebis, scales = "free_y", space = "free_y") +
    facet_grid(Criterium ~ ., scales = "free_y", space = "free_y") +
    coord_flip() +
    labs(x = "Indicator",
         colour = "Beoordeling",
         y = "Aandeel habitat met gunstige staat voor indicator",
         #       title = as.character(paste(i, "\n", sep =)),
         title = habitat
    ) +
    scale_colour_manual(limits = c("Gunstig", "Ongunstig", "Onbekend"),
                        breaks = c("Gunstig", "Ongunstig", "Onbekend"),
                        values = colors_status) +
    geom_hline(aes(yintercept = 75),
               colour = "black",
               size = 0.5,
               linetype = "dashed",
               alpha = 0.5)  +
    INBOtheme::theme_inbo(base_size = 9) +
    theme(legend.position = "bottom",
          #        legend.text.align = 0,
          #        legend.box = "vertical",
          #        legend.text = element_text(size = 8),
          strip.text = element_text(size = 6),
          plot.title = element_text(face = "bold", size = 10,
                                    lineheight = 0.8, hjust = 0),
          axis.text = element_text(lineheight = 0.8),
          #        axis.title = element_text(size = 8),
          #        strip.text = element_text(size =11, colour = "black"),
          panel.background = element_rect(colour = "grey")
    ) +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
    scale_y_continuous(breaks = c(0 , 25, 50, 75, 100),
                       limits = c(0,100)) +
    scale_size_manual(limits = c("b", "zb"),
                      labels = c("Belangrijk", "Zeer belangrijk"),
                      values = c(2 , 4))
  }

