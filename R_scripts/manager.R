library("tidyverse")
library("patchwork")

source("preform_data.R")

source("Fig1.R")
source("Fig3.R")
source("Fig4.R") #takes time!

a <- wrap_plots(zoom_Rpl_mu, Hpl_mu, guides = "collect", widths = c(2, 1)) &
  theme(legend.position = "top", legend.margin = margin())

b <- (plo | (
             col2_sin_final +
             guides(colour = guide_colourbar(title.position = "top",
                                             title.hjust = -5))
            )
     ) & ggthemes::theme_tufte(base_family = "sans") +
         theme(legend.position = "bottom", legend.box = "vertical",
               legend.margin = margin())

ggsave(a / b, file = "raw_panel.svg", width = 7, height = 7)
