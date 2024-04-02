library("tidyverse")
library("patchwork")

source("preform_data.R")
three_color_palette <- c("#377eb8", "#ff7f00", "#4daf4a")

six_color_palette <- c("#56b3e9", "#009e74", "#f0e442",
                       "#0071b2", "#d55e00", "#cc79a7")

source("Fig1.R")

# Fig1 is zoom_Rpl_mu
# Inkscape edits:
#   * substituted 1e-04 with 0.0001
#   * stage highlight/notation
ggsave("F1.svg", zoom_Rpl_mu, device = "svg",
       path = "svgs", scale = 3, width = 8, height = 6,
       units = "cm", dpi = 800)

# Fig2 is plo
# inkscape edit:
#   * substituted 1e-04 with 0.0001
#   * changed the legend point size (and realign points).
source("Fig3.R")
ggsave("F2.svg", plo, device = "svg",
       path = "svgs", scale = 3, width = 16, height = 12,
       units = "cm", dpi = 800)

#Fig3 is final
# inkscape edit: align the legend title with the gradient
source("Fig4.R") #takes time!
ggsave("F3.svg", final, device = "svg",
       path = "svgs", scale = 3, width = 8, height = 6,
       units = "cm", dpi = 800)

#Fig4 is Hpl_mu
# Inkscape edits:
#   * substituted 1e-04 with 0.0001
ggsave("F4.svg", Hpl_mu, device = "svg",
       path = "svgs", scale = 3, width = 8, height = 6,
       units = "cm", dpi = 800)

#Fig S1 is supfig
# Inkscape edits:
#   * Make thicker the line that mark N in the "denser simulation"
source("supfig.R")
ggsave("S1.svg", supfig, device = "svg",
       path = "svgs", scale = 5, width = 8, height = 6,
       units = "cm", dpi = 800)
