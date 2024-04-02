imp <- function(x) {
  transf_data <- x %>%
    select(-V1) %>%
    replace(is.na(.), 0) %>%
    as.matrix()

  ste_col <- log10(rowSums(transf_data))

  nmds_ <- transf_data %>%
    vegdist(method = "binomial") %>%
    metaMDS(trace = FALSE) %>%
    MDSrotate(rowSums(transf_data))

  nmds_ds <- dplyr::bind_cols(nmds_$points, `Log10(Tot. A abundance)` = ste_col)

  return(nmds_ds)
}

library(tidyverse)
library(vegan)
raw_ <- readRDS("../disco_data/nested_sex.RDS")

interm <- raw_ %>%
  separate(file, into = paste0("l", 1:8), sep = "_") %>%
  select(
    `Recombination rate` = l3,
    `Mutation rate` = l5,
    `% having sex` = l7,
    NG.list
  ) %>%
  mutate(
    rennet = map(NG.list, imp),
    `Recombination rate` = as.numeric(`Recombination rate`),
    `Mutation rate` = as.numeric(`Mutation rate`)
  ) %>%
  select(-NG.list) %>%
  group_by(`% having sex`) %>%
  nest() %>%
  ungroup()

# 0.2 sex, 1e-4 recrat, 1e-4 mutrat
x <- interm$data[[1]]$rennet[[1]]

final <- ggplot(x) +
  aes(MDS1, MDS2, col = `Log10(Tot. A abundance)`) +
  geom_point(size = 4) +
  scale_color_gradient(low = "dodgerblue4", high = "dodgerblue1") +
  ggthemes::theme_tufte(base_family = "sans", base_size = 28) +
  guides(colour = guide_colourbar(title.position = "top")) +
  theme(legend.position = "bottom")
