supfig <- all_ %>%
  select(Day, N, R, G, run, mu, perc, rr) %>%
  pivot_longer(-c(Day, mu, perc, rr, run)) %>%
  ggplot() +
  aes(x = Day, y = value, col = factor(mu),
      group = interaction(run, rr, mu, perc)) +
  geom_line(show.legend = FALSE, alpha = 0.15, linewidth = 0.35) +
  scale_colour_manual(values = three_color_palette) +
  ggthemes::theme_tufte(base_family = "sans", base_size = 28) +
  facet_wrap(~factor(name, levels = c("N", "G", "R")),
             ncol = 1, scale = "free") +
  scale_y_log10()
