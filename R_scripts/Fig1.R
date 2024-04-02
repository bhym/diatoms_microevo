base_plot <- function(x, y) {
  x_ <- enquo(x)
  y_ <- enquo(y)

  pl <- all_ %>%
    ggplot() +
    aes(x = Day, col = factor(!!x_), group = interaction(run, rr, mu, perc)) +
    geom_line(data = filter(all_, perc == 0), alpha = 0.15,
              aes(y = !!y_, lty = perc == 0),
              col = "black", linewidth = 1.3, show.legend = FALSE) +
    geom_line(alpha = 0.15, aes(y = !!y_, linewidth = 0.5 + (perc == 0),
                                lty = perc == 0), show.legend = FALSE) +
    scale_linewidth(range = c(0.5, 1.3), breaks = c(0.5, 1.3))

  if (quo_name(x_) == "mu") {
    scale_name <- "Mutation rate"
    pl + scale_color_manual(scale_name, values = three_color_palette) +
      scale_y_log10()
  } else if (quo_name(x_) == "perc") {
    scale_name <- "Effective population (%)"
    pl + scale_colour_viridis_d("Effective population (%)", option = "B") +
      scale_y_log10()
  } else {
    scale_name <- "Unknown"
    pl + scale_y_continuous()
  }
}

Rpl_mu <- base_plot(mu, R)
Hpl_mu <- base_plot(mu, H) +
  geom_smooth(aes(y = H, group = mu), method = "lm",
              col = "black", lwd = 1.5, se = FALSE, show.legend = FALSE) +
  geom_smooth(aes(y = H, group = mu), method = "lm", se = FALSE) +
  ggthemes::theme_tufte(base_family = "sans", base_size = 28) +
  theme(legend.position = "top", legend.margin = margin())

zoom_Rpl_mu <- Rpl_mu + xlim(1430, 1480) +
  geom_line(data = all_[all_[["run"]] > 18, ], aes(y = R), lwd = 1.5,
            col = "black", show.legend = FALSE) +
  geom_line(data = all_[all_[["run"]] > 18, ], aes(y = R), lwd = 1.2) +
  ggthemes::theme_tufte(base_family = "sans", base_size = 28) +
  theme(legend.position = "top", legend.margin = margin())
