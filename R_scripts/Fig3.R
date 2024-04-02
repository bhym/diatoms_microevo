data_ <- all_ %>% select(H:R, -N)
names_ <- all_ %>% select(-c(H:R))

moment <- rep(NA, length(names_$Moment))
moment[which(names_$Moment == "bloom")] <- "within bloom"
moment[which(names_$Moment == "nonb")] <- "outside bloom"
moment[which(names_$Moment == "off1")] <- "transition into bloom"
moment[which(names_$Moment == "off2")] <- "transition out of bloom"
moment[which(names_$Generation == 765)] <- "bloom start"
moment[which(names_$Generation == 1500)] <- "bloom end"

# Extract PC axes for plotting
data_ <- all_ %>% select(H:R, -N)
pca <- prcomp(data_, scale = TRUE, center = TRUE)
pcavalues <- data.frame(mu = factor(names_$mu),
                        perc = factor(names_$perc),
                        rr = factor(names_$rr), pca$x, Moment = moment)

# 0.2 sex, 1e-4 rr, all mutrat
pcavalues <- pcavalues %>% filter(perc == 0.2, rr == 1e-4)

# Extract loadings of the variables
pcaloadings <- data.frame(Variables = rownames(pca$rotation), pca$rotation)

vec <- c(10, 5.2, 9, 7)
plo <- ggplot(pcavalues) +
  aes(x = PC1, y = PC2) +
  geom_segment(data = pcaloadings, aes(x = 0, y = 0, xend = (PC1 * 5),
                                       yend = (PC2 * 5)),
               arrow = arrow(length = unit(2, "picas"))) +
  ggforce::geom_mark_hull(aes(fill = mu), concavity = 100) +
  geom_point(data = filter(pcavalues, Moment == "within bloom"), pch = 2,
             alpha = 0.5, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "within bloom"), pch = 17,
             aes(col = Moment[Moment == "within bloom"]), alpha = 0.5,
             size = 4) +
  geom_point(data = filter(pcavalues, Moment == "outside bloom"), pch = 21,
             alpha = 0.5, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "outside bloom"), pch = 20,
             aes(col = Moment[Moment == "outside bloom"]), alpha = 0.5,
             size = 4) +
  geom_point(data = filter(pcavalues, Moment == "bloom start"), pch = 21,
             alpha = 0.5, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "bloom start"), pch = 20,
             aes(col = Moment[Moment == "bloom start"]), alpha = 0.5,
             size = 4) +
  geom_point(data = filter(pcavalues, Moment == "bloom end"), pch = 21,
             alpha = 0.5, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "bloom end"), pch = 20,
             aes(col = Moment[Moment == "bloom end"]), alpha = 0.5, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "transition into bloom"),
             pch = 21, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "transition into bloom"),
             pch = 20, aes(col = Moment[Moment == "transition into bloom"]),
             alpha = 0.5, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "transition out of bloom"),
             pch = 21, alpha = 0.5, size = 4) +
  geom_point(data = filter(pcavalues, Moment == "transition out of bloom"),
             pch = 20, aes(col = Moment[Moment == "transition out of bloom"]),
             alpha = 0.5, size = 4) +
  geom_segment(data = pcaloadings, aes(x = 0, y = 0, xend = (PC1 * vec),
                                       yend = (PC2 * vec)), lwd = 0.5,
               lty = 3) +
  annotate("text", x = (pcaloadings$PC1 * (vec + 0.4)),
           y = (pcaloadings$PC2 * (vec + 0.4)),
           label = pcaloadings$Variables, size = 11) +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  scale_fill_manual("Mutation rate", values = three_color_palette) +
  scale_color_manual("Moment", values = six_color_palette) +
  ggthemes::theme_tufte(base_family = "sans", base_size = 28)
