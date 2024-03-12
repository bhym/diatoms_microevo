bong <- all_  %>% select(H:R,-N)
nam_bong <- all_ %>% select(-c(H:R))
mu_cols <- factor(nam_bong$mu, labels = c("brown", "dodgerblue", "yellowgreen"))

mo_cols_lev <-  c("chartreuse4", "orange", "blue", "cyan","green","yellow")
moment <- rep(NA, length(nam_bong$Moment))
moment[which(nam_bong$Moment == "bloom")] <- "within bloom"
moment[which(nam_bong$Moment == "nonb")] <- "outside bloom"
moment[which(nam_bong$Moment == "off1")] <- "transition into bloom"
moment[which(nam_bong$Moment == "off2")] <- "transition outro bloom"
moment[which(nam_bong$Generation == 765)] <- "bloom start"
moment[which(nam_bong$Generation == 1500)] <- "bloom end"

# Extract PC axes for plotting
bong <- all_  %>% select(H:R,-N)
PCA <- prcomp(bong, scale=T, center=T)
PCAvalues <- data.frame(mu = factor(nam_bong$mu), perc = factor(nam_bong$perc), rr = factor(nam_bong$rr), PCA$x, Moment = moment)

# 0.2 sex, 1e-4 rr, all mutrat
PCAvalues <- PCAvalues %>% filter(perc == 0.2, rr == 1e-4)

# Extract loadings of the variables
PCAloadings <- data.frame(Variables = rownames(PCA$rotation), PCA$rotation)

vec <- c(10, 5.2, 9, 7)
plo <- ggplot(PCAvalues) +
 aes(x = PC1, y = PC2) +
 geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*5), yend = (PC2*5)), arrow = arrow(length = unit(1/2, "picas")), color = "black") +
  ggforce::geom_mark_hull(aes(fill = mu), concavity = 100) +
  geom_point(data=filter(PCAvalues, !grepl("^bloom",Moment)), pch = 21, col="black", alpha = 0.5) +
  geom_point(data=filter(PCAvalues, grepl("^bloom", Moment)),  pch = 21, col="black", alpha = 0.5) +
  geom_point(data=filter(PCAvalues, !grepl("^bloom",Moment)), pch = 20, aes(col=Moment), alpha = 0.5) +
  geom_point(data=filter(PCAvalues, grepl("^bloom", Moment)),  pch = 20, aes(col=Moment), alpha = 0.5) +
  geom_segment(data = PCAloadings, aes(x = 0, y = 0, xend = (PC1*vec), yend = (PC2*vec)), color = "black", lwd=0.5, lty=3) +
  annotate("text", x = (PCAloadings$PC1*(vec+0.4)), y = (PCAloadings$PC2*(vec+0.4)), label = PCAloadings$Variables, color = "black") +
  geom_vline(xintercept = 0, lty = 2) +
  geom_hline(yintercept = 0, lty = 2) +
  xlim(-3,7) +
  ylim(-3,5) +
  scale_fill_discrete("Mutation rate") +
  scale_color_brewer(type = "div", palette = 5)
