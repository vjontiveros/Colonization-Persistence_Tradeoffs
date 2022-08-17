# install.packages("ggrepel")
library(ggrepel)
library(ggplot2)


# PANEL A -----------------------------------------------------------------

# Rates come from lakes.R

alp.ce <- rbind(lle.ce, llo.ce, rel.ce, red.ce)

df <- as.data.frame(alp.ce)
df$Type <- as.factor(c(rep("Basin", 3), "Redo"))

S_df <- df[1:3, ]  
S_df <- rbind(S_df, c(colMeans(S_df[1:6]), "Basin"))
S_df$c <- as.numeric(S_df$c)
S_df$e <- as.numeric(S_df$e)

plotA <-
  ggplot(df, aes(c, e)) + 
  xlab(expression("Colonization (day"^{-1}*")")) + 
  ylab(expression("Extinction (day"^{-1}*")")) + 
  geom_errorbarh(aes(xmax=c_up, xmin=c_low), size = 0.3)  +
  geom_errorbar( aes(ymax=e_up, ymin=e_low), size = 0.3) + theme_bw() + 
  geom_point(size = 2, pch = 21, fill = "#1ECFD6") + 
  stat_ellipse(aes(x = S_df$c, y = S_df$e), level = 0.93, linetype = 2, col = "grey60") +
  theme(axis.title.x  = element_text(size=14), axis.title.y  = element_text(size=14)) + 
  geom_text_repel(label = c("a", "b", "c", "d")) +
  theme(panel.grid.minor = element_blank())


# PANEL B -----------------------------------------------------------------

# This comes from lines 110-111 of file soil.R

df2 <- soil_site_rep

# Ellipses
S1 <- df2[1:3, 2:7]
S1 <- rbind(S1, colMeans(S1), rep(NA, 6), rep(NA, 6))

S2 <- df2[4:6, 2:7]
S2 <- rbind(S2, colMeans(S2), rep(NA, 6), rep(NA, 6))

plotB <-
  ggplot(df2, aes(c, e)) + 
  xlab(expression("Colonization (day"^{-1}*")")) + ylab(expression("Extinction (day"^{-1}*")")) + 
  geom_errorbarh(aes(xmax=c_up, xmin=c_low), size = 0.3) +
  geom_errorbar( aes(ymax=e_up, ymin=e_low), size = 0.3) + theme_bw() +
  geom_point(pch = 21, size = 2, fill = "#C05640") + 
  stat_ellipse(aes(x = S1$c, y = S1$e), level = 0.93, linetype = 2, col = "grey60") +
  stat_ellipse(aes(x = S2$c, y = S2$e), level = 0.90, linetype = 2, col = "grey60") +
  theme(axis.title.x  = element_text(size=14), axis.title.y  = element_text(size=14)) +
  geom_text(aes(x = 0.00032, y = 0.003), label = "Heiteren") +
  geom_text(aes(x = 0.00046, y = 0.0039), label = "Ermatingen") +
  theme(panel.grid.minor = element_blank())

plotB

# install.packages("cowplot")
library("cowplot")

png(filename = "Fig1Trade.png", width = 16, height = 7, units = "cm", res = 300)
plot_grid(plotA, plotB, labels=c("A", "B"), ncol = 2, nrow = 1)
dev.off()

setEPS()
postscript(file = "Fig1Trade_SEP18.eps", width = 7.08661, height = 3.14961, colormodel = "cmyk")
plot_grid(plotA, plotB, labels=c("A", "B"), ncol = 2, nrow = 1)
dev.off()
