
# Soils -------------------------------------------------------------------

p2 <- ggplot(soil.fam, aes(x = c, y = p, fill = Core)) + 
  geom_point(aes(shape = Core, fill = Core), size = 2) + 
  scale_x_log10(limits = c(0.0001, .0015) ) + 
  scale_y_log10(limits = c(36, 2500)) +
  theme_bw() + ylab("Persistence (day)") + 
  xlab("") + 
  annotation_logticks() +
  scale_shape_manual(values = c(21, 22)) + 
  scale_fill_manual(values = c("#81392a", "#d58e80")) +
  scale_color_manual(values = c("#81392a", "#d58e80")) +
  theme(aspect.ratio = .618) +  
  geom_smooth(aes(color = Core), formula = y ~ x, method = "lm", se = F) +
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank())+
  annotate("segment", x = 0.00014, xend = 0.00026, y = 50, yend = 50, 
           color = "grey20") +
  annotate("segment", x = 0.00014, xend = 0.00014, y = 50, yend = 92.857, 
           color = "grey20") +
  annotate("text", label = "-1", x = 0.000125, y = 43, color = "grey20") 

# Lakes -------------------------------------------------------------------

p1 <- ggplot(lake.fam, aes(x = c/30, y = 1/(e/30), fill = Core)) + 
  geom_point(aes(shape = Core, fill = Core), size = 2) + 
  scale_x_log10(limits = c(0.0075, .021) ) + scale_y_log10(limits = c(8, 310)) +
  theme_bw() + 
  ylab(" ") +
  scale_shape_manual(values = c(21, 22)) + 
  scale_fill_manual(values = c("#14878c", "#61e3e8")) +
  scale_color_manual(values = c("#14878c", "#61e3e8")) +
  xlab(" ") +
  annotation_logticks() +
  geom_smooth(aes(color = Core), formula = y ~ x, method = "lm", se = F) +
  theme(aspect.ratio = .618) +  
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank()) +
  annotate("segment", x = 0.0085, xend = 0.011, y = 11, yend = 11, 
           color = "grey20") +
  annotate("segment", x = 0.0085, xend = 0.0085, y = 11, yend = 14.235, 
           color = "grey20") +
  annotate("text", label = "-1", x = 0.0082, y = 10, color = "grey20") 


# Monegros ----------------------------------------------------------------

p3 <- ggplot(rat.fam, aes(x = c, y = 1/(e), fill = Core)) +
  geom_point(aes(shape = Core, fill = Core), size = 2) + 
  scale_x_log10(limits = c(.0062, .038)) + 
  scale_y_log10(limits = c(6, 100)) +
  theme_bw() + ylab("") + 
  scale_shape_manual(values = c(21, 22)) + 
  scale_fill_manual(values = c("#e4b925", "#f6e9bb")) +
  scale_color_manual(values = c("#e4b925", "#f6e9bb")) +
  xlab(expression(paste("Colonization (day"^"-1", ")"))) +
  annotation_logticks() +
  geom_smooth(aes(color = Core), formula = y ~ x, method = "lm", se = F) +
  theme(aspect.ratio = .618) +  
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank()) +
  annotate("segment", x = 0.007, xend = 0.01, y = 9, yend = 9,
           color = "grey20") +
  annotate("segment", x = 0.007, xend = 0.007, y = 9, yend = 12.85714,
           color = "grey20") +
  annotate("text", label = "-1", x = 0.0066, y = 8.5, color = "grey20")

setEPS()
postscript(file = "F3_20220816.eps", width = 3.93701, height = 9.44882, colormodel = "cmyk")
egg::ggarrange(p1, p2, p3)
dev.off()
