# soil.gen, predA, predB, soil.radlog come from file soil.R

f2_b1 <-
  ggplot(soil.gen, aes(y = Occ, x = MA)) + 
  annotate(xmin = 0.002231293, xmax = 0.003790223, ymin = -Inf, ymax = Inf, geom = "rect", alpha = 0.3) +
  geom_point(aes(shape = Core, fill = Core), size = 2) + 
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("#81392a", "#d58e80")) +
 theme_bw() + ylab("Occupancy") + 
  theme(aspect.ratio = .618) +  
  geom_line(aes(y = predA), lty = 5, color = "#003D73", size = 1) + 
  geom_line(aes(y = predB), lty = 5, color = "#003D73", size = 1) + 
  scale_x_log10() +
  annotation_logticks(sides = "b") +
  xlab("Max. Abundance") +
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank())

f2_b2 <-
  ggplot() + 
  geom_point(aes(x = 1:51, y = c(soil.radlog$y)), size = 2, fill = "#d58e80", pch = 22) + 
  geom_line(aes(x = 1:51, y = c(soil.radlog$fitted.values)), color = "#003D73") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) + 
  theme_bw() +
  xlab("Rank") +
  theme(aspect.ratio = .618) +
  annotation_logticks(sides = "l") +
  ylab("Max. Abundance") +
  theme(panel.grid.minor = element_blank())

library(egg)

f2_b <- egg::ggarrange(f2_b1, f2_b2, ncol = 2)


png(filename = "cs2.png",  width = 20, height = 8, units = "cm", res = 400) 
f2_b 
dev.off()
