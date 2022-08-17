# rat.gen, predA, predB, rat.radlog come from file monegros.R

f2_c1 <-
ggplot(rat.gen, aes(y = Occ, x = MA)) + 
  annotate("rect", xmin =0.01313351 , xmax = 0.02106352, ymin = -Inf, ymax = Inf,
           alpha = .3 ) +
  geom_point(aes(shape = Core, fill = Core), size = 2) + 
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("#e4b925", "#f6e9bb")) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + ylab("Occupancy") + 
  annotation_logticks(sides = "b") +
  theme(aspect.ratio = .618) +  
  geom_line(aes(y = predA), lty = 5, color = "#003D73", size = 1) +
  geom_line(aes(y = predB), lty = 5, color = "#003D73", size = 1) +
  xlab("Max. Abundance") +
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank())

f2_c2 <-
ggplot() + 
  geom_point(aes(x = 1:88, y = c(rat.radlog$y)), size = 2, fill = "#EDD170", pch = 22) + 
  geom_line(aes(x = 1:88, y = c(rat.radlog$fitted.values)), color = "#003D73") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE), limits = c(NA, 1)) + 
  theme_bw() +
  xlab("Rank") +
  theme(aspect.ratio = .618) +
  annotation_logticks(sides = "l") +
  ylab("Max. Abundance") +
  theme(panel.grid.minor = element_blank())

png(filename = "monegros.png",  width = 20, height = 8, units = "cm", res = 400) 
egg::ggarrange(f2_c1, f2_c2, ncol = 2)
dev.off()

