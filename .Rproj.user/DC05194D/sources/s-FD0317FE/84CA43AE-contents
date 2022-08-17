f2_a1 <-
  ggplot(lake.gen %>% filter(p*30 > 8), aes(y = Occ, x = MA)) + 
  annotate("rect", xmin =0.004828888 , xmax = 0.013271, ymin = -Inf, ymax = Inf, 
           alpha = .3 ) + 
  geom_point(aes(shape = Core, fill = Core), size = 2) + 
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("#14878c", "#61e3e8")) +
  scale_x_log10(labels = function(x) format(x, scientific = TRUE)) +
  theme_bw() + ylab("Occupancy") + 
  annotation_logticks(sides = "b") +
  theme(aspect.ratio = .618) +  
  geom_line(aes(y = predA[-c(15, 40, 44)]), lty = 5, color = "#003D73", size = 1) +
  geom_line(aes(y = predB[-c(15, 40, 44)]), lty = 5, color = "#003D73", size = 1)+
  xlab("Max. Abundance") +
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank())

f2_a2 <-
  ggplot() + 
  geom_point(aes(x = 1:25, y = c(lake.radlog$y)), size = 2, fill = "#61e3e8", pch = 22) + 
  geom_line(aes(x = 1:25, y = c(lake.radlog$fitted.values)), color = "#003D73") +
  scale_y_log10(labels = function(x) format(x, scientific = TRUE)) + 
  theme_bw() +
  xlab("Rank") +
  theme(aspect.ratio = .618) +
  annotation_logticks(sides = "l") +
  ylab("Max. Abundance") +
  theme(panel.grid.minor = element_blank())

f2_a <- egg::ggarrange(f2_a1, f2_a2, ncol = 2)

