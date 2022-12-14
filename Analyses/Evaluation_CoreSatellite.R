
# Different variance ------------------------------------------------------

var.test((Occ) ~ Core, data = rat.gen)
var.test((Occ) ~ Core, data = soil.gen)
var.test((Occ) ~ Core, data = lake.gen)

car::leveneTest((Occ) ~ Core, data = lake.gen)
car::leveneTest((Occ) ~ Core, data = soil.gen)
car::leveneTest((Occ) ~ Core, data = rat.gen)

bartlett.test((Occ) ~ Core, data = rat.gen)
bartlett.test((Occ) ~ Core, data = soil.gen)
bartlett.test((Occ) ~ Core, data = lake.gen)

library(cvequality)
asymptotic_test(lake.gen$Occ, lake.gen$Core)
asymptotic_test(soil.gen$Occ, soil.gen$Core)
asymptotic_test(rat.gen$Occ, rat.gen$Core)

mslr_test(x = lake.gen$Occ, y = lake.gen$Core)
mslr_test(x = soil.gen$Occ, y = soil.gen$Core)
mslr_test(x = rat.gen$Occ, y = rat.gen$Core)

# Abundance distribution --------------------------------------------------

plot(vegan::radfit(lake.gen %>% filter(Core == F) %>% select(MA) %>% unlist(), family = Gamma))
plot(vegan::radfit(soil.gen$MA))
plot(vegan::radfit(lake.gen$MA, family = Gamma))

plot(vegan::radfit(soil.gen$MA, family = Gamma))
plot(vegan::radfit(rat.gen$MA, family = Gamma))

(vegan::radfit(rat.gen %>% 
                 # filter(Core == F) %>% 
                 select(MA) %>% unlist(), family = Gamma))
(vegan::radfit(soil.gen$MA))
(vegan::radfit(lake.gen$MA))

plot(vegan::rad.lognormal(rat.gen %>% filter(Core == F) %>% select(MA) %>% unlist(), family = Gamma))

vegan::rad.lognormal(rat.gen$MA)

# Occupancy distribution --------------------------------------------------

#Lakes
data <- read.table("Data/240EE05_ns_ch_otus_table_WC_tax.txt",
                   header = T, sep = "")

lakes.u <- rowSums((data[, 2:41] > 0) * 1.0)

hist(lakes.u)
ggplot() + geom_histogram(aes(x = lakes.u/40), bins = 40) + scale_y_log10()

#Soils

soil.u_0 <- bac %>% mutate(Group = paste(Site_rep, Treatment, sep = "_")) %>% 
  select(2:5, Group) %>% mutate(`30` = (`30` > 0) * 1) %>% 
  mutate(`180` = (`180` > 0) * 1) %>% 
  mutate(`365` = (`365` > 0) * 1) %>% 
  mutate(`1460` = (`1460` > 0) * 1)

soil.u_1 <- by(data = soil.u_0[, 1:4], INDICES = soil.u_0$Group, FUN = rowSums, simplify = T)

soil.u <- soil.u_1[[1]] 

for(i in 2:length(soil.u_1)){
  soil.u <- soil.u + soil.u_1[[i]]
}

ggplot() + geom_histogram(aes(x = soil.u/72), bins = 72) + scale_y_log10()

#Monegros

colnames(abund)
monegros.u <- rowSums((abund[, 1:122] > 0) * 1)

ggplot() + geom_histogram(aes(x = monegros.u/122), bins = 122) + scale_y_log10()

ggplot( rbind(
data.frame(Occupancy = monegros.u/122, Dataset = 'Monegros'),
data.frame(Occupancy = soil.u/72, Dataset = 'Soil'),
data.frame(Occupancy = lakes.u/40, Dataset = 'Pyrenees')
), aes(x = Occupancy, fill = Dataset)) +
  geom_histogram() +
  scale_y_log10() +
  facet_wrap(~ Dataset, ncol = 1) +
  scale_fill_manual(values = c("#81392a", "#14878c",  "#e4b925")) +
  theme_bw() +
  theme(aspect.ratio = .618, panel.grid.minor = element_blank(), legend.position = 'none')


