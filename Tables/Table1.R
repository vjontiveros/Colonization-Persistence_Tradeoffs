
# Load packages -----------------------------------------------------------

library(tidyverse)

# Soils -------------------------------------------------------------------

# We exclude those groups with a persistence lower than 30 days, which is less
# than a quarter of the minimal inter-event sampling time.

cor.test(log10(soil.phy$p[soil.phy$p>30]), log10(soil.phy$c[soil.phy$p>30]), method = "s")
cor.test(log10(soil.cla$p[soil.cla$p>30]), log10(soil.cla$c[soil.cla$p>30]), method = "s")
cor.test(log10(soil.ord$p[soil.ord$p>30]), log10(soil.ord$c[soil.ord$p>30]), method = "s")
cor.test(log10(soil.fam$p[soil.fam$p>30]), log10(soil.fam$c[soil.fam$p>30]), method = "s")
cor.test(log10(soil.gen$p[soil.gen$p>30]), log10(soil.gen$c[soil.gen$p>30]), method = "s")

summary(lm(data = soil.phy %>% filter(p > 30), log10(p) ~ log10(c)))
summary(lm(data = soil.cla %>% filter(p > 30), log10(p) ~ log10(c)))
summary(lm(data = soil.ord %>% filter(p > 30), log10(p) ~ log10(c)))
summary(lm(data = soil.fam %>% filter(p > 30), log10(p) ~ log10(c)))
summary(lm(data = soil.gen %>% filter(p > 30), log10(p) ~ log10(c)))

confint(lm(data = soil.phy %>% filter(p > 30), log10(p) ~ log10(c)))
confint(lm(data = soil.cla %>% filter(p > 30), log10(p) ~ log10(c)))
confint(lm(data = soil.ord %>% filter(p > 30), log10(p) ~ log10(c)))
confint(lm(data = soil.fam %>% filter(p > 30), log10(p) ~ log10(c)))
confint(lm(data = soil.gen %>% filter(p > 30), log10(p) ~ log10(c)))


# LAKES -------------------------------------------------------------------

# We skip those groups with a persistence lower than 8 days.

cor.test(log10(lake.phy$p[lake.phy$p > 8/30]), log10(lake.phy$c[lake.phy$p > 8/30]), method = "s")
cor.test(log10(lake.cla$p[lake.cla$p > 8/30]), log10(lake.cla$c[lake.cla$p > 8/30]), method = "s")
cor.test(log10(lake.ord$p[lake.ord$p > 8/30]), log10(lake.ord$c[lake.ord$p > 8/30]), method = "s")
cor.test(log10(lake.fam$p[lake.fam$p > 8/30]), log10(lake.fam$c[lake.fam$p > 8/30]), method = "s")
cor.test(log10(lake.gen$p[lake.gen$p > 8/30]), log10(lake.gen$c[lake.gen$p > 8/30]), method = "s")

summary(lm(data = lake.phy %>% filter(p > 8/30), log10(p) ~ log10(c)))
summary(lm(data = lake.cla %>% filter(p > 8/30), log10(p) ~ log10(c)))
summary(lm(data = lake.ord %>% filter(p > 8/30), log10(p) ~ log10(c)))
summary(lm(data = lake.fam %>% filter(p > 8/30), log10(p) ~ log10(c)))
summary(lm(data = lake.gen %>% filter(p > 8/30), log10(p) ~ log10(c)))

confint(lm(data = lake.phy %>% filter(p > 8/30), log10(p) ~ log10(c)))
confint(lm(data = lake.cla %>% filter(p > 8/30), log10(p) ~ log10(c)))
confint(lm(data = lake.ord %>% filter(p > 8/30), log10(p) ~ log10(c)))
confint(lm(data = lake.fam %>% filter(p > 8/30), log10(p) ~ log10(c)))
confint(lm(data = lake.gen %>% filter(p > 8/30), log10(p) ~ log10(c)))

# Monegros -------------------------------------------------------------------

# We skip those groups with a persistence lower than 6 days.

cor.test(log10(rat.phy$p[rat.phy$p > 6]), log10(rat.phy$c[rat.phy$p > 6]), method = "s")
cor.test(log10(rat.cla$p[rat.cla$p > 6]), log10(rat.cla$c[rat.cla$p > 6]), method = "s")
cor.test(log10(rat.ord$p[rat.ord$p > 6]), log10(rat.ord$c[rat.ord$p > 6]), method = "s")
cor.test(log10(rat.fam$p[rat.fam$p > 6]), log10(rat.fam$c[rat.fam$p > 6]), method = "s")
cor.test(log10(rat.gen$p[rat.gen$p > 6]), log10(rat.gen$c[rat.gen$p > 6]), method = "s")

summary(lm(data = rat.phy %>% filter(p > 6), log10(p) ~ log10(c)))
summary(lm(data = rat.cla %>% filter(p > 6), log10(p) ~ log10(c)))
summary(lm(data = rat.ord %>% filter(p > 6), log10(p) ~ log10(c)))
summary(lm(data = rat.fam %>% filter(p > 6), log10(p) ~ log10(c)))
summary(lm(data = rat.gen %>% filter(p > 6), log10(p) ~ log10(c)))

confint(lm(data = rat.phy %>% filter(p > 6), log10(p) ~ log10(c)))
confint(lm(data = rat.cla %>% filter(p > 6), log10(p) ~ log10(c)))
confint(lm(data = rat.ord %>% filter(p > 6), log10(p) ~ log10(c)))
confint(lm(data = rat.fam %>% filter(p > 6), log10(p) ~ log10(c)))
confint(lm(data = rat.gen %>% filter(p > 6), log10(p) ~ log10(c)))

