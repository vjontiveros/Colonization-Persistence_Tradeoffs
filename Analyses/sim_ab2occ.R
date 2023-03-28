
# Preambule ---------------------------------------------------------------

# We'll use the abundance distribution found in Monegros to explore if the -1
# slope is produced just by statistical artifacts based on SADs. 

# You need to run file monegros first in order to follow this script.

set.seed(957238401)

# Load packages -----------------------------------------------------------

library(tidyverse)
library(island)


# Transforming abundances to occupancy ------------------------------------

# The abundance distribution has to produce a colonization extinction pattern,
# so we have to sample the distribution several times and mymic the occupancy of
# the species. As we have relative abundances, we can not sample the log(rel.
# abundances), so we have to assign a metacommunity size, because relative
# abundances produce negative values when you take the logarithm. And then, you
# need a local community size because if you sample too many individuals from
# the abundance distribution, you'll end up having all species present always.
# So, we have to explore combinations of metacommunity and local community size
# to find the one that mymics the occupancy distribution in Monegros. To those
# samples, we estimate then colonization and extinction rates, and therefore,
# persistence.

ab.to.occ <- data.frame()

metacomm <- c(5000, 6000, 7000, 8000, 9000, 10000)
localcomm <- c(50, 60, 70, 80, 90, 100)

for(nn in metacomm){
  for(j in localcomm){
    sample.species <- data.frame(Time = 0, Species = 0)
    for(i in 1:1000){
      species <- unique(sample.int(219, j, prob = log10(rat.gen$MA * nn), replace = T))
      sample.species <- sample.species %>% add_row(Time = i, Species = species) 
    }  
    
    prova <- unname(table(sample.species$Species)[-1])/1000

    ab.to.occ <- rbind(ab.to.occ, c(nn, j, min(prova), mean(prova), max(prova)))    
  }
}

summary(rat.gen$Occ) # This is what we have to mimic. 
View(ab.to.occ)

sample.species <- data.frame(Time = 0, Species = 0)
for(i in 1:1000){
  species <- unique(sample.int(219, 65, prob = log10(rat.gen$MA * 10000), replace = T))
  sample.species <- sample.species %>% add_row(Time = i, Species = species) 
}  

prova <- unname(table(sample.species$Species)[-1])/1000


summary(as.vector(prova))
summary(rat.gen$Occ)

# 10000 in the metacommunity and 65 in the local community seem to work well to
# get the central 50% of the distribution.

plot(rat.gen$Occ, as.vector(prova))


# Simulate the dynamics to get colonization and extinction rates ----------

sp.time.mat <- sample.species %>% filter(Time != 0) %>% add_column(n = 1) %>% 
  pivot_wider(names_from = Time, values_from = n, values_fill = 0) %>% arrange(Species)

# I'm gonna reorder the matrix to have similar communities close, just selecting
# the order performing hierarchical clustering of the samples, which effectively
# clusters the most similar samples together.

sim.order <- hclust(dist(t(as.matrix(sp.time.mat[, -1]))), method = 'ward.D')$order

m3 <- sp.time.mat[, -1]
m3 <- sp.time.mat[, sim.order]

ab.sim <- regular_sampling_scheme(as.data.frame(cbind(sp.time.mat[, 1], m3)), 
                                  2:1001, level = "Species", n = 0)

plot(ab.sim$e[rat.gen$Core], rat.gen$Occ[rat.gen$Core])
cor.test(log10(ab.sim$c)[!rat.gen$Core], log10(1/ab.sim$e)[!rat.gen$Core])

ab.sim <- ab.sim %>% mutate(p = 1/e)
summary(lm(data = ab.sim[rat.gen$Core, ], log10(p) ~ log10(c)))


# Obtaining estimate for the slope  ---------------------------------------

set.seed(957238401)

reps <- 300

sim.length <- 1000

sim.slope <- rep(NA, reps)

for(rep in 1:length(sim.slope)){
  sample.species <- data.frame(Time = 0, Species = 0)
  for(i in 1:sim.length){
    species <- unique(sample.int(219, 65, prob = log10(rat.gen$MA * 10000), replace = T))
    sample.species <- sample.species %>% add_row(Time = i, Species = species)
  }

  sp.time.mat <- sample.species %>% filter(Time != 0) %>% add_column(n = 1) %>%
    pivot_wider(names_from = Time, values_from = n, values_fill = 0) %>% arrange(Species)
  
  sim.order <- hclust(dist(t(as.matrix(sp.time.mat[, -1]))), method = 'ward.D')$order
  
  m3 <- sp.time.mat[, -1]
  m3 <- sp.time.mat[, sim.order]
  
  
  ab.sim <- regular_sampling_scheme(as.data.frame(cbind(sp.time.mat[, 1], m3)),
                                    2:(sim.length + 1), level = "Species", n = 0)
  
  ab.sim <- ab.sim %>% mutate(p = 1/e)
  
  sim.slope[rep] <- try(unname(coef(lm(data = ab.sim,
                                       log10(p) ~ log10(c)))[2]))
}


# Calculate confidence interval
sim.slope <- sim.slope %>% as.numeric()

sim.slope <- sim.slope[!(sim.slope %>% is.na())]

sample.mean <- sim.slope %>% mean(na.rm = T)

sample.n <- length(sim.slope)
sample.sd <- sd(sim.slope)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))

summary(sim.slope)

# save(sim.slope, file = "Data/observed_slope1000t.RData")


# TRYING WITH A LOGNORMAL -----------------------------------------------

set.seed(98385323)
sim.lnorm <- rlnorm(100, -4, 1.7) #Usign mean -4 and sd 1.7 as the monegros data was like this

ab.to.occ <- data.frame()

metacomm <- c(5000, 6000, 7000, 8000, 9000, 10000)
localcomm <- c(50, 60, 70, 80, 90, 100)

for(nn in metacomm){
  for(j in localcomm){
    sample.species <- data.frame(Time = 0, Species = 0)
    for(i in 1:1000){
      species <- unique(sample.int(100, j, prob = log10(sim.lnorm * nn), replace = T))
      sample.species <- sample.species %>% add_row(Time = i, Species = species) 
    }  
    
    prova <- unname(table(sample.species$Species)[-1])/1000
    
    ab.to.occ <- rbind(ab.to.occ, c(nn, j, min(prova), mean(prova), max(prova)))    
  }
}

# As this is just a theoretical distribution, we do not have to mimic any given
# species occupancy. So I select metacomm 10000 and localcomm 70 because it has
# a good range of occupancies.

set.seed(957238401)

reps <- 300
sim.length <- 1000

sim.slope <- rep(NA, reps)

for(rep in 1:length(sim.slope)){
  sample.species <- data.frame(Time = 0, Species = 0)
  for(i in 1:sim.length){
    species <- unique(sample.int(100, 70, prob = log10(sim.lnorm * 10000), replace = T))
    sample.species <- sample.species %>% add_row(Time = i, Species = species)
  }
  
  sp.time.mat <- sample.species %>% filter(Time != 0) %>% add_column(n = 1) %>%
    pivot_wider(names_from = Time, values_from = n, values_fill = 0) %>% arrange(Species)
  
  sim.order <- hclust(dist(t(as.matrix(sp.time.mat[, -1]))), method = 'ward.D')$order
  
  m3 <- sp.time.mat[, -1]
  m3 <- sp.time.mat[, sim.order]
  
  ab.sim <- regular_sampling_scheme(as.data.frame(cbind(sp.time.mat[, 1], m3)),
                                    2:(sim.length + 1), level = "Species", n = 0)
  
  ab.sim <- ab.sim %>% mutate(p = 1/e)
  
  sim.slope[rep] <- try(unname(coef(lm(data = ab.sim,
                                       log10(p) ~ log10(c)))[2]))
}

# Calculate confidence interval
sim.slope <- sim.slope %>% as.numeric()

sim.slope <- sim.slope[!(sim.slope %>% is.na())]


sample.mean <- sim.slope  %>% mean(na.rm = T)

sample.n <- length(sim.slope)
sample.sd <- sd(sim.slope)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))

summary(sim.slope)

# save(sim.slope, file = 'Data/lnorm_slope1000t.RData')



# SAME OCCUPANCY = EQUAL FITNESS ------------------------------------------

set.seed(98316823)

# We are going to use equal fitness, that in this case translates to similar
# occupancies. So, the metacommunity here doesn't have any influence, but the
# local sample controls occupancy. Say that we take 50 individuals locally. 


reps <- 300
sim.length <- 1000

sim.slope <- rep(NA, reps)

for(rep in 1:length(sim.slope)){
  sample.species <- data.frame(Time = 0, Species = 0)
  for(i in 1:sim.length){
    species <- unique(sample.int(100, 50, prob = rep(1, 100), replace = T))
    sample.species <- sample.species %>% add_row(Time = i, Species = species)
  }
  
  sp.time.mat <- sample.species %>% filter(Time != 0) %>% add_column(n = 1) %>%
    pivot_wider(names_from = Time, values_from = n, values_fill = 0) %>% arrange(Species)
  
  sim.order <- hclust(dist(t(as.matrix(sp.time.mat[, -1]))), method = 'ward.D')$order
  
  m3 <- sp.time.mat[, -1]
  m3 <- sp.time.mat[, sim.order]
  
  ab.sim <- regular_sampling_scheme(as.data.frame(cbind(sp.time.mat[, 1], m3)),
                                    2:(sim.length + 1), level = "Species", n = 0)
  
  ab.sim <- ab.sim %>% mutate(p = 1/e)
  
  sim.slope[rep] <- try(unname(coef(lm(data = ab.sim,
                                       log10(p) ~ log10(c)))[2]))
}

# Calculate confidence interval
sim.slope <- sim.slope %>% as.numeric()

sim.slope <- sim.slope[!(sim.slope %>% is.na())]


sample.mean <- sim.slope  %>% mean(na.rm = T)

sample.n <- length(sim.slope)
sample.sd <- sd(sim.slope)
sample.se <- sample.sd/sqrt(sample.n)
print(sample.se)

alpha = 0.05
degrees.freedom = sample.n - 1
t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
print(t.score)

margin.error <- t.score * sample.se
lower.bound <- sample.mean - margin.error
upper.bound <- sample.mean + margin.error
print(c(lower.bound,upper.bound))

summary(sim.slope)

# save(sim.slope, file = 'Data/equal_slope1000t.RData')
