# (1/min(rat.gen$MA))/(1/min(rat.gen$Occ))
# 
# 
# plot((sort(rat.gen$MA))/(sum(rat.gen$MA)))
# 
# nrow(rat.gen)
#  unique(sample.int(219, 250, prob = log10(rat.gen$MA * 10000), replace = T))
# 
#  plot(table(sample.int(219, 250, prob = log10(rat.gen$MA * 10000), replace = T)))
# 
#  summary(rat.gen$Occ)



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

# I'm gonna reorder the matrix to have similar communities close, just selecting greedily the next sample

sp.time.mat

m3 <- m1 <- sp.time.mat[, -1]
  
m2 <- shared_species <- crossprod(as.matrix(sp.time.mat[, -1]))
diag(shared_species) <- 0
max.shared <- max(shared_species)
m2.pos <- col.index <- which(shared_species == max.shared, arr.ind = T)

m3[, 1] <- sp.time.mat[, col.index[1, 1] + 1]
m3[, 2] <- sp.time.mat[, col.index[1, 2] + 1]

m2 <- shared_species[-col.index[1, 1], -col.index[1, 1]]

initial.col <- which(colnames(m2) == m2.pos[1, 2])

for(i in 3:1000){
  next.col <- which.max(m2[, initial.col])
  # print(next.col)
  m3[, i] <- m1[, as.numeric(names(next.col))]
  # m2 <- m2[-as.numeric(colnames(m2)[initial.col]), -as.numeric(colnames(m2)[initial.col])]
  m2 <- m2[-initial.col, -initial.col]
  # m2
  initial.col <- which(colnames(m2) == names(next.col))
  # print(initial.col)
}


ab.sim <- regular_sampling_scheme(as.data.frame(cbind(sp.time.mat[, 1], m3)), 
                                  2:1001, level = "Species", n = 0)

# save(ab.sim, file = "absim.RData")

plot(ab.sim$e[!rat.gen$Core], rat.gen$Occ[!rat.gen$Core])
cor.test(log10(ab.sim$c)[!rat.gen$Core], log10(1/ab.sim$e)[!rat.gen$Core])

ab.sim <- ab.sim %>% mutate(p = 1/e)
summary(lm(data = ab.sim[!rat.gen$Core, ], log10(p) ~ log10(c)))
# plot(ab.sim$c, ab.sim$e)
# 
# sp.time.df <- as.data.frame(sp.time.mat)
# rowSums(sp.time.df[, -1])
# summary(rowSums(sp.time.df[, -1])))))


# Obtaining estimate for the slope  ---------------------------------------

set.seed(957238401)

reps <- 300

sim.slope <- rep(NA, reps)

for(rep in 1:length(sim.slope)){
  sample.species <- data.frame(Time = 0, Species = 0)
  for(i in 1:100){
    species <- unique(sample.int(219, 65, prob = log10(rat.gen$MA * 10000), replace = T))
    sample.species <- sample.species %>% add_row(Time = i, Species = species)
  }

  sp.time.mat <- sample.species %>% filter(Time != 0) %>% add_column(n = 1) %>%
    pivot_wider(names_from = Time, values_from = n, values_fill = 0) %>% arrange(Species)

  m3 <- m1 <- sp.time.mat[, -1]
  m2 <- shared_species <- crossprod(as.matrix(sp.time.mat[, -1]))
  diag(shared_species) <- 0
  max.shared <- max(shared_species)
  m2.pos <- col.index <- which(shared_species == max.shared, arr.ind = T)

  m3[, 1] <- sp.time.mat[, col.index[1, 1] + 1]
  m3[, 2] <- sp.time.mat[, col.index[1, 2] + 1]

  m2 <- shared_species[-col.index[1, 1], -col.index[1, 1]]

  initial.col <- which(colnames(m2) == m2.pos[1, 2])

  for(i in 3:100){
    next.col <- which.max(m2[, initial.col])
    m3[, i] <- m1[, as.numeric(names(next.col))]
    m2 <- m2[-initial.col, -initial.col]
    initial.col <- which(colnames(m2) == names(next.col))
  }

  ab.sim <- regular_sampling_scheme(as.data.frame(cbind(sp.time.mat[, 1], m3)),
                                    2:101, level = "Species", n = 0)

  ab.sim <- ab.sim %>% mutate(p = 1/e)

  sim.slope[rep] <- try(unname(coef(lm(data = ab.sim[!rat.gen$Core, ],
                                   log10(p) ~ log10(c)))[2]))
}

# Calculate confidence interval

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

