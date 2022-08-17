
# Load packages -----------------------------------------------------------

library(tidyverse)
library(island)
library(strucchange) 
library(vegan)



# Load data ---------------------------------------------------------------

data <- read.csv("Data/hartmann.csv",sep=";",check.names=F)

bac <- data %>% 
  select(-5) %>% # Many NAs in this column
  filter(Kingdom == "Bacteria")


# Taxonomy  ---------------------------------------------------------------

tax <- bac[, c(11, 13:16)]

tax$Phylum <- as.character(tax$Phylum)

# Some phyla have too few OTUs. We agreggate those.
sum(bac$OTU == "OTU0001") ### Number of Treatments x replicates of a single OTU 

# A cut-off of 167 implies that there are less of 10 OTUs in each taxonomic
# group. So, we aggregate the taxonomic groups with less than that.

#Phylum
mynames <- names(which(table(as.character(tax$Phylum)) < 167))
for(i in 1:nrow(tax)){
  if(any(mynames == tax$Phylum[i])) tax$Phylum[i] <- "Other"
}
tax$Phylum[tax$Phylum == "unclassified"] <- "Other"

#Class
tax$Class <- as.character(tax$Class)
tax$Class[tax$Phylum == "Other"] <- "Other"
mynames <- names(which(table(as.character(tax$Class)) < 167))
for(i in 1:nrow(tax)){
  if(any(mynames == tax$Class[i])) tax$Class[i] <- "Other"
}
tax$Class[tax$Class == "unclassified"] <- "Other"
tax$Class[tax$Class == "Unclassified"] <- "Other"

#Order
tax$Order <- as.character(tax$Order)
tax$Order[tax$Class == "Other"] <- "Other"
mynames <- names(which(table(as.character(tax$Order)) < 167))
for(i in 1:nrow(tax)){
  if(any(mynames == tax$Order[i])) tax$Order[i] <- "Other"
}
tax$Order[tax$Order == "unclassified"] <- "Other"
tax$Order[tax$Order == "Unclassified"] <- "Other"

#Family
tax$Family <- as.character(tax$Family)
tax$Family[tax$Order == "Other"] <- "Other"
mynames <- names(which(table(as.character(tax$Family)) < 167))
for(i in 1:nrow(tax)){
  if(any(mynames == tax$Family[i])) tax$Family[i] <- "Other"
}
tax$Family[tax$Family == "unclassified"] <- "Other"
tax$Family[tax$Family == "Unclassified"] <- "Other"

#Genera
tax$Genera <- as.character(tax$Genera)
tax$Genera[tax$Family == "Other"] <- "Other"
mynames <- names(which(table(as.character(tax$Genera)) < 167))
for(i in 1:nrow(tax)){
  if(any(mynames == tax$Genera[i])) tax$Genera[i] <- "Other"
}
tax$Genera[tax$Genera == "unclassified"] <- "Other"
tax$Genera[tax$Genera == "Unclassified"] <- "Other"

# Combining taxonomy
tax$Class <- paste(tax$Phylum, tax$Class, sep = ";")
tax$Order <- paste(tax$Class, tax$Order, sep = ";")
tax$Family <- paste(tax$Order, tax$Family, sep = ";")
tax$Genera <- paste(tax$Family, tax$Genera, sep = ";")

# table(tax$Genera) #uncomment to see

# Subtitute it back in the initial data.frame
bac$Phylum <- tax$Phylum
bac$Class <- tax$Class
bac$Order <- tax$Order
bac$Family <- tax$Family
bac$Genera <- tax$Genera



# START OF CALCULATIONS ---------------------------------------------------


# Coherence check ---------------------------------------------------------

# Now, we examine the coherence of the method. The soil dataset contains
# information for two sites, three treatments and three replicates each. We
# expect the replicates of each site to be closer intra-site than inter-sites.

bac$Site_rep <- paste(bac$Site, bac$Replica, sep = "_")

soil.df1 <- bac[, c(2:5, 18)] 
soil.df1[, 1:4] <- (soil.df1[, 1:4] > 0) * 1.0

# Estimate colonization extinction for the complete dataset
irregular_single_dataset(soil.df1, 1:4, 0.0001, 0.001, CI = T, jacobian = T)

# Estimate ce for each replicate of the site
soil_site_rep <- 
  irregular_single_dataset(soil.df1, 1:4, 0.0001, 0.001, column = 5, n = 167, 
                         CI = T, jacobian = T, verbose = F)


# Colonization extinction estimation for taxonomic levels --------

# Skipping columns that are not needed and transforming to presence-absence
soil.df2 <- bac[, c(2:5, 11, 13:16)]
soil.df2[, 1:4] <- (soil.df2[, 1:4] > 0) * 1.0

# Estimating rates
soil.phy <- irregular_single_dataset(soil.df2, 1:4, 0.00000004, 0.0000004, column = "Phylum", n = 167, verbose = F)
soil.cla <- irregular_single_dataset(soil.df2, 1:4, 0.00000004, 0.0000004, column = "Class", n = 167, verbose = F)
soil.ord <- irregular_single_dataset(soil.df2, 1:4, 0.00000004, 0.0000004, column = "Order", n = 167, verbose = F)
soil.fam <- irregular_single_dataset(soil.df2, 1:4, 0.00000004, 0.0000004, column = "Family", n = 167, verbose = F)
soil.gen <- irregular_single_dataset(soil.df2, 1:4, 0.00000004, 0.0000004, column = "Genera", n = 167, verbose = F)

# Transforming extinction to persistence 

soil.phy$p <- 1/soil.phy$e
soil.cla$p <- 1/soil.cla$e
soil.ord$p <- 1/soil.ord$e
soil.fam$p <- 1/soil.fam$e
soil.gen$p <- 1/soil.gen$e


# Core - satellite distinction --------------------------------------------

# Calculation of max abundance and occupancy
bac.ab <- bac

bac.ab[, 2] <- bac.ab[, 2]/sum(bac.ab[, 2])
bac.ab[, 3] <- bac.ab[, 3]/sum(bac.ab[, 3])
bac.ab[, 4] <- bac.ab[, 4]/sum(bac.ab[, 4])
bac.ab[, 5] <- bac.ab[, 5]/sum(bac.ab[, 5])

ab.gen <- bac.ab[bac.ab$Genera %in% soil.gen$Group, ]
ab.gen <- ab.gen %>% group_by(Genera) %>% select(c(2:5)) %>% summarise_all(sum) %>% 
  select(-1)

soil.gen$MA <- apply(X = ab.gen, MARGIN = 1, max)

occ.gen <- bac.ab[bac.ab$Genera %in% soil.gen$Group, ]
occ.gen[, 2:5] <- occ.gen[, 2:5] > 0
soil.gen$Occ <- occ.gen %>% group_by(Genera) %>% select(c(2:5)) %>% 
  summarise_all(mean) %>% select(-1) %>% rowMeans()


# Separating core and satellite.

chow <- rep(NA, 144) 
pvalue <- rep(NA, 144)
for (i in 30:114){
  chow[i] <- sctest(Occ ~ (MA), type = "Chow", point = i, data = soil.gen, asymptotic = T)$statistic
  pvalue[i] <- sctest(Occ ~ (MA), type = "Chow", point = i, data = soil.gen, asymptotic = T)$p.value
}

plot(pvalue)
which.max(chow[50:94]) 
# There is a structural change in position 82, there the relation between max.
# abundance and occupancy changes. But the structural change can be between
# positions 82 - 93. This delimitates the gray zone in figure 2.
sort(soil.gen$MA)[82]
sort(soil.gen$MA)[93]

fac <- factor(c(rep(1, 82), rep(2, 62)))[rank((soil.gen$MA))]

fmA <- lm(Occ ~ log10(MA), data = soil.gen[fac == 1, ])
fmB <- lm(Occ ~ log10(MA), data = soil.gen[fac == 2, ])
predA <- rep(NA, 144)
predB <- rep(NA, 144)
predA[fac == 1] <- predict(fmA)
predB[fac == 2] <- predict(fmB)

# We consider core those genera with an occupancy higher than the mean of the
# extremes of the two linear relationships.
((max(predA, na.rm = T) + min(predB, na.rm = T))/2) # .119323

soil.gen$Core <- soil.gen$Occ > ((max(predA, na.rm = T) + min(predB, na.rm = T))/2)

# Now we recycle the boundary of core groups for the families
occ.fam <- bac.ab[bac.ab$Family %in% soil.fam$Group, ]
occ.fam[, 2:5] <- occ.fam[, 2:5] > 0
soil.fam$Occ <- occ.fam %>% group_by(Family) %>% select(c(2:5)) %>% 
  summarise_all(mean) %>% select(-1) %>% rowMeans()

soil.fam$Core <- soil.fam$Occ > ((max(predA, na.rm = T) + min(predB, na.rm = T))/2)

# Core species follow a log normal ----------------------------------------

soil.ma <- (unlist(soil.gen %>% filter(Core == T) %>% select(MA)))

soil.radlog <- vegan::rad.lognormal(soil.ma, family = Gamma)



# Are genera/families closer within groups than between groups? --------------------

#Genera
# At a phyla level
strs <- str_split(soil.gen$Group, ";", simplify = T)

same <- outer(strs[, 1], strs[, 1], FUN = "==")

distance <- dist(log(soil.gen[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

#Class level
str_temp <- paste0(strs[, 1], strs[, 2])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(soil.gen[, c(2,11)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Order level
str_temp <- paste0(str_temp, strs[, 3])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(soil.gen[, c(2,11)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))


### Now with family
#Phylum level
strs <- str_split(soil.fam$Group, ";", simplify = T)

same <- outer(strs[, 1], strs[, 1], FUN = "==")

distance <- dist(log(soil.fam[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Class level
str_temp <- paste0(strs[, 1], strs[, 2])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(soil.fam[, c(2,11)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Order level
str_temp <- paste0(str_temp, strs[, 3])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(soil.fam[, c(2,11)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

