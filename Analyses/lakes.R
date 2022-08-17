
# Load packages -----------------------------------------------------------

library(tidyverse)
library(strucchange)

# Read data ---------------------------------------------------------------

data <- read.table("Data/240EE05_ns_ch_otus_table_WC_tax.txt",
                 header = T, sep = "")

## Now we separate the data of the lakes and the type of sample
lle.wc <- cbind(data[,1], data$B1, data$D1, data$E1, data$G1, data$A2, data$C2,data$D2,data$E2,data$F2,data$G2,data[,42:45])
llo.wc <- cbind(data[,1], data$A3, data$C3, data$E3, data$G3, data$A4, data$C4,data$D4,data$E4,data$F4,data$G4,data[,42:45])
red.wc <- cbind(data[,1], data$E6, data$F6, data$H6, data$B7, data$C7, data$E7,data$F7,data$G7,data$A8,data$C8,data[,42:45])
rel.wc <- cbind(data[,1], data$H4, data$B5, data$C5, data$D5, data$F5, data$H5,data$A6,data$B6,data$C6,data$D6,data[,42:45])

## Transform it to presence-absence
lle.ab <- lle.wc
llo.ab <- llo.wc
rel.ab <- rel.wc
red.ab <- red.wc

lle.wc[, 2:11] <- (lle.wc[,2:11] > 0) * 1.0
llo.wc[, 2:11] <- (llo.wc[,2:11] > 0) * 1.0
rel.wc[, 2:11] <- (rel.wc[,2:11] > 0) * 1.0
red.wc[, 2:11] <- (red.wc[,2:11] > 0) * 1.0

# The species pool is assumed to be different for each lake to compare the
# dynamics of the lakes. Therefore, we take out the OTUs that have no presence in
# each lake.

lle.wc <- lle.wc[rowSums(lle.wc[, 2:11]) > 0, ]
llo.wc <- llo.wc[rowSums(llo.wc[, 2:11]) > 0, ]
rel.wc <- rel.wc[rowSums(rel.wc[, 2:11]) > 0, ]
red.wc <- red.wc[rowSums(red.wc[, 2:11]) > 0, ]

# The samples were not consecutive in winter, so we have to indicate the
# sampling time in the columns, such as month 1, 2, ... 12.
colnames(lle.wc)[2:11] <- (c(1:7, 10:12))
colnames(llo.wc)[2:11] <- c(1:7, 10:12)
colnames(rel.wc)[2:11] <- c(1:7, 10:12)
colnames(red.wc)[2:11] <- c(1:8, 11:12)


# Coherence check ---------------------------------------------------------

# We divide by 30 to have the daily rates.
lle.ce <- island::irregular_single_dataset(lle.wc, 2:11, c = 0.0000001, e = 0.00000001,
                                 CI = T, jacobian = T)/30
llo.ce <- island::irregular_single_dataset(llo.wc, 2:11, c = 0.0000001, e = 0.00000001,
                                 CI = T, jacobian = T)/30
rel.ce <- island::irregular_single_dataset(rel.wc, 2:11, c = 0.0000001, e = 0.00000001,
                                 CI = T, jacobian = T)/30
red.ce <- island::irregular_single_dataset(red.wc, 2:11, c = 0.0000001, e = 0.00000001,
                                 CI = T, jacobian = T)/30

# As the Redon lake has a very different dynamics, we skip that data further on.

# Taxonomy ----------------------------------------------------------------

# First, we join the data for the first three lakes, that are in the same basin
# and have similar dynamics.

lake.df <- rbind(lle.wc, llo.wc, rel.wc)

tax <- str_split(lake.df[, 12], pattern = "; __", simplify = T)
tax <- as.data.frame(tax)

for(i in 1:nrow(tax)){
  if(any(names(which(table(tax$V2) < 30)) == tax$V2[i])) tax$V2[i] <- "Other"
}

tax$V3[tax$V2 == "Other"] <- "Other"
temptax <-  tax$V3
temptax <- names(which(table(temptax) < 30))

for(i in 1:nrow(tax)){
  if(any(temptax == tax$V3[i])) tax$V3[i] <- "Other"
  if(tax$V3[i] == "c") tax$V3[i] <- "Other"
}

tax$V4[tax$V3 == "Other"] <- "Other"
temptax <-  tax$V4
temptax <- names(which(table(temptax) < 30))

for(i in 1:nrow(tax)){
  if(any(temptax == tax$V4[i])) tax$V4[i] <- "Other"
  if(tax$V4[i] == "o") tax$V4[i] <- "Other"
}

tax$V5[tax$V4 == "Other"] <- "Other"
temptax <-  tax$V5
temptax <- names(which(table(temptax) < 30))

for(i in 1:nrow(tax)){
  if(any(temptax == tax$V5[i])) tax$V5[i] <- "Other"
  if(tax$V5[i] == "f") tax$V5[i] <- "Other"
}

tax$V6[tax$V5 == "Other"] <- "Other"
temptax <-  tax$V6
temptax <- names(which(table(temptax) < 30))

for(i in 1:nrow(tax)){
  if(any(temptax == tax$V6[i])) tax$V6[i] <- "Other"
  if(tax$V6[i] == "g") tax$V6[i] <- "Other"
}

tax$V3 <- paste(tax$V2, tax$V3, sep = "_")
tax$V4 <- paste(tax$V3, tax$V4, sep = "_")
tax$V5 <- paste(tax$V4, tax$V5, sep = "_")
tax$V6 <- paste(tax$V5, tax$V6, sep = "_")

table(tax$V6)

lake.df <- cbind(lake.df, tax)


# Colonization-extinction rates estimation for taxonomic groups -----------

# We keep the groups with at least 28 Otus

# Estimating rates
lake.phy <- island::irregular_single_dataset(lake.df, 2:11, c=0.00001, e=0.00001, 
                                            column = "V2", n = 28, jacobian = T)
lake.cla <- island::irregular_single_dataset(lake.df, 2:11, c=0.00001, e=0.00001, 
                                             column = "V3", n = 28, jacobian = T)
lake.ord <- island::irregular_single_dataset(lake.df, 2:11, c=0.00001, e=0.00001, 
                                             column = "V4", n = 28, jacobian = T)
lake.fam <- island::irregular_single_dataset(lake.df, 2:11, c=0.00001, e=0.00001, 
                                             column = "V5", n = 28, jacobian = T)
lake.gen <- island::irregular_single_dataset(lake.df, 2:11, c=0.00001, e=0.00001, 
                                             column = "V6", n = 28, jacobian = T)


# Transforming extinction to persistence 
lake.phy$p <- 1/lake.phy$e
lake.cla$p <- 1/lake.cla$e
lake.ord$p <- 1/lake.ord$e
lake.fam$p <- 1/lake.fam$e
lake.gen$p <- 1/lake.gen$e

# Core - satellite distinction --------------------------------------------

lle.ab <- lle.ab[rowSums(lle.ab[, 2:11]) > 0, ]
llo.ab <- llo.ab[rowSums(llo.ab[, 2:11]) > 0, ]
rel.ab <- rel.ab[rowSums(rel.ab[, 2:11]) > 0, ]

colnames(lle.ab)[2:11] <- (c(1:7, 10:12))
colnames(llo.ab)[2:11] <- c(1:7, 10:12)
colnames(rel.ab)[2:11] <- c(1:7, 10:12)

lle.ab[, 2:11] <- (t(t(lle.ab[, 2:11])/colSums(lle.ab[, 2:11])))
llo.ab[, 2:11] <- (t(t(llo.ab[, 2:11])/colSums(llo.ab[, 2:11])))
rel.ab[, 2:11] <- (t(t(rel.ab[, 2:11])/colSums(rel.ab[, 2:11])))

basin.ab <- rbind(lle.ab, llo.ab, rel.ab)
basin.ab <- cbind(basin.ab, tax)

# Abundance for genera

lake.gen$MA <- basin.ab[basin.ab$V6 %in% lake.gen$Group, ] %>% 
  group_by(V6) %>% select(c(2:11)) %>% summarise_all(sum) %>% select(-1) %>%  
  apply(MARGIN = 1, FUN = max) %>% base::`/`(3)

temp <- basin.ab[basin.ab$V6 %in% lake.gen$Group, ] 
temp[, 2:11] <- (temp[, 2:11] > 0) * 1.0
lake.gen$Occ <- temp %>%  group_by(V6) %>% select(c(2:11)) %>% summarise_all(mean) %>% select(-1) %>%  
  rowMeans() 

# Separating core and satellite

chow <- rep(NA, 70) 
pvalue <- rep(NA, 70)
for (i in 10:62){
  chow[i] <- sctest(Occ ~ (MA) , type = "Chow", point = i, data = lake.gen, asymptotic = T)$statistic
  pvalue[i] <- sctest(Occ ~ (MA) , type = "Chow", point = i, data = lake.gen, asymptotic = T)$p.value
}
plot(pvalue, type = "l")
which.max(chow[15:50])

# There is a structural change in position 22, there the relation between max.
# abundance and occupancy changes. But the structural change can be between
# positions 18 - 27. This delimitates the gray zone in figure 2.

sort(lake.gen$MA)[18]
sort(lake.gen$MA)[27]


fac <- factor(c(rep(1, 22), rep(2, 22)))[rank((lake.gen$MA))]

fmA <- lm(Occ ~ log10(MA), data = lake.gen[fac == 1, ])
fmB <- lm(Occ ~ log10(MA), data = lake.gen[fac == 2, ])
predA <- rep(NA, 44)
predB <- rep(NA, 44)
predA[fac == 1] <- predict(fmA)
predB[fac == 2] <- predict(fmB)

# We consider core those genera with an occupancy higher than the mean of the
# extremes of the two linear relationships.
((max(predA, na.rm = T) + min(predB, na.rm = T))/2) # .1861362

lake.gen$Core <- lake.gen$Occ > ((max(predA, na.rm = T) + min(predB, na.rm = T))/2)

# Now we recycle the boundary of core groups for the families
temp <- basin.ab[basin.ab$V5 %in% lake.fam$Group, ] 
temp[, 2:11] <- (temp[, 2:11] > 0) * 1.0
lake.fam$Occ <- temp %>%  group_by(V5) %>% select(c(2:11)) %>% summarise_all(mean) %>% select(-1) %>%  
  rowMeans() 

lake.fam$Core <- lake.fam$Occ > ((max(predA, na.rm = T) + min(predB, na.rm = T))/2)

# Core species follow a log normal ----------------------------------------

lake.ma <- (unlist(lake.gen %>% filter(Core == T) %>% select(MA)))

lake.radlog <- vegan::rad.lognormal(lake.ma, family = Gamma)

# Are genera/families closer within groups than between groups? --------------------

#Genera
# At a phyla level
temp <- as.character(lake.gen$Group)
temp[3] <- "Actinobacteria_Actinobacteria_Frankiales_Sporichthyaceae_hgcI-clade"
temp[10] <- "Candidate-division-OD1_Other_Other_Other_Other"
temp[11] <- "Candidate-division-TM7_Other_Other_Other_Other"
strs <- str_split(temp, "_", simplify = T)
same <- outer(strs[, 1], strs[, 1], FUN = "==")

distance <- dist(log(lake.gen[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

#Class level
str_temp <- paste0(strs[, 1], strs[, 2])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(lake.gen[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Order level
str_temp <- paste0(str_temp, strs[, 3])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(lake.gen[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))


### Now with family
#Phylum level
temp <- as.character(lake.fam$Group)
temp[10] <- "Candidate-division-OD1_Other_Other_Other"
temp[11] <- "Candidate-division-TM7_Other_Other_Other"
strs <- str_split(temp, "_", simplify = T)
same <- outer(strs[, 1], strs[, 1], FUN = "==")

distance <- dist(log(lake.fam[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Class level
str_temp <- paste0(strs[, 1], strs[, 2])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(lake.fam[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Order level
str_temp <- paste0(str_temp, strs[, 3])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(lake.fam[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

