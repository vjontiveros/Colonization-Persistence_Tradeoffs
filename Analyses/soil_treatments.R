
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

# UP TO HERE, EVERYTHING REMAINS EQUAL. NOW WE SELECT TREATMENTS

# Colonization extinction estimation for taxonomic levels and treat C0 --------

# Skipping columns that are not needed and transforming to presence-absence
soil.treats <- bac[, c(2:5, 11, 13:16, 7), ]

soil.c0 <- soil.treats %>% filter(Treatment == "C0")

soil.c0[, 1:4] <- (soil.c0[, 1:4] > 0) * 1.0

# Estimating rates
soil.c0.phy <- irregular_single_dataset(soil.c0, 1:4, 0.00000004, 0.0000004, column = "Phylum", n = 55, verbose = F)
soil.c0.cla <- irregular_single_dataset(soil.c0, 1:4, 0.00000004, 0.0000004, column = "Class", n = 55, verbose = F)
soil.c0.ord <- irregular_single_dataset(soil.c0, 1:4, 0.00000004, 0.0000004, column = "Order", n = 55, verbose = F)
soil.c0.fam <- irregular_single_dataset(soil.c0, 1:4, 0.00000004, 0.0000004, column = "Family", n = 55, verbose = F)
soil.c0.gen <- irregular_single_dataset(soil.c0, 1:4, 0.00000004, 0.0000004, column = "Genera", n = 55, verbose = F)

# Transforming extinction to persistence 

soil.c0.phy$p <- 1/soil.c0.phy$e
soil.c0.cla$p <- 1/soil.c0.cla$e
soil.c0.ord$p <- 1/soil.c0.ord$e
soil.c0.fam$p <- 1/soil.c0.fam$e
soil.c0.gen$p <- 1/soil.c0.gen$e

# C1

soil.c1 <- soil.treats %>% filter(Treatment == "C1")

soil.c1[, 1:4] <- (soil.c1[, 1:4] > 0) * 1.0

# Estimating rates
soil.c1.phy <- irregular_single_dataset(soil.c1, 1:4, 0.00000004, 0.0000004, column = "Phylum", n = 55, verbose = F)
soil.c1.cla <- irregular_single_dataset(soil.c1, 1:4, 0.00000004, 0.0000004, column = "Class", n = 55, verbose = F)
soil.c1.ord <- irregular_single_dataset(soil.c1, 1:4, 0.00000004, 0.0000004, column = "Order", n = 55, verbose = F)
soil.c1.fam <- irregular_single_dataset(soil.c1, 1:4, 0.00000004, 0.0000004, column = "Family", n = 55, verbose = F)
soil.c1.gen <- irregular_single_dataset(soil.c1, 1:4, 0.00000004, 0.0000004, column = "Genera", n = 55, verbose = F)

# Transforming extinction to persistence 

soil.c1.phy$p <- 1/soil.c1.phy$e
soil.c1.cla$p <- 1/soil.c1.cla$e
soil.c1.ord$p <- 1/soil.c1.ord$e
soil.c1.fam$p <- 1/soil.c1.fam$e
soil.c1.gen$p <- 1/soil.c1.gen$e

# C2

soil.c2 <- soil.treats %>% filter(Treatment == "C2")

soil.c2[, 1:4] <- (soil.c2[, 1:4] > 0) * 1.0

# Estimating rates
soil.c2.phy <- irregular_single_dataset(soil.c2, 1:4, 0.00000004, 0.0000004, column = "Phylum", n = 55, verbose = F)
soil.c2.cla <- irregular_single_dataset(soil.c2, 1:4, 0.00000004, 0.0000004, column = "Class", n = 55, verbose = F)
soil.c2.ord <- irregular_single_dataset(soil.c2, 1:4, 0.00000004, 0.0000004, column = "Order", n = 55, verbose = F)
soil.c2.fam <- irregular_single_dataset(soil.c2, 1:4, 0.00000004, 0.0000004, column = "Family", n = 55, verbose = F)
soil.c2.gen <- irregular_single_dataset(soil.c2, 1:4, 0.00000004, 0.0000004, column = "Genera", n = 55, verbose = F)

# Transforming extinction to persistence 

soil.c2.phy$p <- 1/soil.c2.phy$e
soil.c2.cla$p <- 1/soil.c2.cla$e
soil.c2.ord$p <- 1/soil.c2.ord$e
soil.c2.fam$p <- 1/soil.c2.fam$e
soil.c2.gen$p <- 1/soil.c2.gen$e


# Calculating slope -------------------------------------------------------

# RECYCLING CORE FROM THE GENERAL THING

soil.c0.gen$Core <- soil.gen$Core

lm(data = soil.c0.gen %>% filter(p > 30, p < 10000), log10(p) ~ log10(c))

# lm for core
out <- (lm(data = soil.c0.gen %>% filter(p > 30, p < 10000) %>% filter(Core == T), log10(p) ~ log10(c)))
sm <- summary(out)
# lm for satellite
outs <- lm(data = soil.c0.gen %>% filter(p > 30, p < 10000) %>% filter(Core == F), log10(p) ~ log10(c))
sms <- summary(outs)

# Slope and error for satellite
stde <- sms$coefficients[2,2] # Slope standard error
slope <- sms$coefficients[2,1] # Slope
# 95% confidence interval
df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 

# Equal slope hypothesis contrast: beta = sm$coefficients[2,1]
tscore <- (slope - (sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore,df, lower.tail = F) # Multiplied by 2 as it is a bilateral constrast

pvalue
slope
slope0
slope1
tscore

# Slope and error for core
stde <- sm$coefficients[2,2] 
slope <- sm$coefficients[2,1] 
# CI 95%
df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope0
slope1

# C1
plot(log10(soil.c1.gen$c), log10(soil.c1.gen$p))

soil.c1.gen$Core <- soil.gen$Core

# lm for core
out <- (lm(data = soil.c1.gen %>% filter(p > 30, p < 10000) %>% filter(Core == T), log10(p) ~ log10(c)))
sm <- summary(out)
# lm for satellite
outs <- lm(data = soil.c1.gen %>% filter(p > 30, p < 10000) %>% filter(Core == F), log10(p) ~ log10(c))
sms <- summary(outs)

# Slope and error for satellite
stde <- sms$coefficients[2,2] # Slope standard error
slope <- sms$coefficients[2,1] # Slope
# 95% confidence interval
df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 

# Equal slope hypothesis contrast: beta = sm$coefficients[2,1]
tscore <- (slope - (sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore,df, lower.tail = F) # Multiplied by 2 as it is a bilateral constrast

pvalue
slope
slope0
slope1
tscore

# Slope and error for core
stde <- sm$coefficients[2,2] 
slope <- sm$coefficients[2,1] 
# CI 95%
df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope0
slope1


# C2
plot(log10(soil.c2.gen$c), log10(soil.c2.gen$p))

soil.c2.gen$Core <- soil.gen$Core

# lm for core
out <- (lm(data = soil.c2.gen %>% filter(p > 30, p < 10000) %>% filter(Core == T), log10(p) ~ log10(c)))
sm <- summary(out)
# lm for satellite
outs <- lm(data = soil.c2.gen %>% filter(p > 30, p < 10000) %>% filter(Core == F), log10(p) ~ log10(c))
sms <- summary(outs)

# Slope and error for satellite
stde <- sms$coefficients[2,2] # Slope standard error
slope <- sms$coefficients[2,1] # Slope
# 95% confidence interval
df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 

# Equal slope hypothesis contrast: beta = sm$coefficients[2,1]
tscore <- (slope - (sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore,df, lower.tail = F) # Multiplied by 2 as it is a bilateral constrast

pvalue
slope
slope0
slope1
tscore

# Slope and error for core
stde <- sm$coefficients[2,2] 
slope <- sm$coefficients[2,1] 
# CI 95%
df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope
slope0
slope1
