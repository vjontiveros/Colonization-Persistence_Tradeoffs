
# Load packages -----------------------------------------------------------

library(tidyverse)
library(island)
library(strucchange)

# Read data ---------------------------------------------------------------

data <- read.csv("Data/Bacteria.csv", sep = ";", check.names = F)
data2 <- read.csv("Data/envbact.csv", sep = ";", check.names = F)


# Prepare data ------------------------------------------------------------

# There are several lakes that we are pooling to obtain the colonization and
# extinction rates. Now we prepare separately each lake.



# Playa

lakecol <- data2 %>% filter(env == "z10_Playa") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
playa <- data[,columns]


colnames(playa) <- data2 %>% filter(env == "z10_Playa") %>% select(doy_acc) %>% 
  unlist() %>% unname()


for (i in 1:nrow(playa)){
  for (j in 1:ncol(playa)){
    if (playa[i,j]) playa[i,j] <- 1 
  }
}
playa <- cbind(playa, data[,141:145])
playa <- playa[as.logical(rowSums(playa[,1:15])), ]

# Salineta

lakecol <- data2 %>% filter(env == "9_Salineta") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}

salineta <- data[,columns]
colnames(salineta) <- data2 %>% filter(env == "9_Salineta") %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(salineta)){
  for (j in 1:ncol(salineta)){
    if (salineta[i,j]) salineta[i,j] <- 1 
  }
}
salineta <- cbind(salineta, data[,141:145])
salineta <- salineta[as.logical(rowSums(salineta[,1:16])), ]

#Piñol

lakecol <- data2 %>% filter(env == "2_Pinol") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

# The lake dried at some point in time. We do not include empty columns.

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
pinol <- data[,columns]
colnames(pinol) <- data2 %>% filter(env == "2_Pinol") %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(pinol)){
  for (j in 1:ncol(pinol)){
    if (pinol[i,j]) pinol[i,j] <- 1 
  }
}
pinol <- cbind(pinol, data[,141:145])
pinol <- pinol[as.logical(rowSums(pinol[,1:10])), ]

#Rebollon

lakecol <- data2 %>% filter(env == "7_Rebollon") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

lakecol <- as.character(lakecol)
columns <- c()

for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
rebollon <- data[,columns]

colnames(rebollon) <- data2 %>% filter(env == "7_Rebollon") %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(rebollon)){
  for (j in 1:ncol(rebollon)){
    if (rebollon[i,j]) rebollon[i,j] <- 1 
  }
}
rebollon <- cbind(rebollon, data[,141:145])
rebollon <- rebollon[as.logical(rowSums(rebollon[,1:7])), ]

# Rollico

lakecol <- data2 %>% filter(env == "6_Rollico") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
rollico <- data[,columns]

colnames(rollico) <- data2 %>% filter(env == "6_Rollico") %>% select(doy_acc) %>% 
  unlist() %>% unname()


for (i in 1:nrow(rollico)){
  for (j in 1:ncol(rollico)){
    if (rollico[i,j]) rollico[i,j] <- 1 
  }
}
rollico <- cbind(rollico, data[,141:145])
rollico <- rollico[as.logical(rowSums(rollico[,1:9])), ]

# Amarga baja

lakecol <- data2 %>% filter(env == "z12_Amarga_baja") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()

for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}

abaja <- data[,columns]

colnames(abaja) <- data2 %>% filter(env == "z12_Amarga_baja") %>% select(doy_acc) %>% 
  unlist() %>% unname()
  

for (i in 1:nrow(abaja)){
  for (j in 1:ncol(abaja)){
    if (abaja[i,j]) abaja[i,j] <- 1 
  }
}
abaja <- cbind(abaja, data[,141:145])
abaja <- abaja[as.logical(rowSums(abaja[,1:8])), ]


# Amarga alta

lakecol <- data2 %>% filter(env == "z11_Amarga_alta") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
aalta <- data[,columns]

colnames(aalta) <- data2 %>% filter(env == "z11_Amarga_alta") %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(aalta)){
  for (j in 1:ncol(aalta)){
    if (aalta[i,j]) aalta[i,j] <- 1 
  }
}
aalta <- cbind(aalta, data[,141:145])
aalta <- aalta[as.logical(rowSums(aalta[,1:8])), ]

# Pito

lakecol <- data2 %>% filter(env == "8_Pito") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
pito <- data[,columns]

colnames(pito) <-  data2 %>% filter(env == "8_Pito") %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(pito)){
  for (j in 1:ncol(pito)){
    if (pito[i,j]) pito[i,j] <- 1 
  }
}
pito <- cbind(pito, data[,141:145])
pito <- pito[as.logical(rowSums(pito[,1:11])), ]


# Camaron

lakecol <- data2 %>% filter(env == "4_Camaron") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
camaron <- data[,columns]

colnames(camaron) <- data2 %>% filter(env == "4_Camaron") %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(camaron)){
  for (j in 1:ncol(camaron)){
    if (camaron[i,j]) camaron[i,j] <- 1 
  }
}
camaron <- cbind(camaron, data[,141:145])
camaron <- camaron[as.logical(rowSums(camaron[,1:11])), ]

# Guallar

lakecol <- data2 %>% filter(env == "1_Guallar") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
guallar <- data[,columns]

colnames(guallar) <- data2 %>% filter(env == "1_Guallar") %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(guallar)){
  for (j in 1:ncol(guallar)){
    if (guallar[i,j]) guallar[i,j] <- 1 
  }
}
guallar <- cbind(guallar, data[,141:145])
guallar <- guallar[as.logical(rowSums(guallar[,1:10])), ]


# Muerte

lakecol <- data2 %>% filter(env == "3_Muerte") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
muerte <- data[,columns]
colnames(muerte) <- data2 %>% filter(env == "3_Muerte") %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(muerte)){
  for (j in 1:ncol(muerte)){
    if (muerte[i,j]) muerte[i,j] <- 1 
  }
}
muerte <- cbind(muerte, data[,141:145])
muerte <- muerte[as.logical(rowSums(muerte[,1:7])), ]

# Pez

lakecol <- data2 %>% filter(env == "5_Pez") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()
columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
pez <- data[,columns]
colnames(pez) <- data2 %>% filter(env == "5_Pez") %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(pez)){
  for (j in 1:ncol(pez)){
    if (pez[i,j]) pez[i,j] <- 1 
  }
}
pez <- cbind(pez, data[,141:145])
pez <- pez[as.logical(rowSums(pez[,1:10])), ]


# Taxonomy ----------------------------------------------------------------

# First, we prepare a joint matrix that will help us find the maximum relative
# abundance of each taxonomic group.

lakecol <- 
  data2 %>% 
  filter(env == "5_Pez" | env == "2_Pinol" | env == "z10_Playa" | 
           env == "9_Salineta" | env == "7_Rebollon" | env == "6_Rollico" |
           env == "z12_Amarga_baja" | env == "z11_Amarga_alta" | 
           env == "8_Pito" | env == "4_Camaron" | env == "3_Muerte" |
           env == "1_Guallar") %>% select(sample_ID) %>% 
  c() %>% unname() %>% unlist() %>% as.character()
columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
abund <- data[,columns]
abund <- cbind(abund, data[,141:145])

lakes <- c("playa", "salineta", "pinol", "rebollon", "rollico", "abaja", 
           "aalta", "pito", "camaron", "muerte", "pez", "guallar", "abund")

# We select the phyla with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$tax2, salineta$tax2, pinol$tax2, pez$tax2, rebollon$tax2, rollico$tax2, 
  abaja$tax2, aalta$tax2, pito$tax2, camaron$tax2, muerte$tax2, guallar$tax2)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax2[i] == names.tax)) data$tax2[i] <- "Other"
    if(any(data$tax2[i] == "")) data$tax2[i] <- "Other"
    
  }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when phylum is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax2[i] == "Other")) data$tax3[i] <- "Other"
    if(any(data$tax3[i] == "")) data$tax3[i] <- "Other"
  }
  assign(lakes[n], data)
}

# We select the classes with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$tax3, salineta$tax3, pinol$tax3, pez$tax3, rebollon$tax3, rollico$tax3, 
                                 abaja$tax3, aalta$tax3, pito$tax3, camaron$tax3, muerte$tax3, guallar$tax3)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax3[i] == names.tax)) data$tax3[i] <- "Other"
   }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when class is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax3[i] == "Other")) data$tax4[i] <- "Other"
    if(any(data$tax4[i] == "")) data$tax4[i] <- "Other"
  }
  assign(lakes[n], data)
}

# We select the orders with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$tax4, salineta$tax4, pinol$tax4, 
                                 pez$tax4, rebollon$tax4, rollico$tax4, 
                                 abaja$tax4, aalta$tax4, pito$tax4, 
                                 camaron$tax4, muerte$tax4, guallar$tax4)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax4[i] == names.tax)) data$tax4[i] <- "Other"
  }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when order is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax4[i] == "Other")) data$tax5[i] <- "Other"
    if(any(data$tax5[i] == "")) data$tax5[i] <- "Other"
  }
  assign(lakes[n], data)
}

# We select the families with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$tax5, salineta$tax5, pinol$tax5, 
                                 pez$tax5, rebollon$tax5, rollico$tax5, 
                                 abaja$tax5, aalta$tax5, pito$tax5, 
                                 camaron$tax5, muerte$tax5, guallar$tax5)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax5[i] == names.tax)) data$tax5[i] <- "Other"
  }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when family is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax5[i] == "Other")) data$tax6[i] <- "Other"
    if(any(data$tax6[i] == "")) data$tax6[i] <- "Other"
  }
  assign(lakes[n], data)
}

# We select the genera with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$tax6, salineta$tax6, pinol$tax6, 
                                 pez$tax6, rebollon$tax6, rollico$tax6, 
                                 abaja$tax6, aalta$tax6, pito$tax6, 
                                 camaron$tax6, muerte$tax6, guallar$tax6)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$tax6[i] == names.tax)) data$tax6[i] <- "Other"
  }
  assign(lakes[n], data)
}

# And finally we append the taxonomies together

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  data$tax3 <- paste(data$tax2, data$tax3)
  data$tax4 <- paste(data$tax3, data$tax4)
  data$tax5 <- paste(data$tax4, data$tax5)
  data$tax6 <- paste(data$tax5, data$tax6)
  assign(lakes[n], data)
}

# Let's check if there's something odd

table(c(playa$tax6, salineta$tax6, pinol$tax6, 
        pez$tax6, rebollon$tax6, rollico$tax6, 
        abaja$tax6, aalta$tax6, pito$tax6, 
        camaron$tax6, muerte$tax6, guallar$tax6))
# It seems ok.


# Colonization extinction rates estimation for taxonomic groups -----------

rat.phy <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:7, 1:10, 1:10), 
                                       0.001, 0.01, column = "tax2", n=36)

rat.cla <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:7, 1:10, 1:10), 
                                       0.001, 0.01, column = "tax3", n=36)

rat.ord <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:7, 1:10, 1:10), 
                                        0.001, 0.01, column = "tax4", n=36)

rat.fam <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:7, 1:10, 1:10),
                                       0.001, 0.01, column = "tax5", n=36)

rat.gen <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:7, 1:10, 1:10),
                                       0.001, 0.01, column = "tax6", n=36)

rat.phy$p <- 1/rat.phy$e
rat.cla$p <- 1/rat.cla$e
rat.ord$p <- 1/rat.ord$e
rat.fam$p <- 1/rat.fam$e
rat.gen$p <- 1/rat.gen$e


# Core - satellite distinction --------------------------------------------

# First we calculate the max abundance in the whole dataset for each group
sum.abund <- 
  abund %>% select(-c(123:126)) %>% group_by(tax6) %>% summarise_all(sum)

rel.abund <- sum.abund
for(i in 2:123){
  rel.abund[, i] <- sum.abund[, i]/sum(sum.abund[, i])
}
rel.abund <- sum.abund[, 2:123]/colSums(sum.abund[, 2:123])

max.abund <- data.frame(Group = rel.abund$tax6,
                        MA = apply(rel.abund[, 2:123], MARGIN = 1, FUN = max))

# And we join it with the rates of genera
rat.gen <- left_join(rat.gen, max.abund)

# Now we calculate the occupancies
occupancies <- rep(NA, nrow(rat.gen))
for(i in 1:nrow(rat.gen)){
  temp <- NULL
  for(n in 1:(length(lakes) - 1)){
     temp2 <- get(lakes[n]) %>% 
       filter(tax6 == rat.gen[i, 1]) %>% 
       select(1:(ncol(get(lakes[n])) - 5)) %>% unlist() %>% unname()
    temp <- c(temp, temp2)
  }
  occupancies[i] <- mean(temp)
}

rat.gen$Occ <- occupancies

# Separating core and satellite

chow <- rep(NA, 219) 
pvalue <- rep(NA, 219)
for (i in 10:209){
  chow[i] <- sctest(Occ ~ (MA) , type = "Chow", point = i, data = rat.gen, asymptotic = T)$statistic
  pvalue[i] <- sctest(Occ ~ (MA) , type = "Chow", point = i, data = rat.gen, asymptotic = T)$p.value
}
plot(pvalue, type = "l")
which.max(chow)

# There is a structural change in position 113, there the relation between max.
# abundance and occupancy changes. But the structural change can be between
# positions 102 - 126. This delimitates the gray zone in figure 2.

sort(rat.gen$MA)[102]
sort(rat.gen$MA)[126]

fac <- factor(c(rep(1, 113), rep(2, 106)))[rank((rat.gen$MA))]

fmA <- lm(Occ ~ log10(MA), data = rat.gen[fac == 1, ])
fmB <- lm(Occ ~ log10(MA), data = rat.gen[fac == 2, ])
predA <- rep(NA, 44)
predB <- rep(NA, 44)
predA[fac == 1] <- predict(fmA)
predB[fac == 2] <- predict(fmB)

# We consider core those genera with an occupancy higher than the mean of the
# extremes of the two linear relationships.
((max(predA, na.rm = T) + min(predB, na.rm = T))/2) # .2626719

rat.gen$Core <- rat.gen$Occ > ((max(predA, na.rm = T) + min(predB, na.rm = T))/2)

# We calculate occupancy for families
occupancies2 <- rep(NA, nrow(rat.fam))
for(i in 1:nrow(rat.fam)){
  temp <- NULL
  for(n in 1:(length(lakes) - 1)){
    temp2 <- 
      get(lakes[n]) %>% 
      filter(tax5 == rat.fam[i, 1]) %>% 
      select(1:(ncol(get(lakes[n])) - 5)) %>% unlist() %>% unname()
    temp <- c(temp, temp2)
  }
  occupancies2[i] <- mean(temp)
}

rat.fam$Occ <- occupancies2

# Now we recycle the boundary of core groups for the families

rat.fam$Core <- rat.fam$Occ > ((max(predA, na.rm = T) + min(predB, na.rm = T))/2)

# Core species follow a log normal ----------------------------------------

rat.ma <- (unlist(rat.gen %>% filter(Core == T) %>% select(MA)))

rat.radlog <- vegan::rad.lognormal(rat.ma, family = Gamma)

# Are genera/families closer within groups than between groups? --------------------

temp <- as.character(rat.gen$Group)
temp
temp[2] <- "Acidobacteria Holophagae Subgroup-10 ABS-19 Other"
temp[3] <- "Acidobacteria Holophagae Subgroup-10 Other Other"
temp[5] <- "Actinobacteria Acidimicrobiia Acidimicrobiales OM1-clade Other"
temp[20] <- "Bacteroidetes Bacteroidia Bacteroidia-Incertae-Sedis Draconibacteriaceae uncultured"
temp[28] <- "Bacteroidetes Cytophagia Order-III Other Other"
temp[29] <- "Bacteroidetes Cytophagia Order-III uncultured Other"
temp[30] <- "Bacteroidetes Cytophagia Order-III Unknown-Family Fodinibius"
temp[31] <- "Bacteroidetes Cytophagia Order-III Unknown-Family Other"
temp[32] <- "Bacteroidetes Cytophagia Order-III Unknown-Family uncultured"
temp[33] <- "Bacteroidetes Cytophagia Order-II Rhodothermaceae Salinibacter"
temp[34] <- "Bacteroidetes Cytophagia Order-II Rhodothermaceae uncultured"
temp[48] <- "Bacteroidetes Sphingobacteriia Sphingobacteriales NS11-12-marine-group Other"   
temp[56] <- "Candidate-division-OP3 Other Other Other Other"                                            
temp[57] <- "Candidate-division-SR1 Other Other Other Other"     
temp[81] <- "Firmicutes Clostridia Clostridiales Family-XII Fusibacter"                
temp[91] <- "Gemmatimonadetes Gemmatimonadetes AT425-EubC11-terrestrial-group Other Other"              
temp[92] <- "Gemmatimonadetes Gemmatimonadetes BD2-11-terrestrial-group Other Other"                    
temp[94] <- "Gemmatimonadetes Gemmatimonadetes PAUC43f-marine-benthic-group Other Other"                
temp[95] <- "Gemmatimonadetes Gemmatimonadetes S0134-terrestrial-group Other Other" 
temp[138] <- "Proteobacteria Alphaproteobacteria Rickettsiales Rickettsiales-Incertae-Sedis Other"
temp[153] <- "Proteobacteria Deltaproteobacteria Bdellovibrionales Bdellovibrionaceae OM27-clade"
temp[189] <- "Proteobacteria Gammaproteobacteria Oceanospirillales OM182-clade Other"  
temp[191] <- "Proteobacteria Gammaproteobacteria Order-Incertae-Sedis Family-Incertae-Sedis Marinicella" 
temp[207] <- "Spirochaetae Spirochaetes Spirochaetales Spirochaetaceae Spirochaeta-2"  
temp[209] <- "Tenericutes Mollicutes Mollicutes-RF9 Other Other"                       
temp[212] <- "Verrucomicrobia OPB35-soil-group Other Other Other"                      
temp[214] <- "Verrucomicrobia Opitutae Puniceicoccales Puniceicoccaceae marine-group"  


strs <- str_split(temp, " ", simplify = T)
strs
same <- outer(strs[, 1], strs[, 1], FUN = "==")

#Phyla
distance <- dist(log(rat.gen[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Class
str_temp <- paste0(strs[, 1], strs[, 2])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(rat.gen[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

#Order
str_temp <- paste0(str_temp, strs[, 3])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(rat.gen[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

### Now with family

temp <- as.character(rat.fam$Group)
temp
temp[2] <- "Acidobacteria Holophagae Subgroup-10 ABS-19"
temp[3] <- "Acidobacteria Holophagae Subgroup-10 Other"
temp[5] <- "Actinobacteria Acidimicrobiia Acidimicrobiales OM1-clade"
temp[20] <- "Bacteroidetes Bacteroidia Bacteroidia-Incertae-Sedis Draconibacteriaceae"
temp[26] <- "Bacteroidetes Cytophagia Order-III Other"
temp[27] <- "Bacteroidetes Cytophagia Order-III uncultured"
temp[28] <- "Bacteroidetes Cytophagia Order-III Unknown-Family"
temp[25] <- "Bacteroidetes Cytophagia Order-II Rhodothermaceae"
temp[34] <- "Bacteroidetes Sphingobacteriia Sphingobacteriales NS11-12-marine-group"   
temp[38] <- "Candidate-division-OP3 Other Other Other"                                            
temp[39] <- "Candidate-division-SR1 Other Other Other"     
temp[61] <- "Firmicutes Clostridia Clostridiales Clostridiaceae-3"
temp[62] <- "Firmicutes Clostridia Clostridiales Family-XII"                
temp[71] <- "Gemmatimonadetes Gemmatimonadetes AT425-EubC11-terrestrial-group Other"              
temp[72] <- "Gemmatimonadetes Gemmatimonadetes BD2-11-terrestrial-group Other"                    
temp[74] <- "Gemmatimonadetes Gemmatimonadetes PAUC43f-marine-benthic-group Other"                
temp[75] <- "Gemmatimonadetes Gemmatimonadetes S0134-terrestrial-group Other" 
temp[107] <- "Proteobacteria Alphaproteobacteria Rickettsiales Rickettsiales-Incertae-Sedis"
temp[151] <- "Proteobacteria Gammaproteobacteria Oceanospirillales OM182-clade"  
temp[153] <- "Proteobacteria Gammaproteobacteria Order-Incertae-Sedis Family-Incertae-Sedis" 
temp[169] <- "Tenericutes Mollicutes Mollicutes-RF9 Other"                       
temp[172] <- "Verrucomicrobia OPB35-soil-group Other Other"                      

strs <- str_split(temp, " ", simplify = T)

#Phyla
same <- outer(strs[, 1], strs[, 1], FUN = "==")

distance <- dist(log(rat.fam[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Class
str_temp <- paste0(strs[, 1], strs[, 2])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(rat.fam[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))

# Order
str_temp <- paste0(str_temp, strs[, 3])
same <- outer(str_temp, str_temp, FUN = "==")

distance <- dist(log(rat.fam[, c(2,10)]), upper = T, diag = T)
median((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]])
median((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])

kruskal.test(list(((as.matrix(distance)[upper.tri(same)])[same[upper.tri(same)]]),
                  ((as.matrix(distance)[upper.tri(as.matrix(distance))])[!same[upper.tri(same)]])))
