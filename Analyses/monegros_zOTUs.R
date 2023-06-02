
# Load packages -----------------------------------------------------------

library(tidyverse)
library(island)
library(strucchange)

# Read data ---------------------------------------------------------------

data <- read.csv("Data/zOTU_table_bacteria_monegros_raw.txt", sep = "", check.names = F)
library(readr)
data2 <- read.delim("Data/metadata_bacteria_monegros.txt")
data3 <- read.delim("Data/taxonomy_bacteria_filtered_monegros.txt")
data <- data[rownames(data) %>% order(), ]
data3 <- data3[rownames(data3) %>% order(), ]

# Prepare data ------------------------------------------------------------

# There are several lakes that we are pooling to obtain the colonization and
# extinction rates. Now we prepare separately each lake.



# Playa

lakecol <-
  data2 %>% filter(env == "z10_Playa") %>% arrange(doy_acc) %>% rownames()
  # select(sample_ID) %>% 
  # c() %>% unname() %>% unlist() %>% as.character()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
playa <- data[,columns]


colnames(playa) <- data2 %>% filter(env == "z10_Playa") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()


for (i in 1:nrow(playa)){
  for (j in 1:ncol(playa)){
    if (playa[i,j]) playa[i,j] <- 1 
  }
}
playa <- cbind(playa, data3)
playa <- playa[as.logical(rowSums(playa[,1:15])), ]

# Salineta

lakecol <- data2 %>% filter(env == "9_Salineta") %>% arrange(doy_acc) %>% rownames()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}

salineta <- data[,columns]
colnames(salineta) <- data2 %>% filter(env == "9_Salineta") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(salineta)){
  for (j in 1:ncol(salineta)){
    if (salineta[i,j]) salineta[i,j] <- 1 
  }
}
salineta <- cbind(salineta, data3)
salineta <- salineta[as.logical(rowSums(salineta[,1:16])), ]

#Piñol

lakecol <- data2 %>% filter(env == "2_Pinol") %>% arrange(doy_acc) %>% rownames()

# The lake dried at some point in time. We do not include empty columns.

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
pinol <- data[,columns]
colnames(pinol) <- data2 %>% filter(env == "2_Pinol") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(pinol)){
  for (j in 1:ncol(pinol)){
    if (pinol[i,j]) pinol[i,j] <- 1 
  }
}
pinol <- cbind(pinol, data3)
pinol <- pinol[as.logical(rowSums(pinol[,1:10])), ]

#Rebollon

lakecol <- data2 %>% filter(env == "7_Rebollon") %>% arrange(doy_acc) %>% rownames()

lakecol <- as.character(lakecol)
columns <- c()

for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
rebollon <- data[,columns]

colnames(rebollon) <- data2 %>% filter(env == "7_Rebollon") %>% 
  arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(rebollon)){
  for (j in 1:ncol(rebollon)){
    if (rebollon[i,j]) rebollon[i,j] <- 1 
  }
}
rebollon <- cbind(rebollon, data3)
rebollon <- rebollon[as.logical(rowSums(rebollon[,1:7])), ]

# Rollico

lakecol <- data2 %>% filter(env == "6_Rollico") %>% arrange(doy_acc) %>% rownames()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
rollico <- data[,columns]

colnames(rollico) <- data2 %>% filter(env == "6_Rollico") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()


for (i in 1:nrow(rollico)){
  for (j in 1:ncol(rollico)){
    if (rollico[i,j]) rollico[i,j] <- 1 
  }
}
rollico <- cbind(rollico, data3)
rollico <- rollico[as.logical(rowSums(rollico[,1:9])), ]

# Amarga baja

lakecol <- data2 %>% filter(env == "z12_Amarga_baja") %>% arrange(doy_acc) %>% rownames()

columns <- c()

for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}

abaja <- data[,columns]

colnames(abaja) <- data2 %>% filter(env == "z12_Amarga_baja") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()
  

for (i in 1:nrow(abaja)){
  for (j in 1:ncol(abaja)){
    if (abaja[i,j]) abaja[i,j] <- 1 
  }
}
abaja <- cbind(abaja, data3)
abaja <- abaja[as.logical(rowSums(abaja[,1:8])), ]


# Amarga alta

lakecol <- data2 %>% filter(env == "z11_Amarga_alta") %>% arrange(doy_acc) %>% rownames()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
aalta <- data[,columns]

colnames(aalta) <- data2 %>% filter(env == "z11_Amarga_alta") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()

for (i in 1:nrow(aalta)){
  for (j in 1:ncol(aalta)){
    if (aalta[i,j]) aalta[i,j] <- 1 
  }
}
aalta <- cbind(aalta, data3)
aalta <- aalta[as.logical(rowSums(aalta[,1:8])), ]

# Pito

lakecol <- data2 %>% filter(env == "8_Pito") %>% arrange(doy_acc) %>% rownames()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
pito <- data[,columns]

colnames(pito) <-  data2 %>% filter(env == "8_Pito") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(pito)){
  for (j in 1:ncol(pito)){
    if (pito[i,j]) pito[i,j] <- 1 
  }
}
pito <- cbind(pito, data3)
pito <- pito[as.logical(rowSums(pito[,1:11])), ]


# Camaron

lakecol <- data2 %>% filter(env == "4_Camaron") %>% arrange(doy_acc) %>% rownames()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
camaron <- data[,columns]

colnames(camaron) <- data2 %>% filter(env == "4_Camaron") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(camaron)){
  for (j in 1:ncol(camaron)){
    if (camaron[i,j]) camaron[i,j] <- 1 
  }
}
camaron <- cbind(camaron, data3)
camaron <- camaron[as.logical(rowSums(camaron[,1:11])), ]

# Guallar

lakecol <- data2 %>% filter(env == "1_Guallar") %>% arrange(doy_acc) %>% rownames()

columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
guallar <- data[,columns]

colnames(guallar) <- data2 %>% filter(env == "1_Guallar") %>% 
  arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(guallar)){
  for (j in 1:ncol(guallar)){
    if (guallar[i,j]) guallar[i,j] <- 1 
  }
}
guallar <- cbind(guallar, data3)
guallar <- guallar[as.logical(rowSums(guallar[,1:9])), ]


# Muerte

lakecol <- data2 %>% filter(env == "3_Muerte") %>% arrange(doy_acc) %>% rownames()
columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
muerte <- data[,columns]
colnames(muerte) <- data2 %>% filter(env == "3_Muerte") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(muerte)){
  for (j in 1:ncol(muerte)){
    if (muerte[i,j]) muerte[i,j] <- 1 
  }
}
muerte <- cbind(muerte, data3)
muerte <- muerte[as.logical(rowSums(muerte[,1:6])), ]

# Pez

lakecol <- data2 %>% filter(env == "5_Pez") %>% arrange(doy_acc) %>% rownames()
columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
pez <- data[,columns]
colnames(pez) <- data2 %>% filter(env == "5_Pez") %>% arrange(doy_acc) %>% select(doy_acc) %>% 
  unlist() %>% unname()
for (i in 1:nrow(pez)){
  for (j in 1:ncol(pez)){
    if (pez[i,j]) pez[i,j] <- 1 
  }
}
pez <- cbind(pez, data3)
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
           env == "1_Guallar") %>% rownames()
columns <- c()
for (i in 1:length(lakecol)){
  columns <- c(columns, which(colnames(data) == lakecol[i]))
}
abund <- data[,columns]
abund <- cbind(abund, data3)

lakes <- c("playa", "salineta", "pinol", "rebollon", "rollico", "abaja", 
           "aalta", "pito", "camaron", "muerte", "pez", "guallar", "abund")

# We select the phyla with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$Phylum, salineta$Phylum, pinol$Phylum, 
                                 pez$Phylum, rebollon$Phylum, rollico$Phylum, 
  abaja$Phylum, aalta$Phylum, pito$Phylum, camaron$Phylum, muerte$Phylum, 
  guallar$Phylum)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Phylum[i] == names.tax)) data$Phylum[i] <- "Other"
    if(any(data$Phylum[i] == "")) data$Phylum[i] <- "Other"
    if(any(data$Phylum[i] == "Bacteria-unclass")) data$Phylum[i] <- "Other"
  }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when phylum is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Phylum[i] == "Other")) data$Class[i] <- "Other"
    if(any(data$Class[i] == "")) data$Class[i] <- "Other"
    if(any(data$Class[i] == "Lentisphaerae-unclass")) data$Class[i] <- "Other"

    
  }
  assign(lakes[n], data)
}

# We select the classes with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$Class, salineta$Class, pinol$Class, 
                                 pez$Class, rebollon$Class, rollico$Class, 
                                 abaja$Class, aalta$Class, pito$Class, 
                                 camaron$Class, muerte$Class, guallar$Class)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Class[i] == names.tax)) data$Class[i] <- "Other"
   }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when class is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Class[i] == "Other")) data$Order[i] <- "Other"
    if(any(data$Order[i] == "")) data$Order[i] <- "Other"
    if(any(data$Order[i] == "uncultured")) data$Order[i] <- "Other"
    if(any(data$Order[i] == "Gracilibacteria-unclass")) data$Order[i] <- "Other"
    if(any(data$Order[i] == "Parcubacteria-unclass")) data$Order[i] <- "Other"
    if(any(data$Order[i] == "Alphaproteobacteria-unclass")) data$Order[i] <- "Other"
    if(any(data$Order[i] == "Deltaproteobacteria-unclass")) data$Order[i] <- "Other"
    if(any(data$Order[i] == "Gammaproteobacteria-unclass")) data$Order[i] <- "Other"
     }
  assign(lakes[n], data)
}

# We select the orders with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$Order, salineta$Order, pinol$Order, 
                                 pez$Order, rebollon$Order, rollico$Order, 
                                 abaja$Order, aalta$Order, pito$Order, 
                                 camaron$Order, muerte$Order, guallar$Order)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Order[i] == names.tax)) data$Order[i] <- "Other"
  }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when order is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Order[i] == "Other")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "uncultured")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Chlamydiales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Nostocales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Babeliales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Bacillales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Clostridiales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Micavibrionales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Rickettsiales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Oceanospirillales-unclass")) data$Family[i] <- "Other"
    if(any(data$Family[i] == "Cytophagales-unclass")) data$Family[i] <- "Other"
    
  }
  assign(lakes[n], data)
}

# We select the families with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$Family, salineta$Family, pinol$Family, 
                                 pez$Family, rebollon$Family, rollico$Family, 
                                 abaja$Family, aalta$Family, pito$Family, 
                                 camaron$Family, muerte$Family, guallar$Family)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Family[i] == names.tax)) data$Family[i] <- "Other"
  }
  assign(lakes[n], data)
}

# Now we change missing names to Other, and when family is Other we do it too.
for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Family[i] == "Other")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "uncultured")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Microbacteriaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Nitriliruptoraceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Cyclobacteriaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Crocinitomicaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Cryomorphaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Flavobacteriaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Balneolaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Parachlamydiaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Simkaniaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Paenibacillaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Planococcaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Halobacteroidaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Phycisphaeraceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Pirellulaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Azospirillaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Beijerinckiaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Rhodobacteraceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Rhodospirillaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Rickettsiaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Sphingomonadaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Bacteriovoracaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Desulfobulbaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Oligoflexaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Alteromonadaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Burkholderiaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Methylophilaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Cellvibrionaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Halieaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Unknown Family-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Nitrococcaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Opitutaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Puniceicoccaceae-unclass")) data$Genus[i] <- "Other"
    if(any(data$Genus[i] == "Pedosphaeraceae-unclass")) data$Genus[i] <- "Other"

    
  }
  assign(lakes[n], data)
}

# We select the genera with less than 36 OTUs throughtout the dataset to change it to Other. 
names.tax <- names(which(table(c(playa$Genus, salineta$Genus, pinol$Genus, 
                                 pez$Genus, rebollon$Genus, rollico$Genus, 
                                 abaja$Genus, aalta$Genus, pito$Genus, 
                                 camaron$Genus, muerte$Genus, guallar$Genus)) < 36))

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  for(i in 1:nrow(data)){
    if(any(data$Genus[i] == names.tax)) data$Genus[i] <- "Other"
  }
  assign(lakes[n], data)
}

# And finally we append the taxonomies together

for(n in 1:length(lakes)){
  data <- get(lakes[n])
  data$Class <- paste(data$Phylum, data$Class)
  data$Order <- paste(data$Class, data$Order)
  data$Family <- paste(data$Order, data$Family)
  data$Genus <- paste(data$Family, data$Genus)
  assign(lakes[n], data)
}

# Let's check if there's something odd

View(table(c(playa$Genus, salineta$Genus, pinol$Genus, 
        pez$Genus, rebollon$Genus, rollico$Genus, 
        abaja$Genus, aalta$Genus, pito$Genus, 
        camaron$Genus, muerte$Genus, guallar$Genus)))
# Seems ok...

# Colonization extinction rates estimation for taxonomic groups -----------

rat.phy <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:6, 1:10, 1:9), 
                                       0.001, 0.01, column = "Phylum", n=36)

rat.cla <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:6, 1:10, 1:9), 
                                       0.001, 0.01, column = "Class", n=36)

rat.ord <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:6, 1:10, 1:9), 
                                        0.001, 0.01, column = "Order", n=36)

rat.fam <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:6, 1:10, 1:9),
                                       0.001, 0.01, column = "Family", n=36)

rat.gen <- irregular_multiple_datasets(list(playa, salineta,  
                                            pinol, rebollon, rollico, abaja, 
                                            aalta, pito, camaron, muerte, pez, guallar), 
                                       list(1:15, 1:16, 1:10, 1:7, 1:9, 
                                            1:8, 1:8, 1:11, 1:11, 1:6, 1:10, 1:9),
                                       0.001, 0.01, column = "Genus", n = 36)

rat.phy$p <- 1/rat.phy$e
rat.cla$p <- 1/rat.cla$e
rat.ord$p <- 1/rat.ord$e
rat.fam$p <- 1/rat.fam$e
rat.gen$p <- 1/rat.gen$e


# Core - satellite distinction --------------------------------------------

# First we calculate the max abundance in the whole dataset for each group
sum.abund <- 
  abund %>% select(-c(121:126)) %>% group_by(Genus) %>% summarise_all(sum)

rel.abund <- sum.abund
for(i in 2:121){
  rel.abund[, i] <- sum.abund[, i]/sum(sum.abund[, i])
}
rel.abund <- sum.abund[, 2:121]/colSums(sum.abund[, 2:121])

max.abund <- data.frame(Group = rel.abund$Genus,
                        MA = apply(rel.abund[, 2:120], MARGIN = 1, FUN = max))

# And we join it with the rates of genera
rat.gen <- left_join(rat.gen, max.abund)

# Now we calculate the occupancies
occupancies <- rep(NA, nrow(rat.gen))
for(i in 1:nrow(rat.gen)){
  temp <- NULL
  for(n in 1:(length(lakes) - 1)){
     temp2 <- get(lakes[n]) %>% 
       filter(Genus == rat.gen[i, 1]) %>% 
       select(1:(ncol(get(lakes[n])) - 7)) %>% unlist() %>% unname()
    temp <- c(temp, temp2)
  }
  occupancies[i] <- mean(temp)
}

rat.gen$Occ <- occupancies

# Separating core and satellite

chow <- rep(NA, 230) 
pvalue <- rep(NA, 230)
for (i in 10:220){
  chow[i] <- sctest(Occ ~ (MA) , type = "Chow", point = i, data = rat.gen, asymptotic = T)$statistic
  pvalue[i] <- sctest(Occ ~ (MA) , type = "Chow", point = i, data = rat.gen, asymptotic = T)$p.value
}
plot(pvalue, type = "l")
which.max(chow)

# There is a structural change in position 111, there the relation between max.
# abundance and occupancy changes. But the structural change can be between
# positions 28 - 149. This delimitates the gray zone in figure XXX.

sort(rat.gen$MA)[28]
sort(rat.gen$MA)[149]

fac <- factor(c(rep(1, 111), rep(2, 119)))[rank((rat.gen$MA))]

fmA <- lm(Occ ~ log10(MA), data = rat.gen[fac == 1, ])
fmB <- lm(Occ ~ log10(MA), data = rat.gen[fac == 2, ])
predA <- rep(NA, 44)
predB <- rep(NA, 44)
predA[fac == 1] <- predict(fmA)
predB[fac == 2] <- predict(fmB)

# We consider core those genera with an occupancy higher than the mean of the
# extremes of the two linear relationships.
((max(predA, na.rm = T) + min(predB, na.rm = T))/2) # .2513547

rat.gen$Core <- rat.gen$Occ > ((max(predA, na.rm = T) + min(predB, na.rm = T))/2)

# We calculate occupancy for families
occupancies2 <- rep(NA, nrow(rat.fam))
for(i in 1:nrow(rat.fam)){
  temp <- NULL
  for(n in 1:(length(lakes) - 1)){
    temp2 <- 
      get(lakes[n]) %>% 
      filter(Family == rat.fam[i, 1]) %>% 
      select(1:(ncol(get(lakes[n])) - 7)) %>% unlist() %>% unname()
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

