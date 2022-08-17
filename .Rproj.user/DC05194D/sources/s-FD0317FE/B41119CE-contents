
# Load packages -----------------------------------------------------------

library(tidyverse)
library(island)


# Preparing data ----------------------------------------------------------

# bac comes from monegros.R

temp <- bac %>% filter(Site == "Ermatingen", Treatment == "C0")
temp 

temp[6934:(6933*2)]
temp2 <- temp[1:6933,]
temp2[, 18:21] <- temp[(6933 + 1):(6933 * 2),2:5]
temp2[, 22:25] <- temp[(6933*2 + 1):(6933 * 3),2:5]

temp3 <- temp2 

temp <- bac %>% filter(Site == "Ermatingen", Treatment == "C1")
temp 

temp[6934:(6933*2), ]
temp2 <- temp[1:6933,]
temp2[, 18:21] <- temp[(6933 + 1):(6933 * 2),2:5]
temp2[, 22:25] <- temp[(6933*2 + 1):(6933 * 3),2:5]

temp3 <- rbind(temp3, temp2)

temp <- bac %>% filter(Site == "Ermatingen", Treatment == "C2")
temp2 <- temp[1:6933,]
temp2[, 18:21] <- temp[(6933 + 1):(6933 * 2),2:5]
temp2[, 22:25] <- temp[(6933*2 + 1):(6933 * 3),2:5]

temp3 <- rbind(temp3, temp2)

temp <- bac %>% filter(Site == "Heiteren", Treatment == "C0")
temp2 <- temp[1:6933,]
temp2[, 18:21] <- temp[(6933 + 1):(6933 * 2),2:5]
temp2[, 22:25] <- temp[(6933*2 + 1):(6933 * 3),2:5]

temp3 <- rbind(temp3, temp2)

temp <- bac %>% filter(Site == "Heiteren", Treatment == "C1")
temp2 <- temp[1:6933,]
temp2[, 18:21] <- temp[(6933 + 1):(6933 * 2),2:5]
temp2[, 22:25] <- temp[(6933*2 + 1):(6933 * 3),2:5]

temp3 <- rbind(temp3, temp2)

temp <- bac %>% filter(Site == "Heiteren", Treatment == "C2")
temp2 <- temp[1:6933,]
temp2[, 18:21] <- temp[(6933 + 1):(6933 * 2),2:5]
temp2[, 22:25] <- temp[(6933*2 + 1):(6933 * 3),2:5]

temp3 <- rbind(temp3, temp2)

colnames(temp3)
temp4 <- temp3[c(2:5,18:25, 11, 13:16)]

table(temp4$Phylum)
bac.phy

taxonomy <- rbind(bac.ab[1:6933, 13:16], bac.ab[1:6933, 13:16], bac.ab[1:6933, 13:16],
                  bac.ab[1:6933, 13:16], bac.ab[1:6933, 13:16], bac.ab[1:6933, 13:16])

temptax <- cbind(temp4[,-c(14:17)], taxonomy)



temp5 <- (temp4 %>% group_by(Phylum) %>% filter(n() > 59)) %>% 
  arrange(Phylum) %>% ungroup()

temp5$Phylum <- as.character(temp5$Phylum)

temp5 <- data.frame(temp5)


# Prepare to estimate phyla -----------------------------------------------

Time <- rep(c(30, 180, 365, 1460), 3)
unique
tag <- substr(unique(temp5$Phylum), 1, 3)
mss_cedp(Data = temp5[, c(13, 1:12)], Time = Time, Factor = 1, Tag = tag,
         PerfectDetectability = F, z = 4)
nrow(temp5)
temp6 <- split(temp5, temp5$Phylum)
summary(temp5)



Data1 <- temp5[, c(13, 1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)]
Data1$Phylum <- as.factor(Data1$Phylum)
Name_of_Factors <- c("Phylum")
Factors <- Filter(is.factor, Data1)
No_of_Factors <- length(Factors[1,])
n <- No_of_Factors + 1

factor(Data1$Phylum)[1]
Data1$Phylum == factor(Data1$Phylum)[1]

D1 <- as.matrix(Data1[Data1$Phylum == levels(Data1$Phylum)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1 <- sss_cedp(P1, Time_Vector, Transects,
               Colonization = rat.phy$c[1]/100, Extinction = rat.phy$e[1]/10, 
               Detectability=.5,
               Phi_Time_0=.5,
               Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1

detect_res <- (unlist(R1))

detect_res

for(i in 2:28){
  D1 <- as.matrix(Data1[Data1$Phylum == levels(Data1$Phylum)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1 <- sss_cedp(P1, Time_Vector, Transects,
                 Colonization = rat.phy$c[i], Extinction = rat.phy$e[i], 
                 Detectability=.5,
                 Phi_Time_0=.5,
                 Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_res <- rbind(detect_res, R1)  
}
groups <- levels(Data1$Phylum)

detect_res <- cbind(groups, detect_res)

# Check if the estimates do not change
D1 <- as.matrix(Data1[Data1$Phylum == levels(Data1$Phylum)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1b <- sss_cedp(P1, Time_Vector, Transects,
                Colonization = rat.phy$c[1]/100, Extinction = rat.phy$e[1]/100, 
                Detectability=.5,
                Phi_Time_0=.5,
                Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1b

detect_resb <- (unlist(R1b))

detect_resb

for(i in 2:28){
  D1 <- as.matrix(Data1[Data1$Phylum == levels(Data1$Phylum)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1b <- sss_cedp(P1, Time_Vector, Transects,
                  Colonization = rat.phy$c[i]/100, Extinction = rat.phy$e[i]/100, 
                  Detectability=.5,
                  Phi_Time_0=.5,
                  Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_resb <- rbind(detect_resb, R1b)  
}



# Good: 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 22, 23, 25, 26, 27, 28 
# Maybe good with change: 21
# Maybe good as is: 24

abs((as.numeric(matrix(unlist(detect_res), nrow = 28)[,2]) - 
       matrix(unlist(detect_resb), nrow = 28)[, 1])/
      as.numeric(matrix(unlist(detect_res), nrow = 28)[,2])) > .01   

unlist(detect_res[abs((as.numeric(matrix(unlist(detect_res), nrow = 28)[,2]) - 
                         matrix(unlist(detect_resb), nrow = 28)[, 1])/
                        as.numeric(matrix(unlist(detect_res), nrow = 28)[,2])) > .01   , 1])


cedp.phy <- detect_res

# Now the classes ---------------------------------------------------------

temp5 <- (temptax %>% group_by(Class) %>% filter(n() > 59)) %>% 
  arrange(Class) %>% ungroup()

unique(temp5$Class)

temp5 <- data.frame(temp5)

Data1 <- temp5[, c(14, 1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)]
Data1$Class <- as.factor(Data1$Class)
Name_of_Factors <- c("Class")
Factors <- Filter(is.factor, Data1)
No_of_Factors <- length(Factors[1,])
n <- No_of_Factors + 1

factor(Data1$Class)[1]
Data1$Class == factor(Data1$Class)[1]

D1 <- as.matrix(Data1[Data1$Class == levels(Data1$Class)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1 <- sss_cedp(P1, Time_Vector, Transects,
               Colonization = rat.cla$c[1]/100, Extinction = rat.cla$e[1]/100, 
               Detectability=.5,
               Phi_Time_0=.5,
               Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1

detect_res <- (unlist(R1))

detect_res

for(i in 2:66){
  D1 <- as.matrix(Data1[Data1$Class == levels(Data1$Class)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1 <- sss_cedp(P1, Time_Vector, Transects,
                 Colonization = rat.cla$c[i], Extinction = rat.cla$e[i], 
                 Detectability=.5,
                 Phi_Time_0=.5,
                 Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_res <- rbind(detect_res, R1)  
}
# detect_res <- R1
groups <- levels(Data1$Class)

detect_res <- cbind(groups, detect_res)

# Check if the estimates do not change 
k <- 3
D1 <- as.matrix(Data1[Data1$Class == levels(Data1$Class)[k],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1 <- sss_cedp(P1, Time_Vector, Transects,
               Colonization = rat.cla$c[k], Extinction = rat.cla$e[k], 
               Detectability=.5,
               Phi_Time_0=.5,
               Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1

R1 <- sss_cedp(P1, Time_Vector, Transects,
               Colonization = rat.cla$c[k]/20, Extinction = rat.cla$e[k]/10, 
               Detectability=.5,
               Phi_Time_0=.5,
               Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1

# Good ones: 2, 3, 
# Bad ones: 1,
# This would take much time, better automatize it

#### Diffferent starting point
D1 <- as.matrix(Data1[Data1$Class == levels(Data1$Class)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1b <- sss_cedp(P1, Time_Vector, Transects,
                Colonization = rat.cla$c[1]/100, Extinction = rat.cla$e[1]/100,
                Detectability=.5,
                Phi_Time_0=.5,
                Tol=1.0e-8, Verbose = 0, MIT = 1000)


detect_resb <- unlist(R1b)

for(i in 2:66){
  D1 <- as.matrix(Data1[Data1$Class == levels(Data1$Class)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1b <- sss_cedp(P1, Time_Vector, Transects,
                  Colonization = rat.cla$c[i]/100, Extinction = rat.cla$e[i]/100,
                  Detectability=.5,
                  Phi_Time_0=.5,
                  Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_resb <- rbind(detect_resb, R1b)
}
detect_resb
abs((as.numeric(matrix(unlist(detect_res), nrow = 66)[,2]) - 
       matrix(unlist(detect_resb), nrow = 66)[, 1])/
      as.numeric(matrix(unlist(detect_res), nrow = 66)[,2])) > .01   

(as.numeric(matrix(unlist(detect_res), nrow = 66)[,6]) - 
    matrix(unlist(detect_resb), nrow = 66)[, 5]) > .01   


matrix(unlist(detect_res), nrow = 66)
matrix(unlist(detect_resb), nrow = 66)

# Bad ones: 1, 14, 22, 29, 31, 34, 36, 40, 47, 54, 59, 62

cedp.cla <- detect_res
# Now orders --------------------------------------------------------------

temp5 <- (temptax %>% group_by(Order) %>% filter(n() > 59)) %>% 
  arrange(Order) %>% ungroup()

unique(temp5$Order)

temp5 <- data.frame(temp5)

Data1 <- temp5[, c(15, 1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)]
Data1$Order <- as.factor(Data1$Order)
Name_of_Factors <- c("Order")
Factors <- Filter(is.factor, Data1)
No_of_Factors <- length(Factors[1,])
n <- No_of_Factors + 1

factor(Data1$Order)[1]
Data1$Order == factor(Data1$Order)[1]

D1 <- as.matrix(Data1[Data1$Order == levels(Data1$Order)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1 <- sss_cedp(P1, Time_Vector, Transects,
               Colonization = rat.ord$c[1], Extinction = rat.ord$e[1], 
               Detectability=.5,
               Phi_Time_0=.5,
               Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1

detect_res <- (unlist(R1))

detect_res

for(i in 2:106){
  D1 <- as.matrix(Data1[Data1$Order == levels(Data1$Order)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1 <- sss_cedp(P1, Time_Vector, Transects,
                 Colonization = rat.ord$c[i], Extinction = rat.ord$e[i], 
                 Detectability=.5,
                 Phi_Time_0=.5,
                 Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_res <- rbind(detect_res, R1)  
}
groups <- levels(Data1$Order)

detect_res <- cbind(groups, detect_res)


#### Diffferent starting point
D1 <- as.matrix(Data1[Data1$Order == levels(Data1$Order)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1b <- sss_cedp(P1, Time_Vector, Transects,
                Colonization = rat.ord$c[1]/100, Extinction = rat.ord$e[1]/100,
                Detectability=.5,
                Phi_Time_0=.5,
                Tol=1.0e-8, Verbose = 0, MIT = 1000)


detect_resb <- unlist(R1b)

for(i in 2:106){
  D1 <- as.matrix(Data1[Data1$Order == levels(Data1$Order)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1b <- sss_cedp(P1, Time_Vector, Transects,
                  Colonization = rat.ord$c[i]/100, Extinction = rat.ord$e[i]/100,
                  Detectability=.5,
                  Phi_Time_0=.5,
                  Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_resb <- rbind(detect_resb, R1b)
}
detect_resb
which(abs((as.numeric(matrix(unlist(detect_res), nrow = 106)[,2]) - 
             matrix(unlist(detect_resb), nrow = 106)[, 1])/
            as.numeric(matrix(unlist(detect_res), nrow = 106)[,2])) > .01)   

which((as.numeric(matrix(unlist(detect_res), nrow = 106)[,6]) - 
         matrix(unlist(detect_resb), nrow = 106)[, 5]) > .01   )


matrix(unlist(detect_res), nrow = 66)
matrix(unlist(detect_resb), nrow = 66)

# Bad ones: 1,  19,  27,  29,  39,  41,  45,  46,  50,  56,  64,  77,  86,  88,  92,  97, 100, 101

bad <- c(1,  19,  27,  29,  39,  41,  45,  46,  50,  56,  64,  77,  86,  88,  92,  97, 100, 101)

plot(
  log(matrix(unlist(detect_resb), nrow = 106)[-bad, 1]),
  log(1/matrix(unlist(detect_resb), nrow = 106)[-bad, 2]), ylim = c(4, 10))

cedp.ord <- detect_res

# Now families --------------------------------------------------------------

temp5 <- (temptax %>% group_by(Family) %>% filter(n() > 58)) %>% 
  arrange(Family) %>% ungroup()

unique(temp5$Family)

temp5 <- data.frame(temp5)

Data1 <- temp5[, c(16, 1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)]
Data1$Family <- as.factor(Data1$Family)
Name_of_Factors <- c("Family")
Factors <- Filter(is.factor, Data1)
No_of_Factors <- length(Factors[1,])
n <- No_of_Factors + 1


D1 <- as.matrix(Data1[Data1$Family == levels(Data1$Family)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1 <- sss_cedp(P1, Time_Vector, Transects,
               Colonization = rat.fam$c[1], Extinction = rat.fam$e[1], 
               Detectability=.5,
               Phi_Time_0=.5,
               Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1

detect_res <- (unlist(R1))

detect_res

for(i in 2:135){
  D1 <- as.matrix(Data1[Data1$Family == levels(Data1$Family)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1 <- sss_cedp(P1, Time_Vector, Transects,
                 Colonization = rat.fam$c[i], Extinction = rat.fam$e[i], 
                 Detectability=.5,
                 Phi_Time_0=.5,
                 Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_res <- rbind(detect_res, R1)  
}
groups <- levels(Data1$Family)

detect_res <- cbind(groups, detect_res)


#### Diffferent starting point
D1 <- as.matrix(Data1[Data1$Family == levels(Data1$Family)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1b <- sss_cedp(P1, Time_Vector, Transects,
                Colonization = rat.fam$c[1]/100, Extinction = rat.fam$e[1]/100,
                Detectability=.5,
                Phi_Time_0=.5,
                Tol=1.0e-8, Verbose = 0, MIT = 1000)


detect_resb <- unlist(R1b)

for(i in 2:135){
  D1 <- as.matrix(Data1[Data1$Family == levels(Data1$Family)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1b <- sss_cedp(P1, Time_Vector, Transects,
                  Colonization = rat.fam$c[i]/100, Extinction = rat.fam$e[i]/100,
                  Detectability=.5,
                  Phi_Time_0=.5,
                  Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_resb <- rbind(detect_resb, R1b)
}
detect_resb
which(abs((as.numeric(matrix(unlist(detect_res), nrow = 135)[,2]) - 
             matrix(unlist(detect_resb), nrow = 135)[, 1])/
            as.numeric(matrix(unlist(detect_res), nrow = 135)[,2])) > .01)   

which((as.numeric(matrix(unlist(detect_res), nrow = 135)[,6]) - 
         matrix(unlist(detect_resb), nrow = 135)[, 5]) > .01   )


matrix(unlist(detect_res), nrow = 66)
matrix(unlist(detect_resb), nrow = 66)

# Bad ones: 1,  21,  26,  37,  39,  44,  50,  52,  56,  57,  61,  62,  66,  73,  83,  94, 101, 110, 114, 115, 117, 121, 126, 129, 130

bad <- c(1,  21,  26,  37,  39,  44,  50,  52,  56,  57,  61,  62,  66,  73,  83,  94, 101, 110, 114, 115, 117, 121, 126, 129, 130)

cedp.fam <- detect_res

# And now genera ----------------------------------------------------------

temp5 <- (temptax %>% group_by(Genera) %>% filter(n() > 58)) %>% 
  arrange(Genera) %>% ungroup()

unique(temp5$Genera)

temp5 <- data.frame(temp5)

Data1 <- temp5[, c(17, 1, 5, 9, 2, 6, 10, 3, 7, 11, 4, 8, 12)]
Data1$Genera <- as.factor(Data1$Genera)
Name_of_Factors <- c("Genera")
Factors <- Filter(is.factor, Data1)
No_of_Factors <- length(Factors[1,])
n <- No_of_Factors + 1


D1 <- as.matrix(Data1[Data1$Genera == levels(Data1$Genera)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1 <- sss_cedp(P1, Time_Vector, Transects,
               Colonization = rat.gen$c[1], Extinction = rat.gen$e[1], 
               Detectability=.5,
               Phi_Time_0=.5,
               Tol=1.0e-8, Verbose = 0, MIT = 1000)

R1

detect_res <- (unlist(R1))

detect_res

for(i in 2:144){
  D1 <- as.matrix(Data1[Data1$Genera == levels(Data1$Genera)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1 <- sss_cedp(P1, Time_Vector, Transects,
                 Colonization = rat.gen$c[i], Extinction = rat.gen$e[i], 
                 Detectability=.5,
                 Phi_Time_0=.5,
                 Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_res <- rbind(detect_res, R1)  
}
groups <- levels(Data1$Genera)

detect_res <- cbind(groups, detect_res)


#### Diffferent starting point
D1 <- as.matrix(Data1[Data1$Genera == levels(Data1$Genera)[1],n:ncol(Data1)])
Time <- as.double(c(30, 180, 365, 1460))
P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
# Dealing with time.
Time_Vector <- as.numeric(names(table(Time)))
Transects   <- as.numeric((table(Time))) * 3
R1b <- sss_cedp(P1, Time_Vector, Transects,
                Colonization = rat.gen$c[1]/100, Extinction = rat.gen$e[1]/100,
                Detectability=.5,
                Phi_Time_0=.5,
                Tol=1.0e-8, Verbose = 0, MIT = 1000)

detect_resb <- unlist(R1b)

for(i in 2:144){
  D1 <- as.matrix(Data1[Data1$Genera == levels(Data1$Genera)[i],n:ncol(Data1)])
  Time <- as.double(c(30, 180, 365, 1460))
  P1 <- as.matrix(D1[1:nrow(D1),1:ncol(D1)])
  # Dealing with time.
  Time_Vector <- as.numeric(names(table(Time)))
  Transects   <- as.numeric((table(Time))) * 3
  R1b <- sss_cedp(P1, Time_Vector, Transects,
                  Colonization = rat.gen$c[i]/100, Extinction = rat.gen$e[i]/100,
                  Detectability=.5,
                  Phi_Time_0=.5,
                  Tol=1.0e-8, Verbose = 0, MIT = 1000)
  
  detect_resb <- rbind(detect_resb, R1b)
}
detect_resb
which(abs((as.numeric(matrix(unlist(detect_res), nrow = 144)[,2]) - 
             matrix(unlist(detect_resb), nrow = 144)[, 1])/
            as.numeric(matrix(unlist(detect_res), nrow = 144)[,2])) > .01)   

which((as.numeric(matrix(unlist(detect_res), nrow = 135)[,6]) - 
         matrix(unlist(detect_resb), nrow = 135)[, 5]) > .01   )


matrix(unlist(detect_res), nrow = 66)
matrix(unlist(detect_resb), nrow = 66)

# Bad ones: 1,  21,  26,  37,  39,  44,  50,  52,  56,  57,  61,  62,  66,  73,  83,  94, 101, 110, 114, 115, 117, 121, 126, 129, 130

bad <- c(1,  21,  26,  32,  38,  40,  43,  46,  52,  54,  58,  59,  63,  64,
         69,  70,  74,  78,  86,  87,  89, 101, 108, 117, 119, 120, 122, 123, 
         125, 129, 134, 137, 138, 139)

cedp.gen <- detect_res


# Now we put together both methods, perfect/imperfect detectability -------

cedp.phy[25, 1] <- "Other"

cedp2.phy <- as.data.frame(cedp.phy)
cedp2.phy <- cedp2.phy %>% rename(Group = groups) %>% 
  mutate(Group = unname(unlist(Group)), C = unname(unlist(C)), 
         E = unname(unlist(E)))


phy.tog <- left_join(rat.phy, cedp2.phy, by = "Group")

phy.sli <- phy.tog %>% slice(c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 
                               16, 17, 18, 19, 23, 24, 25, 26, 27, 28))
# Class -------------------------------------------------------------------

cedp2.cla <- as.data.frame(cedp.cla)
cedp2.cla <- cedp2.cla %>% rename(Group = groups) %>% 
  mutate(Group = unname(unlist(Group)), C = unname(unlist(C)), 
         E = unname(unlist(E)))


cla.tog <- left_join(rat.cla, cedp2.cla, by = "Group")


cla.sli <- cla.tog %>% slice(-c(1, 14, 22, 29, 31, 34, 36, 40, 47, 54, 59, 62))
# Order -------------------------------------------------------------------

cedp2.ord <- as.data.frame(cedp.ord)
cedp2.ord <- cedp2.ord %>% rename(Group = groups) %>% 
  mutate(Group = unname(unlist(Group)), C = unname(unlist(C)), 
         E = unname(unlist(E)))


ord.tog <- left_join(rat.ord, cedp2.ord, by = "Group")

ord.sli <- ord.tog %>% slice(-c(1,  19,  27,  29,  39,  41,  45,  46,  50,  56,  64,  77,  86,  88,  92,  97, 100, 101))


# Family -------------------------------------------------------------------

cedp2.fam <- as.data.frame(cedp.fam)
cedp2.fam <- cedp2.fam %>% rename(Group = groups) %>% 
  mutate(Group = unname(unlist(Group)), C = unname(unlist(C)), 
         E = unname(unlist(E)))


fam.tog <- left_join(rat.fam, cedp2.fam, by = "Group")

fam.sli <- fam.tog %>% slice(-c(1,  21,  26,  37,  39,  44,  50,  52,  56,  57,  61,  62,  66,  73,  83,  94, 101, 110, 114, 115, 117, 121, 126, 129, 130))

# Genera -------------------------------------------------------------------

cedp2.gen <- as.data.frame(cedp.gen)
cedp2.gen <- cedp2.gen %>% rename(Group = groups) %>% 
  mutate(Group = unname(unlist(Group)), C = unname(unlist(C)), 
         E = unname(unlist(E)))


gen.tog <- left_join(rat.gen, cedp2.gen, by = "Group")

gen.sli <- gen.tog %>% slice(-c(1,  21,  26,  32,  38,  40,  43,  46,  52,  54,  58,  59,  63,  64,
                                69,  70,  74,  78,  86,  87,  89, 101, 108, 117, 119, 120, 122, 123, 
                                125, 129, 134, 137, 138, 139))

# We clean data a bit more -----------------------------------------------------------

phy.sli <- phy.sli %>% mutate(Persistence = 1/E) %>% filter(Persistence > 30) %>% filter(Persistence < 100000)
cla.sli <- cla.sli %>% mutate(Persistence = 1/E) %>% filter(Persistence > 30) %>% filter(Persistence < 100000)
ord.sli <- ord.sli %>% mutate(Persistence = 1/E) %>% filter(Persistence > 30) %>% filter(Persistence < 100000)
fam.sli <- fam.sli %>% mutate(Persistence = 1/E) %>% filter(Persistence > 30) %>% filter(Persistence < 100000)
gen.sli <- gen.sli %>% mutate(Persistence = 1/E) %>% filter(Persistence > 30) %>% filter(Persistence < 100000)

# Table 1 -----------------------------------------------------------------

cor.test(phy.sli$C, phy.sli$Persistence, method = "s")
cor.test(cla.sli$C, cla.sli$Persistence, method = "s")
cor.test(ord.sli$C, ord.sli$Persistence, method = "s")
cor.test(fam.sli$C, fam.sli$Persistence, method = "s")
cor.test(gen.sli$C, gen.sli$Persistence, method = "s")

lm(log10(phy.sli$Persistence) ~ log10(phy.sli$C))
lm(log10(cla.sli$Persistence) ~ log10(cla.sli$C))
lm(log10(ord.sli$Persistence) ~ log10(ord.sli$C))
lm(log10(fam.sli$Persistence) ~ log10(fam.sli$C))
lm(log10(gen.sli$Persistence) ~ log10(gen.sli$C))

confint(lm(log10(phy.sli$Persistence) ~ log10(phy.sli$C)))
confint(lm(log10(cla.sli$Persistence) ~ log10(cla.sli$C)))
confint(lm(log10(ord.sli$Persistence) ~ log10(ord.sli$C)))
confint(lm(log10(fam.sli$Persistence) ~ log10(fam.sli$C)))
confint(lm(log10(gen.sli$Persistence) ~ log10(gen.sli$C)))


# Plot core-satellite -----------------------------------------------------

plot2 <-
  ggplot(fam.sli, aes(x = C, y = Persistence, fill = Core)) + 
  geom_point(aes(shape = Core, fill = Core), size = 2) + 
  scale_x_log10( ) + 
  scale_y_log10() +
  theme_bw() + ylab("Persistence (day)") + 
  xlab(expression(paste("Colonization (day"^"-1", ")"))) + 
  annotation_logticks() +
  scale_shape_manual(values = c(21, 22)) + 
  scale_fill_manual(values = c("#81392a", "#d58e80")) +
  scale_color_manual(values = c("#81392a", "#d58e80")) +
  theme(aspect.ratio = .618) +  
  geom_smooth(aes(color = Core), formula = y ~ x, method = "lm", se = F) +
  theme(legend.position = "none") +
  theme(panel.grid.minor = element_blank())+
  annotate("segment", x = 0.00006, xend = 0.00009, y = 200, yend = 200, 
           color = "grey20") +
  annotate("segment", x = 0.00006, xend = 0.00006, y = 200, yend = 300, 
           color = "grey20") +
  annotate("text", label = "-1", x = 0.000055, y = 197, color = "grey20") 

10^(  log10(200)   - log10(0.00006) + log10(0.00009) )

ggsave("simage.eps", width = 12, height = 8, units = "cm")

# Comparison slopes -------------------------------------------------------

# Genera 

out <- (lm(data = gen.sli %>% filter(Core == T), log10(Persistence) ~ log10(C)))
sm <- summary(out)
# Ajuste para las satélites
outs <- lm(data = gen.sli %>% filter(p > 30) %>% filter(Core == F), log10(Persistence) ~ log10(C))
sms <- summary(outs)
# Este sí sale significativo, tanto para la pendiente como para la ordenada
# Contraste de que las pendientes sean iguales
stde <- sms$coefficients[2,2] # Este es el error estandar de la pendiente
slope <- sms$coefficients[2,1] # Esta es la pendiente
# Intervalo de confianza al 95%
df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
# Contraste de hipótesis: beta = sm$coefficients[2,1]
tscore <- (slope - (sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore,df, lower.tail = F) # Multiplicamos por 2 por ser un contraste bilateral
pvalue
slope
slope0
slope1
tscore
stde <- sm$coefficients[2,2] # Este es el error estandar de la pendiente
slope <- sm$coefficients[2,1] # Esta es la pendiente
# Intervalo de confianza al 95%
df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope0
slope1

#Family

out <- (lm(data = fam.sli %>% filter(p > 30)  %>% filter(Core == T), log10(Persistence) ~ log10(C)))
sm <- summary(out)
# Ajuste para las satélites
outs <- lm(data = fam.sli %>% filter(p > 30) %>% filter(Core == F), log10(Persistence) ~ log10(C))
sms <- summary(outs)
# Este sí sale significativo, tanto para la pendiente como para la ordenada
# Contraste de que las pendientes sean iguales
stde <- sms$coefficients[2,2] # Este es el error estandar de la pendiente
slope <- sms$coefficients[2,1] # Esta es la pendiente
# Intervalo de confianza al 95%
df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
# Contraste de hipótesis: beta = sm$coefficients[2,1]
tscore <- (slope-(sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore,df, lower.tail = F) # Multiplicamos por 2 por ser un contraste bilateral
pvalue
slope
tscore
slope0
slope1
stde <- sm$coefficients[2,2] # Este es el error estandar de la pendiente
slope <- sm$coefficients[2,1] # Esta es la pendiente
# Intervalo de confianza al 95%
df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope0
slope1

