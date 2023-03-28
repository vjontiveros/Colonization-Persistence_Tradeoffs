
# Soils -------------------------------------------------------------------

# Values were obtained looking at Ho et al. 2017 FEMS Microbiology Ecology table
# 1, compared to the taxonomic classification in soil.gen...

# Columns are Core/Satellite, rows oligo/copio/both life histories.

chisq.copio.oligo <- matrix(c(20, 34, 13, 13, 11, 21), nrow = 3, byrow = T)
# chisq.copio.oligo <- matrix(c(20, 34, 13, 13), nrow = 2, byrow = T)

row.names(chisq.copio.oligo) <- c("Oligo", "Copio", "both")

chisq.res <- chisq.test(chisq.copio.oligo)

round(chisq.res$residuals, 3)

chisq.res$parameter

# Monegros lakes ----------------------------------------------------------

# View(rat.gen[, c(1, 13)])

chisq.copio.oligo <- matrix(c(10, 33, 26, 19, 47, 51), nrow = 3, byrow = T)

row.names(chisq.copio.oligo) <- c("Oligo", "Copio", "both")

chisq.res <- chisq.test(chisq.copio.oligo)
chisq.res
round(chisq.res$residuals, 3)


# Pyrenees lakes ----------------------------------------------------------

# View(lake.gen[, c(1, 13)])

chisq.copio.oligo <- matrix(c(3, 5, 6, 3, 14, 4), nrow = 3, byrow = T)

row.names(chisq.copio.oligo) <- c("Oligo", "Copio", "both")

chisq.res <- chisq.test(chisq.copio.oligo)
chisq.res
round(chisq.res$residuals, 3)

library(pwr)
pwr.chisq.test(w = .3, 
               N = sum(c(3, 5, 6, 3, 14, 4)), 
               df = 2, 
               sig.level = 0.05,
               # power = .95
               )
