
# SOILS -------------------------------------------------------------------

# GENERA

# lm for core
out <- (lm(data = soil.gen %>% filter(p > 30) %>% filter(Core == T), log10(p) ~ log10(c)))
sm <- summary(out)
# lm for satellite
outs <- lm(data = soil.gen %>% filter(p > 30) %>% filter(Core == F), log10(p) ~ log10(c))
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

# FAMILIES
# Same structure as the genera one.

out <- (lm(data = soil.fam %>% filter(p > 30)  %>% filter(Core == T), log10(p) ~ log10(c)))
sm <- summary(out)

outs <- lm(data = soil.fam %>% filter(p > 30) %>% filter(Core == F), log10(p) ~ log10(c))
sms <- summary(outs)

stde <- sms$coefficients[2,2] 
slope <- sms$coefficients[2,1] 

df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 

tscore <- (slope-(sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore,df, lower.tail = F) 
pvalue
slope
tscore
slope0
slope1

stde <- sm$coefficients[2,2] 
slope <- sm$coefficients[2,1] 

df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope0
slope1



# LAKES -------------------------------------------------------------------

# GENERA

# lm for core
out <- (lm(data = lake.gen %>% filter((p * 30) > 8) %>% filter(Core == T), log10(p) ~ log10(c)))
sm <- summary(out)
# lm for satellite
outs <- lm(data = lake.gen %>% filter((p * 30) > 8) %>% filter(Core == F), log10(p) ~ log10(c))
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
pvalue <- 2*pt(tscore, df, lower.tail = T) # Multiplied by 2 as it is a bilateral constrast

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

# FAMILIES
# Same structure as the genera one.

out <- (lm(data = lake.fam %>% filter((p * 30) > 8)  %>% filter(Core == T), log10(p) ~ log10(c)))
sm <- summary(out)

outs <- lm(data = lake.fam %>% filter((p * 30) > 8) %>% filter(Core == F), log10(p) ~ log10(c))
sms <- summary(outs)

stde <- sms$coefficients[2,2] 
slope <- sms$coefficients[2,1] 

df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 

tscore <- (slope-(sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore, df, lower.tail = T) 
pvalue
slope
tscore
slope0
slope1

stde <- sm$coefficients[2,2] 
slope <- sm$coefficients[2,1] 

df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope0
slope1


# MONEGROS -------------------------------------------------------------------

# GENERA

# lm for core
out <- (lm(data = rat.gen %>% filter(p > 6) %>% filter(Core == T), 
           log10(p) ~ log10(c)))
sm <- summary(out)
# lm for satellite
outs <- lm(data = rat.gen %>% filter(p > 6) %>% filter(Core == F), 
           log10(p) ~ log10(c))
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
pvalue <- 2*pt(tscore, df, lower.tail = T) # Multiplied by 2 as it is a bilateral constrast

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

# FAMILIES
# Same structure as the genera one.

out <- (lm(data = rat.fam %>% filter(p > 6)  %>% filter(Core == T), 
           log10(p) ~ log10(c)))
sm <- summary(out)

outs <- lm(data = rat.fam %>% filter(p > 6) %>% filter(Core == F), 
           log10(p) ~ log10(c))
sms <- summary(outs)

stde <- sms$coefficients[2,2] 
slope <- sms$coefficients[2,1] 

df <- outs$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 

tscore <- (slope-(sm$coefficients[2,1]))/stde
pvalue <- 2*pt(tscore, df, lower.tail = T) 
pvalue
slope
tscore
slope0
slope1

stde <- sm$coefficients[2,2] 
slope <- sm$coefficients[2,1] 

df <- out$df.residual
slope0 <- slope-abs(qt(0.025,df))*stde
slope1 <- slope+abs(qt(0.025,df))*stde 
slope0
slope1
