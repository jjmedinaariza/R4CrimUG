library(dplyr)
library(forcats)
##R in Windows have some problems with https addresses, that's why we need to do this first:
urlfile<-'https://raw.githubusercontent.com/jjmedinaariza/LAWS70821/master/BCS0708.csv'
#We create a data frame object reading the data from the remote .csv file
BCS0708<-read.csv(url(urlfile))
summary(BCS0708)

table(BCS0708$ethgrp2)
levels(BCS0708$ethgrp2)

BCS0708$ethgrp2_rc <- recode_factor(BCS0708$ethgrp2, '1' = "white", '2' = "asian or asian british", 
                                    '3' = "black or black british", '4' = "chinese or other", 
                                    '5' = "mixed")
table(BCS0708$ethgrp2_rc)
levels(BCS0708$ethgrp2_rc)

BCS0708$ethgrp2_rc <- fct_collapse(BCS0708$ethgrp2_rc, "Other" = c("chinese or other", "mixed"))
levels(BCS0708$ethgrp2_rc)
table(BCS0708$ethgrp2_rc)

table(BCS0708$tenure1)

BCS0708 <- within(BCS0708, age_quartile <- as.integer(cut(age, quantile(age, probs=0:4/4, na.rm=TRUE), include.lowest=TRUE)))
BCS0708$tenure1_rc <- fct_collapse(BCS0708$tenure1, 
                                   "Own" = c("buying it with the help of a mortgage or loan", "own it outright"),
                                   "Rent" = c("rent it", "pay part rent and part mortgage(shared ownership)"),
                                   "Rent free" = c("live here rent free (inc. rent free in relative/friend's)/sq"))
table(BCS0708$tenure1_rc)

summary(BCS0708$age)

BCS0708$age_rc <- cut(BCS0708$age, 
         breaks = c(16,25,35,50,65,Inf),
         labels = c("16 and over", "25-34", "35-49", "50-64", "65 and over"),
         right = FALSE)
class(BCS0708$age_rc)
levels(BCS0708$age_rc)

fitl_1 <- glm(bcsvictim ~ age_rc + tenure1_rc + sex + ethgrp2_rc + rural2, data=BCS0708, family = "binomial")
summary(fitl_1)
exp(coef(fitl_1))
library(arm)
fitl_1_s <- standardize(fitl_1)
display(fitl_1_s)
library(sjPlot)
sjp.glm(fitl_1)
library(effects)
plot(allEffects(fitl_1), ask=FALSE)

#Computing deviance and pseudo r squared
with(fitl_1, null.deviance - deviance)
with(fitl_1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
#Model chi squared is highly significant

#Likelihood ratio (pseudo r squared)
with(fitl_1, (null.deviance - deviance)/null.deviance)

