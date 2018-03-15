library(haven)
library(dplyr, quietly=TRUE, warn.conflicts=FALSE)
library(ggplot2,quietly=TRUE, warn.conflicts=FALSE)
data_url <- "https://dataverse.harvard.edu/api/access/datafile/3036350"
banbox <- read_dta(url(data_url))
aer2017<- filter(banbox, crimbox == 1, pre == 1)
aer2017 <- select(aer2017, crime, response)
aer2017$crime_f <- as_factor(aer2017$crime)
aer2017$response_f <- as.factor(aer2017$response)
t.test(response ~ crime_f, data = aer2017)
#The z-test comparing two proportions is equivalent to the chi-square 
#test of independence, and the prop.test( ) procedure formally 
#calculates the chi-square test. The p-value from the z-test for 
#two proportions is equal to the p-value from the chi-square test, 
#and the z-statistic is equal to the square root of the chi-square 
#statistic in this situation.
table(aer2017$response, aer2017$crime_f)
1140+179
1223+113
179/1319
113/1336
prop.test(c(179, 113), c(1319, 1336), correct=FALSE)

urlfile<-'https://raw.githubusercontent.com/jjmedinaariza/LAWS70821/master/BCS0708.csv'
BCS0708<-read.csv(url(urlfile))
ggplot(BCS0708, aes(educat3, tcviolent, fill=educat3)) + 
  geom_boxplot(outlier.size=1) + 
  guides(fill=FALSE) 
library(psych)
describeBy(BCS0708$tcviolent, BCS0708$educat3)
library(gplots)
plotmeans(tcviolent ~ educat3, data = BCS0708)
fearmodel.1<- aov(tcviolent ~ educat3, data=BCS0708)
summary(fearmodel.1)
library(car)
leveneTest(BCS0708$tcviolent, BCS0708$educat3, center = median)
oneway.test(tcviolent ~ educat3, data=BCS0708)
qqPlot(fearmodel.1)
library(WRS2)
t1waybt(tcviolent ~ educat3, data = BCS0708, tr = .05, nboot = 599)
pairwise.t.test(BCS0708$tcviolent, BCS0708$educat3, p.adjust.method="bonferroni") 
summary.lm(fearmodel.1) 
