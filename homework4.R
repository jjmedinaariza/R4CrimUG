#Get needed libraries
library(haven)
library(dplyr)
library(ggplot2)
library(mosaic)
library(haven)

#Get data
data_url <- "https://dataverse.harvard.edu/api/access/datafile/3036350"
banbox <- read_dta(url(data_url))

#Subset the data to relevant variables
homework<- select(banbox, black, white, response, pre)

#Look at this https://www.r-bloggers.com/proportions-with-mean/

#Obtaining confidence intervals with resampling
#First construct the resampling distributions
#Here I am using the kind of subseting used in base R, which is different from dplyr
blackpre <- do(10000) * mean(resample(homework$response[homework$pre=="1" & homework$black==1]))
whitepre <- do(10000) * mean(resample(homework$response[homework$pre=="1" & homework$white==1]))
blackpost <- do(10000) * mean(resample(homework$response[homework$pre=="0" & homework$black==1]))
whitepost <- do(10000) * mean(resample(homework$response[homework$pre=="0" & homework$white==1]))

qdata(~mean, p = c(.025, .975), blackpre)
qdata(~mean, p = c(.025, .975), whitepre)
qdata(~mean, p = c(.025, .975), blackpost)
qdata(~mean, p = c(.025, .975), whitepost)

#Plot the resampling distributions
ggplot(blackpre, aes(x=mean)) + 
  geom_density() +
  geom_density(data=whitepre, colour="orange") +
  geom_density(data=blackpost, colour="purple") +
  geom_density(data=whitepost, colour="red")

#Extract the upper and lower level of the confidence intervals
blackpre_ci <- qdata(~mean, p = c(.025, .975), blackpre)
whitepre_ci <- qdata(~mean, p = c(.025, .975), whitepre)
blackpost_ci <- qdata(~mean, p = c(.025, .975), blackpost)
whitepost_ci <- qdata(~mean, p = c(.025, .975), whitepost)

#Create a dataframe with the info needed for plotting error bars

df <- data.frame(
  #First you create a vector with the point estimate for each of the four groups
prop <- c(mean(blackpre$mean), mean(blackpost$mean), mean(whitepre$mean), mean(whitepost$mean)),
#then you can add the lower bound of the confidence interval extracting this information from the
#objects we creared above
l_ci <-c(blackpre_ci[1,1], blackpost_ci[1,1], whitepre_ci[1,1], whitepost_ci[1,1]),
#same but now the upper bound
u_ci <-c(blackpre_ci[2,1], blackpost_ci[2,1], whitepre_ci[2,1], whitepost_ci[2,1]),
#then create two factors to indicate to R whether the row is Black or White and Pre or Post
black <-factor(c("Black", "Black", "White", "White")),
pre <-factor(c("Pre", "Post", "Pre", "Post"))
)

#Then we can use ggplot to plot these dataframe
ggplot(df, aes(prop, pre, colour = black)) +
  geom_point() +
  geom_errorbarh(aes(xmax = u_ci, xmin = l_ci))

#Approximate test for proportions
#Since we have loaded the mosaic package this is the prop.test function of this package
prop.test(homework$response[homework$pre=="1" & homework$black==1])
prop.test(homework$response[homework$pre=="1" & homework$white==1])
prop.test(homework$response[homework$pre=="0" & homework$black==1])
prop.test(homework$response[homework$pre=="0" & homework$white==1])
#Exact test for proportions
#Since we have loaded the mosaic package this is the binom.test function of this package
binom.test(homework$response[homework$pre=="1" & homework$black==1], ci.method = "Agresti-Coull")
binom.test(homework$response[homework$pre=="1" & homework$white==1], ci.method = "Agresti-Coull")
binom.test(homework$response[homework$pre=="0" & homework$black==1], ci.method = "Agresti-Coull")
binom.test(homework$response[homework$pre=="0" & homework$white==1], ci.method = "Agresti-Coull")

#you can store the results of running this functions
proptest_blackpre <- prop.test(homework$response[homework$pre=="1" & homework$black==1])
proptest_blackpre
#and can invoke the elements that go inside the objects with the stored tests
#for the point estimate
proptest_blackpre$estimate
#for the confidence interval
proptest_blackpre$conf.int
#for one of the elements of the confidence interval
proptest_blackpre$conf.int[1]

#earlier in the script I used base R to subset but you can also use dplyr as in the session today
blackpre_df <- filter(homework, black==1, pre==1)
#Whatever method you use to subset you get the same results
mean(blackpre_df$response)
mean(homework$response[homework$pre=="1" & homework$black==1])
propforblackspre <- prop.test(blackpre_df$response)
propforblackspre$estimate
