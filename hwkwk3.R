library(ggplot2)

ncovr <- read.csv("NAT.csv")

ggplot(data = ncovr, aes(x = HR90)) + 
  geom_histogram() + 
  facet_wrap(~SOUTH)

ggplot(data = ncovr,
       aes(x = as.factor(SOUTH), y = HR90)) + 
  geom_boxplot()

ggplot(ncovr, aes(x = HR90, fill = as.factor(SOUTH))) +
  geom_histogram(position = "identity", alpha = 0.4)

ggplot(ncovr, aes(x = log10(HR90+1), fill = as.factor(SOUTH))) +
  geom_histogram(position = "identity", alpha = 0.4)

ggplot(ncovr, aes(x = log10(HR90 + 1), fill = as.factor(SOUTH))) + 
  geom_density(alpha = .3)

ggplot(ncovr, aes(x = UE70, y = HR70)) +
  geom_point(alpha=.4) +
  geom_smooth(colour="red", size=1, se=FALSE)

ggplot(ncovr, aes(x = UE70, y = HR70, colour= as.factor(SOUTH))) +
  geom_point(alpha=.1) +
  geom_smooth(size=1.2, se=FALSE)