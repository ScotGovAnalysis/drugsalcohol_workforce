library(tidyverse)

#uses bootstrapping to derive estimates and measures of central tendency for workforce size

#getting mean
samplemean<- with(Staffing_byorg, mean(`TOTAL EMPLOYED`))

#creating a matrix with half a million rows and columns equal to size of dataset
B <- 500000
n <- nrow(Staffing_byorg)

#generating boostrapped samples
boot.samples <- matrix(sample(Staffing_byorg$`TOTAL EMPLOYED`, size=B*n, replace=T), B,n)

#generating mean for each of the 500,000 rows
boot.stats <- apply(boot.samples,1,mean)

##visualising the distribution (Gauss would be so proud!)
ggplot(data.frame(mean_emp=boot.stats),aes(x=mean_emp))+
  geom_histogram(binwidth=0.25,aes(y=..density..)) +
  geom_density(color="red") +
  geom_vline(aes(xintercept=mean(mean_emp))) +
  theme_bw() +
  labs(x="Total number of reported employees (WTEs)")

#Generating standard error and mean of the bootstrapped means
total.se=sd(boot.stats)
mean(boot.stats)

#creating 95% confidence interval 
CI <- mean(boot.stats)+c(-1,1)*1.96*total.se

##Generating total for 'missing' population, using bootstrapped means
(mean(boot.stats))*118+ sum(Staffing_byorg$`TOTAL EMPLOYED`)
13.5*206
21.8*206
