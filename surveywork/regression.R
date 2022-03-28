library(tidyverse)

#Isolating N/A sick days
join_not <- basedata %>% 
  mutate(sick_days = `24: How many days have been lost to sickness amongst paid employees over the past 6 months (1 May 2021 to 1 November 2021):`) %>%
  filter(is.na(sick_days)) %>% 
  select(`4: Service/Provider Name:`,
         `8: Please select the option which best describes your service category:`,
         `2: Email address:`) %>% 
  mutate(flag=1)

#Joining datasets
linreg <- anti_join(Staffing_byorg, join_not, by=c("4: Service/Provider Name:", 
                                              "8: Please select the option which best describes your service category:",
                                              "2: Email address:")) %>%
  mutate(caseload = `27: Of those workers with a caseload, what is the average number of service users being case-managed by each frontline WTE worker:`,
         sickdays = (log(`24: How many days have been lost to sickness amongst paid employees over the past 6 months (1 May 2021 to 1 November 2021):`+1)),
         sick_days= `24: How many days have been lost to sickness amongst paid employees over the past 6 months (1 May 2021 to 1 November 2021):`)

#plot
linreg %>%
  ggplot(aes(x=caseload,
             y=sick_days)) +
  geom_point() +
  geom_smooth(method = lm, se=F)+
  theme_bw() +
  labs(y="Days lost to sickness",
       title = "Correlation between caseload per WTE and sick days")

##########################################
#Creating regression model
boo <- lm(sickdays~caseload+vacancy_ratio+volunteers, data = linreg)

##Summary+comfidence intervals
summary(boo)
confint(boo)

#diagnostics

#correlations
##Doesn't really work for volunteers as it is not continuous
cor(linreg[c('caseload', 'vacancy_ratio', 'volunteers')])

##Visualising correlations
pairs(linreg[c('caseload', 'vacancy_ratio', 'volunteers')])

##Plotting various info re residuals and such
par(mfrow=c(2,2))
plot(boo)

#generating standardised regression coefficients
pooledSD1 <- sd(linreg$caseload)/sd(linreg$sickdays)
.048449*pooledSD1

pooledSD2 <- sd(linreg$vacancy_ratio)/sd(linreg$sickdays)
-0.000613*pooledSD2

pooledSD3 <- sd(linreg$volunteers)/sd(linreg$sickdays)
0.713437 *pooledSD3

