###Script used to modify linear regression for manuscript

##CREATING CLINICAL/NON-CLINICAL TOTALS
TEST <- basedata %>% 
  select(3, 5, 20, 23:33,36:51,54:65, 68:87, 95:99, 106:109) %>% 
  select(-`2: Email address:`, `2: Email address:`,
         -`25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`,
         `25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`,
         -`6: Please select your NHS Health Board and HSCP: Health Board`,
         `6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(`TOTAL EMPLOYED` = rowSums(.[c(3,4,5,6,7,13,14,15,16,17,18,19,20,29,30,31,32,33,34,41,42,43,44,45,46,47,48,49,50,61,62,63,64,65)]),
         `TOTAL VACANCIES` = rowSums(.[c(8,9,10,11,12,21,22,23,24,25,26,27,28,35,36,37,38,39,40,51,52,53,54,55,56,57,58,59,60)]),
         `TOTAL CAPACITY` = `TOTAL EMPLOYED` + `TOTAL VACANCIES`,
         vacancy_ratio = round(`TOTAL VACANCIES`/`TOTAL CAPACITY`*100, 1),
         # volunteers = case_when(`25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`>0~1,
         #                        T~0),
         volunteers = `25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`,
         CLINICAL = rowSums(.[c(3,4,5,6,7,13,14,15,16,17,18,19,20,29,30,31,32,33,34)]),
         NONCLINICAL = rowSums(.[c(41,42,43,44,45,46,47,48,49,50,61,62,63,64,65)])) %>% 
  select(1,2,66:78, everything()) 

##Matching with previous dataset
linreg_mod <- anti_join(TEST, join_not, by=c("4: Service/Provider Name:", 
                                             "8: Please select the option which best describes your service category:",
                                             "2: Email address:")) %>%
  mutate(caseload = `27: Of those workers with a caseload, what is the average number of service users being case-managed by each frontline WTE worker:`,
         sickdays = (log(`24: How many days have been lost to sickness amongst paid employees over the past 6 months (1 May 2021 to 1 November 2021):`+1)),
         sick_days= `24: How many days have been lost to sickness amongst paid employees over the past 6 months (1 May 2021 to 1 November 2021):`,
         CLINICALFLAG = case_when(CLINICAL >0 & NONCLINICAL ==0 ~1, 
                                  T~0),
         NONCLINICAL_FLAG=case_when(NONCLINICAL>0 & CLINICAL ==0~1,
                                    T~0),
         BOTH = case_when(NONCLINICAL>0 & CLINICAL >0~1,
                          T~0))


##Regression analyses

#Original from SG publication
testregression <- lm(sickdays~caseload+vacancy_ratio, data = linreg_mod)
#With new variables added
testregression2 <- lm(sickdays~caseload+vacancy_ratio+volunteers+CLINICAL+NONCLINICAL, data = linreg_mod)


summary(testregression)
summary(testregression2)
# confint(testregression)
# confint(testregression2)

par(mfrow=c(2,2))
plot(testregression2)

cor(linreg_mod[c('caseload', 'vacancy_ratio', 'volunteers', 'CLINICAL', 'NONCLINICAL')])
#visualising correlations
pairs(linreg_mod[c('caseload', 'vacancy_ratio', 'volunteers', 'CLINICAL', 'NONCLINICAL')])

##Analyses of variance
anova(testregression,testregression2) ##the type of people employed in an organisation is significantly associated with sickness above and beyond what the number of people employed there 
