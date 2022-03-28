library(tidyverse)

###################################################################

#Reading in base dataset (same as in the surveywork filepath)
basedata <- readxl::read_xlsx("Surveyresults_final.xlsx")

#Modifying base dataset as appropriate (same as in surveywork filepath
##Noting the silly adding scripts, due to outdated Tidyverse currently available on Scots
Staffing_byorg <- basedata %>% 
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
         volunteers = case_when(`25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`>0~1,
                                T~0)) %>% 
  select(1,2,66:76, everything()) 
  
###################################################################
  
#Method 1a: Mean by geography, and mean of means
##REFER TO THE PUBLISHED REPORT FOR MORE INFO

View(Staffing_byorg %>% 
       filter(`6: Please select your NHS Health Board and HSCP: Health Board` != "Borders") %>% 
       group_by(`6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
       summarise(n_HB = n(),
                 sum_HB = sum(`TOTAL EMPLOYED`),
                 meanby_HB = mean(`TOTAL EMPLOYED`)) %>% 
       mutate(meanofmeans=mean(meanby_HB),
              sdofdistofsamplemeans=(sd(meanby_HB)/(sqrt(12))),
              `Health Board` = `6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
       select(`Health Board`, everything(), -`6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
       # ggplot(aes(x=meanby_HB))+
       # geom_histogram(bins=5)
       left_join(surveyrecipients, by= "Health Board") %>%
       select(-cat) %>% 
       mutate(diff = total-n_HB,
              missing_HB = meanby_HB*diff,
              missing_HB2 = meanofmeans*diff,
              totalemp_maybe = sum_HB+missing_HB,
              totalemp_maybe2=sum_HB+missing_HB2,
              sum_meanbyHB=sum(totalemp_maybe),
              sum_meanofmeans=sum(totalemp_maybe2)))
              
###################################################################
##Method 2b: Mean by geography and mean of means WINSORISED
##note the Winsorize command from the DescTools library

View(Staffing_byorg %>% 
       mutate(`TOTAL EMPLOYED` = DescTools::Winsorize(Staffing_byorg$`TOTAL EMPLOYED`, probs = c(.05,.95))) %>% 
       filter(`6: Please select your NHS Health Board and HSCP: Health Board` != "Borders") %>% 
       group_by(`6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
       summarise(n_HB = n(),
                 sum_HB = sum(`TOTAL EMPLOYED`),
                 meanby_HB = mean(`TOTAL EMPLOYED`)) %>% 
       mutate(meanofmeans=mean(meanby_HB),
              sdofdistofsamplemeans=(sd(meanby_HB)/(sqrt(12))),
              `Health Board` = `6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
       select(`Health Board`, everything(), -`6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
       # ggplot(aes(x=meanby_HB))+
       # geom_histogram(bins=5)
       left_join(surveyrecipients, by= "Health Board") %>%
       select(-cat) %>% 
       mutate(diff = total-n_HB,
              missing_HB = meanby_HB*diff,
              missing_HB2 = meanofmeans*diff,
              totalemp_maybe = sum_HB+missing_HB,
              totalemp_maybe2=sum_HB+missing_HB2,
              sum_meanbyHB=sum(totalemp_maybe),
              sum_meanofmeans=sum(totalemp_maybe2)))
