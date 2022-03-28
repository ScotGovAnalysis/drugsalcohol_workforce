library(tidyverse)
library(flextable)

#########################################################
#Reading in data
##THIS IS THE CORE DATASET

basedata <- readxl::read_xlsx("C:/Users/u449921/Documents/Workforce/Survey work/Surveyresults_final.xlsx")

#########################################################
#Table 1: basic stats about survey returns

##Manually creating table contents
Measure <- c("Number of survey recipients", 
             "Number of valid survey responses", 
             "Response rate",
             "Number of health boards reporting",
             "Total number of WTE employees reported",
             "Median number of WTE employees per service (IQR)")

Totals <- c("206",
            "88",
            "43%",
            "12 of 14",
            "1,572.8",
            "11.0 (5.0-22.5)")
            
##creating table
data.frame(Measure, Totals) %>% 
  flextable() %>% 
  width(j=1, 4.5) %>%
  width(j=2,1.5) %>% 
  add_header_row(colwidths = 2,
                 values = "Table 1: Descriptive statistics of respondents to survey of drug and alcohol services") %>% 
  bold(part = 'header') %>% 
  fontsize(i=1, size =12, part = 'header') %>% 
  border_outer(border = officer::fp_border()) %>%
  align(j=2, align = 'right', part = 'all') %>% 
  align(j=1, align = 'left', part = 'all') %>% 
  vline(j=1, border = officer::fp_border()) %>% 
  hline(i=4, border = officer::fp_border())
  
#########################################################

#Figure 1: SEE SPATIAL .R FILE

#########################################################

#Figure 2: responses by organisation type
basedata %>% 
  filter(`6: Please select your NHS Health Board and HSCP: Health Board` != "Borders") %>% 
  group_by(`8: Please select the option which best describes your service category:`) %>% 
  summarise(total = n()) %>% 
  mutate(overall = sum(total),
         prop = round(total/overall*100,1),
         organisation="") %>% 
  ggplot(aes(x=organisation, y=prop, fill = reorder(`8: Please select the option which best describes your service category:`, prop))) +
  geom_col(position = 'stack') +
  coord_flip()+
  theme +
   theme(legend.position = 'bottom',
        legend.title = element_blank())+
  labs(x="",
       y="%",
       title = "Figure 2: Survey respondents by organisation type",
       subtitle = "n=88") +
  scale_fill_brewer(palette = 'Blues') +
  guides(fill=guide_legend(reverse=T,ncol=2))+
  geom_text(aes(label = paste0(prop, '%')), position = position_stack(.5))
  
#########################################################
  
#Table 2: Report vacancy rates by board
  
#Step 1: Formatting new dataset
##Required a very manual (and very tedious) process of summing, because we don't have the latest version of dplyr so can't add across columns
Staffing_byorg <- basedata %>% 
  select(3, 5,23:33,36:51,54:65, 68:87, 95:99, 106:109) %>% 
  select(-`2: Email address:`, `2: Email address:`,
         -`25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`,
         `25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  mutate(`TOTAL EMPLOYED` = rowSums(.[c(3,4,5,6,7,13,14,15,16,17,18,19,20,29,30,31,32,33,34,41,42,43,44,45,46,47,48,49,50,61,62,63,64,65)]),
         `TOTAL VACANCIES` = rowSums(.[c(8,9,10,11,12,21,22,23,24,25,26,27,28,35,36,37,38,39,40,51,52,53,54,55,56,57,58,59,60)]),
         `TOTAL CAPACITY` = `TOTAL EMPLOYED` + `TOTAL VACANCIES`,
         vacancy_ratio = round(`TOTAL VACANCIES`/`TOTAL CAPACITY`*100, 1),
         volunteers = case_when(`25: How many people were working in your service in a strictly volunteer/unpaid capacity as at 1 November 2021:`>0~1,
                                T~0)) %>% 
  select(1,2,66:75, everything()) 
  
Step 2: Merging on to the base dataset
basedata %>% 
  select(`4: Service/Provider Name:`, 
         `6: Please select your NHS Health Board and HSCP: Health Board`, 
         `8: Please select the option which best describes your service category:`, 
         `26: What is your service's overall caseload as at 1 November 2021`) %>%
    mutate(`26: What is your service's overall caseload as at 1 November 2021`= ifelse(is.na(`26: What is your service's overall caseload as at 1 November 2021`),
                                                                                       0,
                                                                                       `26: What is your service's overall caseload as at 1 November 2021`)) %>% 
  right_join(Staffing_byorg, by=c("4: Service/Provider Name:", 
                                  "6: Please select your NHS Health Board and HSCP: Health Board",
                                 "8: Please select the option which best describes your service category:", 
                                 "26: What is your service's overall caseload as at 1 November 2021")) %>% 
  group_by(`6: Please select your NHS Health Board and HSCP: Health Board`) %>% 
  summarise(summed_emp = sum(`TOTAL EMPLOYED`),
            summed_vac = sum(`TOTAL VACANCIES`)) %>% 
  mutate(total = summed_emp +summed_vac,
         prop = round(summed_vac/total*100,1)) %>% 
  arrange(desc(prop)) %>% 
  filter(!is.na(prop)) %>% 
  mutate(`NHS Board` = `6: Please select your NHS Health Board and HSCP: Health Board`,
         `Reported employment` = scales::comma(summed_emp),
         `Reported vacancies` = scales::comma(summed_vac),
         `Reported total capacity` = scales::comma(total),
         `Vacancy rate` = paste0(prop,"%")) %>% 
  select(-`6: Please select your NHS Health Board and HSCP: Health Board`, -summed_emp, -summed_vac, -total,-prop) %>% 
  add_row(`NHS Board` = "Total",
    `Reported employment`="1,572.8",
          `Reported vacancies` = "152.2",
          `Reported total capacity` = "1,725.0",
          `Vacancy rate` = "8.8%") %>% 
  flextable() %>% 
  width(j=1, 2.5) %>% 
  width(j=c(2,3,4,5), 1.2) %>% 
  add_header_row(colwidths = 5,
                 values = "Table 2: Reported  Vacancy rates in Drug and Alcohol Services by NHS Board (as of 1 November 2021)") %>% 
  bold(part = 'header') %>% 
  bold(j=5) %>% 
  bold(i=13) %>%
  vline(j=4, border = officer::fp_border()) %>% 
  vline(j=1, border = officer::fp_border()) %>% 
  hline(i=12, border = officer::fp_border()) %>%
  fontsize(i=1, size =12, part = 'header') %>% 
  border_outer(border = officer::fp_border()) %>% 
  align(j=1, part = 'all') %>% 
  add_footer_row(top = F, 
                 colwidths = 5,
                 values = "n=88")
                 
#########################################################

#Figure 3: Employment and vacancy totals by service type

Staffing_byorg %>% 
  select(1:9) %>% 
  pivot_longer(`TOTAL EMPLOYED`:`TOTAL VACANCIES`, names_to="Category", values_to = "Total") %>% 
  group_by(`8: Please select the option which best describes your service category:`, Category) %>% 
  summarise(sums = sum(Total)) %>% 
   mutate(`Organisation Type` = str_wrap(`8: Please select the option which best describes your service category:`,12),
          Category = case_when(Category == "TOTAL EMPLOYED"~"Reported Employment",
                               T~"Reported Vacancies")) %>% 
  ggplot(aes(x=`Organisation Type`, y=sums, fill=Category)) +
  geom_col(position = "dodge")+
  theme +
  coord_flip() +
  geom_text(aes(label =  scales::comma(sums, accuracy=.1)), position = position_dodge(1), hjust = -.01) +
  labs(x="Organisation type",
       y="WTEs",
       title = "Figure 3: Employment and vacancy totals across all roles \nby service category (as of 1 November 2021)") +
  scale_fill_manual(values = c("#0065bd", "#912688")) +
  scale_y_continuous(labels=scales::comma,
                     limits=c(0,620, by=100),
                     breaks = seq(0,620, by= 100))

#########################################################

#Table 3: Rates by service type

Staffing_byorg %>% 
  group_by(`8: Please select the option which best describes your service category:`) %>% 
  summarise(summed_emp = sum(`TOTAL EMPLOYED`),
            summed_vac = sum(`TOTAL VACANCIES`)) %>% 
  mutate(total = summed_emp +summed_vac,
         prop = round(summed_vac/total*100,1),
         `Organisation Type` = `8: Please select the option which best describes your service category:`,
         `Reported employment` = scales::comma(summed_emp, accuracy = .1),
         `Reported vacancies` = scales::comma(summed_vac, accuracy = .1),
         `Reported total capacity` = scales::comma(total, accuracy = .1),
         `Vacancy rate` = paste0(prop,"%")) %>% 
  arrange(desc(prop)) %>% 
  select(-`8: Please select the option which best describes your service category:`, -summed_emp, -summed_vac, -total,-prop) %>%
  add_row(`Organisation Type` = "Total", 
          `Reported employment` = "1,572.8",
          `Reported vacancies` = "152.2",
          `Reported total capacity` = "1,725.0",
          `Vacancy rate` = "8.8%") %>% 
  flextable() %>% 
  width(j=1, 2.6) %>% 
  width(j=c(2,3,4,5), 1.2) %>% 
  add_header_row(colwidths = 5,
                 values = "Table 3: Vacancy rates by organisation type (as of 1 November 2021)") %>% 
  bold(part = 'header') %>% 
  bold(j=5) %>% 
  vline(j=4, border = officer::fp_border()) %>% 
  vline(j=1, border = officer::fp_border()) %>% 
  fontsize(i=1, size =12, part = 'header') %>% 
  border_outer(border = officer::fp_border()) %>% 
  hline(i=6, border = officer::fp_border()) %>% 
  bold(i=7) %>% 
  align(j=1, part='all')

#########################################################

#Figure 4: See Wafflechart.R
