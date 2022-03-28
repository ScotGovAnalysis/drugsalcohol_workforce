##ANNEXES

###########################################################################################
#Annex 1: survey responses by health board

##Step 1: create dataset of initial survey recipients
surveyrecipients <- readxl::read_xlsx("C:/Users/u449921/Documents/Workforce/Survey work/Survey_recipients.xlsx") %>%
  mutate(`Health Board` = str_replace(`Health Board`, "NHS ", ""),
         cat = "Number of services on DAISy") %>%
  group_by(`Health Board`, cat) %>%
  summarise(total = n())

##Step 2: create dataset of survey respondents
Fig1 <- basedata %>%
  group_by(`6: Please select your NHS Health Board and HSCP: Health Board`) %>%
  summarise(total = n()) %>%
  filter(!is.na(`6: Please select your NHS Health Board and HSCP: Health Board`),
         `6: Please select your NHS Health Board and HSCP: Health Board` != "Borders") %>%
  mutate(`Health Board` = `6: Please select your NHS Health Board and HSCP: Health Board`,
         cat = "Number of survey respondents") %>%
  select(`Health Board`, cat, total,
         -`6: Please select your NHS Health Board and HSCP: Health Board`)

##Bind and vis
bind_rows(surveyrecipients, Fig1) %>%
  pivot_wider(names_from = cat, values_from = total) %>%
  replace(is.na(.), 0) %>%
  mutate(prop = `Number of survey respondents`/`Number of services on DAISy`) %>%
  pivot_longer(cols = `Number of services on DAISy`:`Number of survey respondents`, names_to = "cat", values_to = "total") %>%
  ggplot(aes(x=reorder(`Health Board`, prop), y=total, fill = cat)) +
  geom_col(position = 'dodge') +
  coord_flip() +
  theme+
  theme(legend.title = element_blank())+
  labs(x="NHS Health Board",
       caption = "Note: Arranged in order of proportional response rate by health board",
       title = "",
       y="Total") +
  geom_text(aes(label = total), position = position_dodge(1), hjust=-.1) +
  scale_fill_manual(values = c("#0065bd", "#912688"))
  
  ###########################################################################################
  
  #Annex 2: Employment/vacancies by professions
  
  ##SEE FINAL VIS at Annex2.png
  
  ##Required creating function to tidy/format data for each of the professions
  staffing_plotting <- function(df, plottitle){
  ggplot(data=df, aes(x=`Job Type`, y=Total, fill = Employment)) +
    geom_col(position = 'dodge') +
    theme_bw() +
    theme(legend.title = element_blank(),
          legend.position = "bottom")+
    coord_flip() +
    geom_text(aes(label = scales::comma(Total, accuracy = .1)), 
              position = position_dodge(1), 
              hjust = -.01) +
    labs(x="Class",
         y="Whole-time equivalents",
         title = plottitle)
}

##Also requires gridExtra package:
gridExtra::grid.arrange((staffing_wrangle(basedata, "Medical") %>% 
                           staffing_plotting("Medical")),
                        (staffing_wrangle(basedata, "Psychology") %>% 
                           staffing_plotting("Psychology")),
                        (staffing_wrangle(basedata, "Nursing") %>% 
                           staffing_plotting("Nursing")),
                        (staffing_wrangle(basedata, "Non-clinical") %>% 
                           staffing_plotting("Non-clinical")),
                        top = grid::textGrob("Employment and vacancy rates of frontline staff by category (WTEs)", gp=grid::gpar(fontsize=20)))
                        
 ###########################################################################################
  
 Annex 3: Table of employment/vacancies by professions
 
 ##A horribly unwieldy table that would not fit in the body of the document
 ##Uses function found in Wafflechart.R
 
 rbind((staffing_wrangle(basedata, "Nursing") %>% 
  pivot_wider(names_from = Employment, values_from = Total) %>% 
  mutate(total = Vacancy+Employed,
         vacancyrate = round(Vacancy/total*100,1))),

(staffing_wrangle(basedata, "Medical") %>% 
  pivot_wider(names_from = Employment, values_from = Total) %>% 
  mutate(total = Vacancy+Employed,
         vacancyrate = round(Vacancy/total*100,1))),

(staffing_wrangle(basedata, "Psychology") %>% 
  pivot_wider(names_from = Employment, values_from = Total) %>% 
  mutate(total = Vacancy+Employed,
         vacancyrate = round(Vacancy/total*100,1))),

(staffing_wrangle(basedata, "Non-clinical") %>% 
  pivot_wider(names_from = Employment, values_from = Total) %>% 
  mutate(total = Vacancy+Employed,
         vacancyrate = round(Vacancy/total*100,1)))) %>% 
  mutate(`Reported employment` = Employed,
         `Reported vacancies` = Vacancy,
         `Reported total capacity` = total,
         `Vacancy rate` = paste0(vacancyrate,"%")) %>% 
  arrange(desc(vacancyrate)) %>% 
  filter(!is.na(vacancyrate)) %>%
    select(-Employed, -Vacancy, -total, -vacancyrate,) %>% 
  flextable() %>% 
  width(j=1, 1.1) %>% 
  width(j=c(2,3,4,5,6), 1.2) %>% 
  bold(part = 'header') %>% 
  bold(i=c(7,10, 16, 21)) %>% 
  bold(j=6) %>% 
  vline(j=5, border = officer::fp_border()) %>% 
  vline(j=2, border = officer::fp_border()) %>% 
  fontsize(i=1, size =12, part = 'header') %>% 
  border_outer(border = officer::fp_border()) %>% 
  align(j=1)
  
###########################################################################################
  
#Annex 4: regression analysis

#see regression.R

###########################################################################################
  
Annex 5: Workforce estimates

##see estimates subfolder for more information about how these figures were derived

Methodology <- c("Mean by health board", "Mean of means",
          "Mean by health board (Winsorization)", "Mean of means (Winsorization)",
          "Bootstrap")

`Employment(WTEs)` <- c(3768, 3499, 3505, 3288, 3658)

data.frame(Methodology, `Employment(WTEs)`) %>% 
  flextable() %>% 
  width(j=1, 3) %>%
  width(j=2, 2) %>% 
   add_header_row(colwidths = 2,
                 values = "Table 5.1: Estimated employment across Scotland's alcohol and drug services by method") %>% 
  fontsize(i=1, size =12, part = 'header') %>% 
  bold(part='header') %>% 
  hline(i=c(2,4), border = officer::fp_border()) %>% 
  vline(j=1, border = officer::fp_border()) %>% 
  border_outer(border = officer::fp_border()) %>% 
  colformat_num(j=2, big.mark=",")
