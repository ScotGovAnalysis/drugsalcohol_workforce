library(tidyverse)
library(flextable)
library(officer)
library(lubridate)
library(ggalluvial)

#############
#############
###ALL VIS###
#############
#############

#creating theme

theme <- 
  theme_bw()+
  theme(axis.text.x = element_text(angle = 45, size=12, vjust=1, hjust=1),
        axis.text.y = element_text(size=12),
        axis.title = element_text(size = 14),
        plot.title = element_text(size=20),
        strip.text.x = element_text(size =13))
 ###########################################################################

#READING DATA: SFC 
#Note the shortening of titles

college <- readxl::read_excel("//scotland.gov.uk/dc1/fs3_home/U449921/workforce/data/Alcohol and Drugs College PIs 1314 to 1920.xlsx") %>% 
  mutate(Superclass = case_when(Superclass == "Counselling / Advice Work / Crisis Support" ~ "Counselling/Advice Work/Crisis Support",
                                Superclass == "Health Care Management/Health Studies" ~ "Healthcare Management/Health Studies",
                                Superclass == "Semi-medical/Physical/Psycho/Therapies" ~ "Semimedical/Physical/Psycho/Therapies",
                                Superclass == "Paramedical Services/Supplementary Medicine" ~ "Paramedical/Supplementary Medicine",
                                T~Superclass))
##########
#Figure 1: COLLEGES

college %>%  
  # filter(!Superclass %in% c("Dental Services", "Ophthalmic Services", "Caring Skills", "Health and Safety")) %>%
  mutate(Superclass = case_when(Superclass %in% c("Counselling / Advice Work / Crisis Support",
                                                  "Caring Skills",
                                                  "Social / Family / Community Work",
                                                  "Child Care Services",
                                                  "Crisis Support/Counselling",
                                                  "Family/Community Work",
                                                  "Social care/Social Work Skills") ~ paste0(Superclass, "*"),
                                                  T ~Superclass)) %>% 
  filter(`PI Outcome` == "Completed success") %>% 
  ggplot(aes(x=`Academic Year`, y=Enrolments)) +
  geom_col(fill = "#0065bd")+
  facet_wrap(~Superclass, ncol=3)+
  geom_text(aes(label = ifelse(Enrolments<5000, 
                               ifelse(`Academic Year` %in% c("2013-14", "2019-20"), scales::comma(Enrolments, accuracy = 1), ""), 
                               ""), 
                vjust = -.1), 
            size = 4.5) +
  labs(caption = "*These courses were consolidated/amalgamated in 2017-18, so are not directly comparable over the time series",
       title = "Figure 1: Students successfully completing a college course in\nhealth care, social care or medical subjects, 2013-14 to 2019-20",
       y="Completions") +
  scale_y_continuous(labels = scales::comma) +
  theme +
  theme(axis.text.x = element_text(angle = 45, size=12, vjust=1, hjust=1))

#################
#Figure 2: UNIVERSITY
#NOTE: Script is piped through to read in data and create vis   

readxl::read_excel("//scotland.gov.uk/dc1/fs3_home/U449921/workforce/data/Alcohol and Drugs University Qualifiers 1314 to 1819 by level.xlsx") %>% 
  filter(`Subject Name` %in% c("Counselling", "Health and Welfare", "Pharmacology, Toxicology and Pharmacy")) %>% 
  mutate(`Level of Qualification` = case_when(`Level of Qualification` %in% c("First Degree", "Other undergraduate") ~ "Undergraduate",
                                              T~`Level of Qualification`)) %>% 
  group_by(`Academic Year`,`Subject Name`, `Level of Qualification`) %>% 
  summarise(total = sum(Qualifiers)) %>% 
  ggplot(aes(x=`Academic Year`, y=total, group = `Subject Name`, color = `Subject Name`)) +
  geom_point(size =4)+
  geom_line()+
  facet_wrap(~`Level of Qualification`) +
  theme +
  theme(axis.text.x = element_text(angle = 90, size=12, hjust =-1)) +
  labs(title = "Figure 2: Students successfully completing a university degree in\ndrugs and alcohol studies subjects, 2013-14 to 2018-19",
       y="Graduates (Full-Person Equivalents)") +
  scale_color_manual(values = c("#9ecae1", "#4292c6", "#084594")) +
  scale_y_continuous(breaks = seq(0,320, by=20))
  
 ###########################################################################

 #Table 1: DRNS information
 
 Institution <- c("Glasgow Caledonian University", "University of Dundee", "Robert Gordon University", "University of Glasgow", "University of Glasgow", "University of Stirling", "University of Stirling",  "University of the West of Scotland", "University of the West of Scotland")
Title <- c("Substance Use", "Leverhulme Research Centre for Forensic Science","Addiction and Substance Use in a Range of Contexts", "Substance Use in a Contemporary World", "MRC/CSO Social and Public Health Sciences Unit",  "Substance Use", "Salvation Army Centre for Addiction Services and Research", "Contemporary Drug & Alcohol Studies", "Addiction Psychology")
Type <- c("Research Group", "Research Group","Short Course", "Microcredential", "Research Group",  "Taught Postgraduate", "Research Group", "Taught Postgraduate", "Taught Postgraduate")


data.frame(Institution, Title, Type) %>% 
  flextable() %>% 
  merge_v(j='Institution') %>% 
  # theme_vanilla() %>%
  # autofit() %>% 
  width(width = 2.5) %>% 
  add_header_row(colwidths = 3,
              values = "Table 1: Courses and research groups in relevant areas") %>% 
   bold(part = 'header') %>% 
  align(align = 'left', part='all') %>%
  vline(j=c(1,2), border = officer::fp_border())%>% 
  fontsize(i=1, size =12, part = 'header') %>% 
  border_outer(border = officer::fp_border()) %>% 
  hline(i=c(1,2,3,4,5,6,7,8), border = officer::fp_border())
  
###########################################################################
   
#FIGURE 3: NES DATA
   
#Step 1: read in datasets from NES (available here: https://turasdata.nes.nhs.scot/data-and-reports/official-workforce-statistics/all-official-statistics-publications/07-december-2021-workforce/dashboards/nhsscotland-workforce/?pageid=5982)

##Creating datasets for medical and nonmedical roles
NES_medical <- read_csv("//scotland.gov.uk/dc1/fs3_home/U449921/workforce/data/NES_medical.csv") %>% 
    mutate(date = as.Date(`Census date`, format = "%d/%m/%Y"),
           cat = "Medical") %>% 
    filter(Specialty %in% c("General practice", "General psychiatry", "Psychotherapy", "Clinical pharmacology & therapeutics")) %>% 
    group_by(Specialty, date, cat) %>% 
    summarise(WTE = sum(Employment)) 
  
NES_nonmedical <- read_csv("//scotland.gov.uk/dc1/fs3_home/U449921/workforce/data/NES_non-medical.csv") %>% 
  mutate(date = as.Date(`Census date`, format = "%d/%m/%Y"),
         cat = "Non-medical",
         Specialty = case_when(str_detect(`Sub job family`, "Adult|Mental health") ~ paste0(`Sub job family`, " nurse"),
                               T~`Sub job family`)) %>% 
  filter(Specialty %in% c("Adult nurse", "Mental health nurse", "Clinical psychology and counselling", "Pharmacy")) %>% 
  group_by(Specialty, date, cat) %>% 
  summarise(WTE = sum(Employment))

##Step 2: merging
summed_professions <- rbind(NES_medical, NES_nonmedical) %>% 
  filter(Specialty != "Adult nurse") %>% 
  ungroup() %>%
  mutate(Specialty = fct_reorder(Specialty, WTE, .desc = F),
         cat = case_when(str_detect(date, "09-30") ~ "Y", 
                         str_detect(date, "03-31") ~ "Y",
                         T~"N")) %>% 
  filter(cat == "Y")
  
##Step 3: Create alluvial chart
ggplot(data = summed_professions, aes(x=date, y=WTE, alluvium = Specialty)) +
  geom_alluvium(aes(fill = Specialty)) +
  scale_fill_manual(values = c("#3f007d",
                               "#6a51a3",
                               "#9e9ac8",
                               "#dadaeb",
                               "#08306b",
                               "#2171b5",
                               "#6baed6")) +
  theme+
  theme(axis.text.x = element_text(angle = 90, size=12, vjust=.5, hjust=1),) +
  scale_y_continuous(breaks = seq(0,70000, by=5000), labels = scales::comma, expand = c(0,250)) +
  scale_x_date(date_labels = "%d/%m/%Y", breaks=unique(summed_professions$date), expand = c(0,20)) +
  labs(x="",
       title="Figure 3: Registrations of select medical and non-medical roles, \nSeptember 2011-September 2021",
       caption = "Note: Purples refer to medical roles and blues are non-medical") +
  guides(fill=guide_legend(ncol=2))
  
 #FIGURE 4: Still NES data, but this time just for psychology
 
 read_csv("//scotland.gov.uk/dc1/fs3_home/U449921/workforce/data/psychology.csv") %>% 
  separate(`Census Date`, into = c("month","year"), " ") %>% 
  mutate(year = as.numeric(year),
         rank = row_number())  %>% 
  filter(year>2010,
         month %in% c("Mar", "Sep")) %>% 
  group_by(month, year) %>% 
  summarise(total = sum(`Sum of Value`)) %>% 
  arrange(-desc(year)) %>% 
  ungroup() %>% 
  mutate(date = str_c(month, "-", year),
         rank = row_number()) %>% 
  ggplot(aes(x=reorder(date,-rank), y=total)) +
  geom_col(fill = "#0065bd")+
  theme+
  coord_flip()+
  geom_text(aes(label = ifelse(rank %in%c(1,22), total, "")), hjust = -.1, size = 4.5) +
  labs(title = "Figure 4: WTE psychologists working in alcohol and substance abuse, \nJune 2011 - present",
       y="WTEs",
       x="")

###########################################################################

#Table 2: NES data again (data request)

#creating table (horribly tedious task, this is)
Course_Title <- c("Implementing Core Skills for Preventing Relapse and Managing Recovery Management in Substance Misuse Services",
                  "Introduction to CBT for Anxiety in Substance Misuse Settings", 
                  "Implementing Workforce Development Planning in Scottish Alcohol and Other Drug Services",
                  "Core Behavioural and CBT Skills for Relapse Prevention and Recovery Management ",
                  "Training the Trainers: Introduction to Core Behavioural and CBT Skills for Relapse Prevention and Recovery Management",
                  "Coaching the Coaches Seminar (invitation only)",
                  "Developing and Enhancing Motivational Coaching Skills",
                  "New Coach Training (Developing and Enhancing Motivational Coaching Skills)",
                  "Scottish Winter School in Motivational Interviewing",
                  "Introduction to Motivational Interviewing",
                  "Workforce Development Resources to Support Alcohol/Drug Work in Scotland",
                  "Safety and Stabilisation in Substance Misuse and Forensic Settings",
                  "TOTAL")
`2019-20` <- c(30, 11, 46, "-", 5, 9, 17, "-", 48, "-",  "-", "-", 166)
`2020-21` <- c("-", "-", "-", 46, 4, "-", 16, 6, 25, 23,  37,  31, 188)

##Creating vis
data.frame(Course_Title, `2019-20`, `2020-21`) %>%
  mutate(`Course Title` = Course_Title,
         `AY2019-20` = `2019-20`,
         `AY2020-21` = `2020-21`) %>% 
  select(`Course Title`:`AY2020-21`) %>% 
  flextable() %>%
  width(j = 1, width = 5.5) %>% 
  # theme_vanilla() %>%
  add_header_row(colwidths = 3,
                 values = "Table 2: Training courses available through NES, 2019-20 to 2020-21") %>%
  align(j=1, align = 'left') %>%
  bold(i = 13) %>%
  bold(part = 'header') %>% 
  vline(j=c(1,2), border = officer::fp_border()) %>% 
  fontsize(i=1, size =12, part = 'header') %>% 
  border_outer(border = officer::fp_border()) %>% 
  hline(i=c(1,2,3,4,5,6,7,8,9,10,11,12,13), border = officer::fp_border())
 
###########################################################################

#SDF DATA (bespoke data request)

#Figure 5: Training and CPD completion
#This dataset was in poor shape - lots of wrangling required

SDF <- read_csv("//scotland.gov.uk/dc1/fs3_home/U449921/workforce/data/SDF_training.csv") %>% 
  mutate(Course = case_when(str_detect(Title, "Stigma") ~ "Understanding stigma: promoting inclusive attitudes/practice",
                            str_detect(Title, 'Trauma') ~ "Introduction to trauma-informed practice for people who use substances",
                            str_detect(Title, 'Motivational') ~ "Introduction to motivational interviewing",
                            str_detect(Title, 'Alive') ~ "Staying alive: preventing drug-related deaths",
                            str_detect(Title, 'Child') ~ "Children Affected by Parental Substance Use",
                            str_detect(Title, 'Toot') ~ "Tooting v. Shooting (and other routes/methods of drug use)",
                            str_detect(Title, 'Alcohol') ~ "Understanding/Supporting People with Alcohol-Related Brain Injury",
                            str_detect(Title, 'Essential Skills|Engagement') ~"Essential skills for working with people who use substances",
                            str_detect(Title, 'Older') ~ "Older & Wiser? Working with people who use substances as they age",
                            T~Title),
         Sector = case_when(str_detect(Sector, 'voluntary|Voluntary') ~ "Charity/volunteer org.",
                            str_detect(Sector, "Housing") ~ "Housing and homelessness services",
                            str_detect(Sector, "National") ~ "NHS",
                            str_detect(Sector, "Police") ~ "Police services",
                            str_detect(Sector, "Social work") ~ "Social Work",
                            str_detect(Sector, "Family|Person") ~"People who use drugs/alcohol (inc. family)",
                            T~Sector),
         date = dmy(`End Date`),
         Year = as.character(year(date)),
         Sector = ifelse(is.na(Sector), 'Unknown', Sector),
         Course = str_wrap(Course, width = 40))


#Visual
SDF %>% 
  group_by(Course, Year) %>%
  summarise(total = n()) %>% 
  ggplot(aes(x= reorder(Year, desc(Year)), y=total, fill = Course)) +
  geom_col(position = 'stack') +
  theme_bw() +
  theme +
  coord_flip() +
  scale_fill_brewer(type = "qual", palette = "Paired") +
  scale_y_continuous(breaks = seq(0,3500, by=500), labels = scales::comma) +
    labs(title = "Figure 5: Enrollments on SDF core courses, 2019-2021*",
         y="Enrollments",
         x="Year",
         caption = "*Note 2021 figures are only through October") +
  guides(fill=guide_legend(ncol=2, reverse = T)) +
  geom_text(aes(label = ifelse(total>200, 
                                scales::comma(total, accuracy = 1), "")), position = position_stack(vjust = .5), size = 4.5) 

#Figure 6: Same data by sector
SDF %>% 
  group_by(Sector) %>%
  summarise(total = n()) %>% 
  filter(!Sector %in%(c("Transport and logistics", "Teacher, training and education", "Retail and sales", "Interested member of the public", "People who use drugs/alcohol (inc. family)"))) %>% 
  mutate(Sector = str_wrap(Sector, 30)) %>% 
  ggplot(aes(x= reorder(Sector,total), y=total)) +
  geom_col(fill = "#0065bd") +
  theme_bw() +
  coord_flip() +
  labs(title = "Figure 6: SDF core course enrollments by org. type, \n2019-2021",
       y="Enrollments",
       x="Sector",
       subtitle = "Omits sectors with <10 total enrollments*",
       caption="*Transport and logistics, teacher, training and education, retail and sales, interested member of the public, people who use drugs")+
  theme +
  geom_text(aes(label = scales::comma(total, accuracy = 1)), hjust = 1, size = 4.5) +
  scale_y_continuous(breaks = seq(0,2500, by=200),
                     labels = scales::comma)
                     
#Table 3: Same data by sector by year

SDF %>% 
  group_by(Sector,Year) %>% 
  summarise(total=n()) %>% 
  pivot_wider(names_from = Year, values_from = total) %>% 
  filter(!is.na(`2019`)) %>% 
  mutate(propdif = round((`2021`-`2019`)/`2019`*100, 0)) %>% 
  arrange(desc(propdif)) %>% 
  mutate(`% Change` =paste0(propdif, '%')) %>% 
  select(-propdif) %>%
  flextable() %>%
  width(width = 3, j=1) %>% 
  # theme_vanilla() %>%
  add_header_row(colwidths = 5,
                 values = "Table 3: Proportional differences in enrollment on SDF courses by sector, 2019 to 2021") %>%
  # align(align = 'center', part='all') %>%
  vline(j=c(1,2,3,4), border = officer::fp_border()) %>% 
  fontsize(i=1, size =12, part = 'header') %>% 
  bold(j = 5) %>% 
  bold(part = 'header') %>% 
  hline(i=c(1,2,3,4,5,6,7,8,9), border = officer::fp_border()) %>% 
  border_outer(border = officer::fp_border()) 


#Figure 7: Addiction Worker Training Project (bespoke request)

read_csv("//scotland.gov.uk/dc1/fs3_home/U449921/workforce/data/SDF_AWTP.csv") %>% 
  pivot_longer(Starts:Jobs, names_to = "Category", values_to = "Total") %>% 
  mutate(Year = as.character(Year), 
         Category = as.factor(Category),
         levels = case_when(Category == "Starts"~1, 
                            Category == "Completions"~2,
                            T~3)) %>% 
  ggplot(aes(x=Year, y=Total, fill = reorder(Category, levels)))+
  geom_col(position = "dodge", color = "black")+
  theme +
  theme(legend.title = element_blank())+
  scale_fill_manual(values = c("#0065bd", "#912688", "white")) +
  labs(title = "Figure 7: Enrollments on the Addiction Worker Training Project by category, \n2005-2020",
       y="Total",
       x="Year")
