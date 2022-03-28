##Creating a function to tidy employment figures BY EMPLOYMENT TYPE

staffing_wrangle <- function(df, profession) { 
df %>% 
    select(5,23:33,36:51,54:65, 68:87, 95:99, 106:109) %>% 
    pivot_longer(`9.1.1: Number of whole-time equivalent nursing posts as at 1 November 2021:: Employed (WTEs): Nurses - Band 5`:
                   `21.2.5: Number of whole-time equivalent 'other' posts as at 1 November 2021 : Employed (WTEs): -`,
                 names_to = "Job Type",
                 values_to = "Total") %>% 
    mutate(Category = case_when(str_detect(`Job Type`, "nursing") ~ "Nursing",
                                str_detect(`Job Type`, "medical") ~ "Medical",
                                str_detect(`Job Type`, "psychology") ~ "Psychology",
                                str_detect(`Job Type`, "PAID STAFF ONLY") ~ "Non-clinical",
                                T~ "Other"),
           Employment = case_when(str_detect(`Job Type`, "Employed") ~ "Employed",
                                  T~"Vacancy"),
           `Job Type` = str_remove_all(
             `Job Type`,
             "Number of whole-time equivalent | posts as at 1 November 2021|PAID STAFF ONLY|Employed|Vacancies|WTEs|WTE|nursing|medical|psychology|[().:']"),
           `Job Type` = case_when(Category == "Non-clinical"~str_sub(`Job Type`, 34, 100),
                                  T~str_sub(`Job Type`, 5,100)),
           `Job Type` = case_when(str_detect(`Job Type`, "Outreach workers") ~"Outreach workers",
                                  str_detect(`Job Type`, "COSCA") ~"COSCA-accredited counsellors",
                                  T~`Job Type`),
           `Job Type` = str_trim(`Job Type`)) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  group_by(`Job Type`, Category, Employment) %>% 
  summarise(sums = sum(Total)) %>% 
  filter(Category == profession) %>% 
  pivot_wider(names_from = `Job Type`, values_from = sums) %>% 
  mutate(TOTAL = rowSums(.[c(-1,-2)])) %>% 
  pivot_longer(-c(Category, Employment), names_to = "Job Type", values_to="Total")
}

##Creating dataset to create wafflechart

wafflechart <- rbind((staffing_wrangle(basedata, "Nursing") %>% 
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
  filter(`Job Type` == "TOTAL") %>% 
  select(Category, Employed) %>% 
  ungroup() %>% 
  add_row(Category = "Other", Employed = 78.49) %>% 
  mutate(total = sum(Employed),
         prop = round(Employed/total*100,0),
         Category = as.factor(Category)) %>% 
  arrange(desc(prop))

##rearranging order 
wafflechart$Category <- factor(wafflechart$Category, levels = c("Non-clinical", "Nursing", "Other", "Medical", "Psychology"))

##Creating vis
library(waffle)

ggplot(wafflechart, aes(fill=reorder(Category,prop), values = prop))+
  geom_waffle(n_rows = 5, size=.33, colour = "white")+
  scale_fill_manual(values = c("#deebf7","#9ecae1","#4292c6", "#08519c", "#08306b"))+
  # scale_fill_brewer(palette = "Blues")+
  coord_equal() +
  theme_void() +
   theme(legend.position = 'bottom',
        legend.text=element_text(size=10),
        legend.title=element_blank(),
        plot.title = element_text(size=16),
        # panel.grid.major.x = element_blank(),
        panel.grid.minor.x=element_blank(),
        panel.grid.minor.y = element_blank()) +
  labs(title = "Figure 4: Proportional distribution of employed WTEs across \nall respondents",
       caption = "n=1,572.8 WTEs") +
  guides(fill=guide_legend(reverse=T))
