#Charts that I liked but that unfortunately did not fit anywhere else, so added here for posterity

## see basedata from main_vis.R

#TIME CHART (front matter)
basedata %>% 
  select(Date) %>% 
  separate(Date, into = c("Date", "Time"), sep = 10) %>% 
  mutate(time = format(strptime(Time, format = "%H:%M:%S"), '%I:%M:%S %p')) %>% 
  separate(time, into = c("hour", "rest"), sep = 2) %>% 
  separate(rest, into = c("guff", "class"), sep = 7) %>% 
  group_by(hour, class) %>% 
  summarise(total = n()) %>% 
  ungroup() %>% 
  add_row(hour = "07", class = "PM", total = 0) %>% 
  ggplot(aes(x=as.factor(hour), y=total, fill = class))+
  geom_col() +
  labs(x="",
       y="",
       title = "Figure 1: Responses to workforce survey by time of day (GMT)",
       subtitle = "n=89")+
  coord_polar(theta="x", start = .26) +
  theme_bw()+
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_text(size = 25),
        legend.title=element_blank()) 
        

#CUMULATIVE RESPONDENTS CHART (front matter)
basedata %>% 
  select(Date) %>% 
  separate(Date, into = c("Date", "Time"), sep = 10) %>% 
  add_row(Date = "2021-11-22", Time = "") %>% 
  mutate(date = as.Date(Date, '%Y-%m-%d')) %>% 
  group_by(date) %>% 
  summarise(total = n()) %>%
  ungroup() %>% 
  ggplot(aes(x=date, y=cumsum(total))) +
  geom_line() +
  theme_bw()+
  scale_y_continuous(breaks = seq(0,90, by = 5)) +
  labs(x="",
       y="Responses",
       title = "Figure 2: Cumulative responses to the workforce survey")+
  geom_vline(xintercept = as.numeric(as.Date("2021-11-30")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-06")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-09")), linetype = 4, colour = "blue") +
  geom_vline(xintercept = as.numeric(as.Date("2021-12-15")), linetype = 4, colour = "blue")
