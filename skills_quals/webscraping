library(rvest)
library(tidyverse)

###################################################################################

#This was an aborted attempt to employ webscraping in gathering more granular data from university/college webpages. Amazingly the method actually worked, but with substantial shortcomings – namely, it did not function when course offerings were in a javascript (i.e. reactive) table. 

The two examples below are demonstrative. Edinburgh and Glasgow Unis have their course offerings in standard HTML format, but comparing their DevTools shows the extent to which the formatting differs. In other words, this approach was far too fiddly to be applied at scale. 

But... it *did* work, and the applications here can be extended to other workstreams too.

###################################################################################

Edinburgh_base <- read_html("https://www.ed.ac.uk/studying/undergraduate/degrees/index.php?action=degreeList")

##Extracting course titles
Edinburgh_extract_course <- Edinburgh_base %>% 
   html_nodes('body') %>% 
   xml_find_all("//a[contains(@class, 'list-group-item')]") %>% 
   html_text()

##Extracting URLs
Edinburgh_URL <- 
  Edinburgh_base %>% 
  html_nodes('a') %>% 
  html_attr('href') %>% 
  str_subset('&code=')

##Creating dataset
Edinburgh <- data.frame(Edinburgh_extract_course, Edinburgh_URL) %>% 
  mutate(Edinburgh_URL = paste0("https://www.ed.ac.uk", Edinburgh_URL),
         uni = "University of Edinburgh")

###################################################################################

Glasgow_base <- read_html("https://www.gla.ac.uk/undergraduate/degrees/")

Glasgow_extract_course <- Glasgow_base %>% 
  html_nodes('body') %>% 
  xml_find_all("//li") %>% 
  html_text() 

#Subsetting to capture only course titles
Glasgow_extract_course <- Glasgow_extract_course[23:143]

Glasgow_URL <- 
  Glasgow_base %>% 
  html_nodes('a') %>% 
  html_attr('href')

#Again with the URLs
Glasgow_URL <- Glasgow_URL[28:148]

Glasgow <- data.frame(Glasgow_extract_course, Glasgow_URL) %>% 
  mutate(Glasgow_URL = paste0("https://www.gla.ac.uk", Glasgow_URL),
         uni = "University of Glasgow")
         
