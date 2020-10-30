library(tidyverse)
library(readr)
install.packages("janitor")
library(janitor)
filesol <- "raw_data/Y101_Y102_Y201_Y202_Y101-5.csv"
sol <- read_csv(filesol, skip = 2) %>% 
  janitor::clean_names()
sol <- sol %>% 
  filter(str_detect(description, "OS=Homo sapiens")) %>% 
  filter(x1pep == "x")
one_description <- sol$description[1]
str_extract(one_description,"GN=[^\\s]+")
str_extract(one_description, "GN=[^\\s]+") %>% 
  str_replace("GN=", "")
sol <- sol %>%
  mutate(genename =  str_extract(description,"GN=[^\\s]+") %>% 
           str_replace("GN=", ""))