#load in the data
library(readr)
Human_development_index <- read_csv("Raw_data/Human-development-index.csv")

#tidying the data
library(tidyverse)

hdi2 <- Human_development_index %>% 
  pivot_longer(names_to = "year", 
               values_to = "index",
               cols = -c(1,2))
#get rid of na, specify col                
hdi_no_na <- hdi2 %>% 
  drop_na(index)
#filter out na rows
hdi_no_na <-  hdi2 %>%  
  filter(!is.na(index))

#summarise the data
hdi_summary <- hdi_no_na %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(index))
hdi_summary <- hdi_no_na %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(index),
            n = length(index), sd_index = sd(index), se_index = sd_index/sqrt(n))
hdi_summary_low <- hdi_summary %>% 
  filter(rank(mean_index) < 11)
hdi_summary_low

hdi_summary_low %>% 
  ggplot() +
  geom_point(aes(x = Country,
                 y = mean_index)) +
  geom_errorbar(aes(x = Country,
                    ymin = mean_index - se_index,
                    ymax = mean_index + se_index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

#continous code
hdi3 <- Human_development_index %>% 
  pivot_longer(names_to = "year", 
               values_to = "index",
               cols = -c(1,2)) %>% 
  drop_na(index) %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(index)) %>% 
  group_by(Country) %>% 
  summarise(mean_index = mean(index),
            n = length(index), sd_index = sd(index), se_index = sd_index/sqrt(n)) %>% 
  filter(rank(mean_index) < 11) %>% 
  ggplot() +
  geom_point(aes(x = Country,
                 y = mean_index)) +
  geom_errorbar(aes(x = Country,
                    ymin = mean_index - se_index,
                    ymax = mean_index + se_index)) +
  scale_y_continuous(limits = c(0, 0.5),
                     expand = c(0, 0),
                     name = "HDI") +
  scale_x_discrete(expand = c(0, 0),
                   name = "") +
  theme_classic() +
  coord_flip()

#saving processed data
file <-  "Processed_data/hdi2.txt"
write.table(hdi2, 
            file, 
            quote = FALSE,
            row.names = FALSE)
file <-  "Processed_data/hdi_summary_low.txt"
write.table(hdi_summary_low, 
            file, 
            quote = FALSE,
            row.names = FALSE)
file <-  "Processed_data/hdi_summary.txt"
write.table(hdi_summary, 
            file, 
            quote = FALSE,
            row.names = FALSE)
file <-  "Processed_data/hdi_no_na.txt"
write.table(hdi_no_na, 
            file, 
            quote = FALSE,
            row.names = FALSE)

