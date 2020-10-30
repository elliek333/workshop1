#reading in the data
chaff <- read.delim("~/Documents/Uni/Data Science/workshop 1/workshop 1/Raw_data/chaff.txt")
View(chaff)

#tidying data
library(tidyverse)
chaff2<- chaff%>% 
  pivot_longer(names_to = "sex", 
               values_to = "mass",
               cols = everything())

# saving processed data
file <-  "Processed_data/chaff2.txt"
write.table(chaff2, 
            file, 
            quote = FALSE,
            row.names = FALSE)

#statistical analysis
t.test(chaff2$mass ~ chaff2$sex)

#summarise dataset
chaff_sum <- chaff2 %>% 
  group_by(sex) %>% 
  summarise(mean = mean(mass),
            n = length(mass),
            std= sd(mass),
            se = std/sqrt(n))

#making a figure
library(ggplot2)
fig <- ggplot(chaff2, aes(x = sex, y = mass)) + 
  geom_boxplot(color = c(rgb(0,0,1,1/2),rgb(1,0,0,1/2)), fill = c(rgb(0,0,1,1/4),rgb(1,0,0,1/4))) +
  theme_classic() +
  theme(axis.text.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = .5, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, angle = 0, hjust = 1, vjust = 0, face = "plain"),  
        axis.title.x = element_text(color = "black", size = 12, angle = 0, hjust = .5, vjust = 0, face = "plain"),
        axis.title.y = element_text(color = "black", size = 12, angle = 90, hjust = .5, vjust = .5, face = "plain")) +
  ylab("Mass (g)") +
  xlab("Sex") + 
  stat_summary(fun = mean,
               fun.min = function(x) mean(x) - sd(x), 
               fun.max = function(x) mean(x) + sd(x), 
               geom = "pointrange") +
  stat_summary(fun = mean,
               geom = "line") 

#saving the figure
units <- "in"  
fig_w <- 3.5
fig_h <- fig_w
dpi <- 300
device <- "tiff"
ggsave("Figures/kermit.tiff",
       plot = fig,
       device = device,
       width = fig_w,
       height = fig_h,
       units = units,
       dpi = dpi)


