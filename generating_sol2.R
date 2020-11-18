library(tidyverse)
# define file name
filesol <- "Raw_data/Y101_Y102_Y201_Y202_Y101-5.csv"

# skip first two lines
sol <- read_csv(filesol, skip = 2) %>% 

# filter out the bovine proteins and those proteins identified from fewer than 2 peptides
sol <- sol %>% 
  filter(str_detect(description, "OS=Homo sapiens")) %>% 
  filter(x1pep == "x")

# Extract the genename from the description and put it in a column.
sol <- sol %>%
  mutate(genename =  str_extract(description,"GN=[^\\s]+") %>% 
           str_replace("GN=", ""))

accession <- sol$accession[2]
protid <- str_extract(accession, "1::[^;]+") %>% 
  str_replace("1::", "")

# adding a new column 
sol <- sol %>%
  mutate(protid =  str_extract(accession, "1::[^;]+") %>% 
           str_replace("1::", ""))

sol2 <- sol %>% pivot_longer(names_to = "lineage_rep",
                             values_to = "abundance",
                             cols = starts_with("y"))

sol2 <- sol2 %>%
  extract(lineage_rep,
          c("line", "rep"),
          "(y[0-9]{3,4})\\_([a-c])")

file <-  "Processed_data/sol2.txt"
write.table(sol2, 
            file, 
            quote = FALSE,
            row.names = FALSE)