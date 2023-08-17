# generate pacea_data data object for list of files
library(dplyr)

pacea_data <- read.csv("./data-raw/data-key/data_list.csv", header = T) %>% 
  arrange(data_name)

use_data(pacea_data, overwrite = TRUE)
