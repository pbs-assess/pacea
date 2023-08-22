# generate pacea_data data object for list of files
library(dplyr)

roms_data <- read.csv("./data-raw/data-key/data_list.csv", header = T) %>% 
  arrange(data_name)

use_data(roms_data, overwrite = TRUE)
