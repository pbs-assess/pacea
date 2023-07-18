# generate pacea_data data object for list of files

pacea_data <- read.csv("./data-raw/data-key/data_list.csv")

use_data(pacea_data, overwrite = T)


