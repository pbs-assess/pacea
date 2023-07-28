# generate pacea_data data object for list of files

pacea_data <- read.csv("./data-raw/data-key/data_list.csv", header = T)
pacea_data <- pacea_data[order(pacea_data$data_name),]

use_data(pacea_data, overwrite = TRUE)


