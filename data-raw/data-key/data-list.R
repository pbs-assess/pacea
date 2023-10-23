# generate pacea_data data object for list of files
library(dplyr)

bccm_data <- read.csv(paste0(here::here(),
                             "/data-raw/data-key/data_list.csv"),
                      header = TRUE)

use_data(bccm_data,
         overwrite = TRUE)
