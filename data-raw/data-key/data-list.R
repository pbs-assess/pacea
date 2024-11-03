# generate pacea_data data objects for list of files
# library(dplyr)

bccm_data <- read.csv(paste0(here::here(),
                             "/data-raw/data-key/data_list.csv"),
                      header = TRUE)

use_data(bccm_data,
         overwrite = TRUE)

bccm_data_full <- bccm_data
for(i in 1:nrow(bccm_data_full)){
  bccm_data_full[i, "data_name"] <- paste0(bccm_data_full[i, "data_name"],
                                           "_full")
}

use_data(bccm_data_full,
         overwrite = TRUE)


# Same names are used for bccm_full, just with _full appended. So no need to
#  save another data object here.

hotssea_data <- read.csv(paste0(here::here(),
                                "/data-raw/data-key/hotssea_data_list.csv"),
                         header = TRUE)

use_data(hotssea_data,
         overwrite = TRUE)
