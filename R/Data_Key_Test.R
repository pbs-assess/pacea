# Test to check that all the data.frames present in the folder "/data" have been added to the Data_Key
# NOTE: Doesn't work within testthat folder as it does not allow setwd() to be outside /testthat
Data_Key_Test <- function()
{
  test_that('Have all data.frames present in the folder "/data" been added to the Data_Key?',{
    expect_equal(length(unique(PACea::Data_Key$DF_Name)),
                 sum(grepl(list.files(), pattern='DF')))
    if(length(unique(Data_Key$DF_Name)) != sum(grepl(list.files(), pattern='DF')) )
    {
      names_DF_files <- str_split(list.files(),pattern = '.rda', simplify = T)[,1]
      names_DF_files <- names_DF_files[grepl(names_DF_files, pattern = 'DF')]
      print(paste0('The following files appear to need adding to the Data_Key: ',
                   names_DF_files[!(names_DF_files %in% PACea::Data_Key$DF_Name)]))
    }
  })
  
}
