# Helper function for uploading new data to PACea
PACea_upload <- function(Time_Resolution, Data_Key=T)
{
  if(!(Time_Resolution %in% c('Fixed', 'Annual', 'Monthly')))
  {
    stop('Time_resolution must be one of "Fixed", "Annual", or "Monthly"')
  }
  
  if(Time_Resolution == 'Fixed')
  {
    col_names <- c('Poly_ID')
  }
  if(Time_Resolution == 'Annual')
  {
    col_names <- c('Poly_ID, Year')
  }
  if(Time_Resolution == 'Monthly')
  {
    col_names <- c('Poly_ID, Year, Month')
  }
  
  print(paste0('Based on the inputted information, the required column names for the data are: ',col_names))
  
  print(paste0('Please also fill in the information in the columns of the csv file **Upload_Data_Key.csv** found in the current working directory: ',getwd(),'.This should have been created if the argument <Data_Key=T>.'))
  
  print('Where possible, please provide an .Rmd, .R, or other script showing how the data were created.')
  
  print('We aim to update the datasets every ~6 months. If new data are likely to become available, then a well-documented and automated script can help us to update the data in the future.')
  
  if(Data_Key)
  {
    write.csv(
      data.frame(Fetch_Name=NA,
                 Time_Resolution=Time_Resolution,
                 Data_Type=NA,
                 Units=NA,
                 Author=NA,
                 Citation=NA,
                 Comments=NA),
      file = 'Upload_Data_Key.csv',
      row.names = F
    )
  }
  
  user_prompt <- readline(prompt = 
  'The data and the completed Data_Key should be uploaded to the PACea GitHub 
page at https://github.com/pbs-assess/PACea/issues/2. To open a web browser at the 
GitHub page, type in the number 1 and hit return.')
  
  if(as.integer(user_prompt) == 1)
  {
    browseURL(url='https://github.com/pbs-assess/PACea/issues/2')
  }
}