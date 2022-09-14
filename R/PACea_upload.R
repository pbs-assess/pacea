#' @export
# Helper function for uploading new data to PACea
PACea_upload <- function(Spatial, Time_Resolution, Data_Key=T, vignette_mode = F)
{
  if(!(Time_Resolution %in% c('Fixed', 'Annual', 'Monthly')))
  {
    stop('Time_resolution must be one of "Fixed", "Annual", or "Monthly"')
  }
  if(!Spatial)
  {
    if(Time_Resolution == 'Fixed')
    {
      stop('Non-spatial data fixed in time do not make sense.')
    }
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
  
  if(!Spatial)
  {
    print("The column 'Poly_ID' must be set to -1 to indicate the data are non-spatial.")
  }
  
  if(Spatial)
  {
    print('Remember... PACea stores all spatial data internally across the BC Grid.')
    print('To return the exact BC Grid polygons in sf format, along with their coordinate reference system, run Pacea_regions("BC Grid", return_sf_Polys = T).')
    print('The polygons can be used in all commonly-used R packages for spatial aggregation (sp, sf, raster, etc.,) with appropriate conversion functions.')
    print('To export the polygons as a shapefile for external use (e.g. ESRI), run st_write(Pacea_regions("BC Grid", return_sf_Polys = T), "nameofshapefile.shp")')
  }
  
  print(paste0('Please also fill in the information in the columns of the csv file **Upload_Data_Key.csv** found in the current working directory: ',getwd(),'.This should have been created if the argument <Data_Key=T>.'))
  
  print('Where possible, please provide an .Rmd, .R, or other script showing how the data were created.')
  
  print('We aim to update the datasets every ~6 months. If new data are likely to become available, then a well-documented and automated script can help us to update the data in the future.')
  
  if(Data_Key & !vignette_mode)
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
  if(Data_Key & vignette_mode)
  {
    data.frame(Fetch_Name=NA,
                 Time_Resolution=Time_Resolution,
                 Data_Type=NA,
                 Units=NA,
                 Author=NA,
                 Citation=NA,
                 Comments=NA)
  }
  if(Data_Key & !vignette_mode)
  {
    user_prompt <- readline(prompt = 
                              'The data and the completed Data_Key should be uploaded to the PACea GitHub 
page at https://github.com/pbs-assess/PACea/issues/2. To open a web browser at the 
GitHub page, type in the number 1 and hit return.')
    
    if(as.integer(user_prompt) == 1)
    {
      browseURL(url='https://github.com/pbs-assess/PACea/issues/2')
    }
  }
  if(Data_Key & vignette_mode)
  {
    print('The data and the completed Data_Key should be uploaded to the PACea GitHub 
page at https://github.com/pbs-assess/PACea/issues/2. To open a web browser at the 
GitHub page, type in the number 1 and hit return.')
  }
}