# Main script for fetching the data 
PACea_fetch <- function(
  regions, # Where do you want the data?
  fetch_names, # What data do you want?
  year_range, # When do you want the data?
  month_range, # When do you want the data?
  output_as_csv=F)

{
    
  if(max(year_range) > lubridate::year(lubridate::ymd(Sys.Date())) |
     min(year_range) < 1914)
  {
    stop(paste0('Year range must be between ', 1914,' and ',lubridate::year(lubridate::ymd(Sys.Date()))))
  }
  if(max(month_range) > 12 |
     min(month_range) < 1)
  {
    stop(paste0('Month range must be between ', 1,' and ',12))
  }
  if(length(regions) > 1)
  {
    stop('Data can only be extracted for one region at a time')
  }
  
  browser()
  # Match the common name to the corresponding pre-compiled data.frame
  DF_names <-
    PACea::Data_Key %>%
    dplyr::filter(Fetch_Name %in% fetch_names) %>%
    select(DF_Name, DF_Variable_Names, Time_Resolution) %>%
    group_by(Time_Resolution) # make sure Monthly variables (if present) are loaded first
  Time_Resolution <- DF_names$Time_Resolution
  DF_Variable_Names <- DF_names$DF_Variable_Names
  DF_names <- DF_names$DF_Name
  
  
  # create a function for reading package data by string
  getdata <- function(mydataset) {
    data(list=mydataset, package = 'PACea')  
    return(get(mydataset))
  }
  
  # Extract the indices of the desired region(s)
  # collapse into a vector
  region_indices <- do.call(
    PACea::BC_Partition_Objects$index_vectors[regions],
    what='c'
    )
  
  # Extract the names of the polygons within each region
  # collapse into a character vector
  # TODO
  # region_names <- do.call(
  #   PACea::BC_Partition_Objects$Poly_names_vectors[regions],
  #   what='c'
  # )
  
  # create a list of variable names - splitting the individual names by by the ';'
  DF_Variable_Names <- strsplit(DF_Variable_Names, ';')

  count <- 1  # loop through the DF_names and extract the data
  for(i in DF_names)
  {
    tmp_dat <- getdata(i)
    
    # Are any coastwide indices present in the data?
    # Coastwide indices stored as Poly_ID == -1
    
    # First - check that coastwide AND regional variables are not stored
    if(sum(tmp_dat$Poly_ID == -1) > 0 & sum(tmp_dat$Poly_ID != -1) > 0)
    {
      stop('It appears that Coastwide AND regional values were stored in the same 
           data.frame. This is not allowed. Please ensure the coastwide and regional
           values are stored in separate data.frames')
    }
    
    # If so replicate the variable values across the desired regions
    if(sum(tmp_dat$Poly_ID == -1) > 0)
    {
      tmp_dat <- 
        tmp_dat %>%
        mutate(nrep = length(region_indices)) %>%
        tidyr::uncount(nrep) %>%
        mutate(Poly_ID = rep(region_indices, times=dim(tmp_dat)[1]))
    }
    
    if(Time_Resolution[count] == 'Monthly')
    {
      tmp_dat <-
        tmp_dat %>%
        dplyr::filter(Year %in% year_range &
                 Month %in% month_range &
                 Poly_ID %in% region_indices) %>%
        dplyr::select(Year, Month, Poly_ID, DF_Variable_Names[[count]]) %>%
        tidyr::complete(Year=year_range, Month=month_range, Poly_ID=region_indices)# %>%
      # mutate(Poly_ID = PACea::BC_Partition_Objects$Poly_names_vectors[Poly_ID]) #TODO
    }
    if(Time_Resolution[count] == 'Annual')
    {
      tmp_dat <-
        tmp_dat %>%
        filter(Year %in% year_range &
                 Poly_ID %in% region_indices) %>%
        select(Year, Poly_ID, DF_Variable_Names[[count]]) %>%
        tidyr::complete(Year=year_range, Poly_ID=region_indices) # %>%
      # mutate(Poly_ID = PACea::BC_Partition_Objects$Poly_names_vectors[Poly_ID]) #TODO
    }
    if(Time_Resolution[count] == 'Fixed')
    {
      tmp_dat <-
        tmp_dat %>%
        filter(Poly_ID %in% region_indices) %>%
        select(Poly_ID, DF_Variable_Names[[count]]) %>%
        tidyr::complete(Poly_ID=region_indices) # %>%
      # mutate(Poly_ID = PACea::BC_Partition_Objects$Poly_names_vectors[Poly_ID]) #TODO
    }
    
    if(count == 1)
    {
      Data <- tmp_dat
    }
    if(count > 1)
    {
      Data <- dplyr::left_join(
        Data, 
        tmp_dat
      )
    }
    count <- count + 1 
  }
  # Map the Poly_Name to the Data
  Data$Poly_Name <-
    PACea::BC_Partition_Objects$BC_Partition$Poly_Name[
      Data$Poly_ID
    ]
  
  return(Data)
}
