#' @export
# Main script for fetching the data 
PACea_fetch <- function(
  regions, # Where do you want the data?
  poly_names=NULL, # do you only want the data returned on a subset of the polygons or even define a new region as the sum of polygons? 
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
  
  #browser()
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

  # create a list of variable names - splitting the individual names by by the ';'
  DF_Variable_Names <- strsplit(DF_Variable_Names, ';')

  count <- 1  # loop through the DF_names and extract the data
  for(i in DF_names)
  {
    tmp_dat <- getdata(i)
    
    # Are any coastwide indices present in the data?
    # Coastwide indices stored as Poly_ID == -1
    Coastwide_Logical <- tmp_dat$Poly_ID
    
    # First - check that coastwide AND regional variables are not stored
    if(sum(Coastwide_Logical == -1) > 0 & sum(Coastwide_Logical != -1) > 0)
    {
      stop('It appears that coastwide AND regional values were stored in the same 
           data.frame. This is not allowed. Please ensure the coastwide and regional
           values are stored in separate data.frames')
    }
    
    # If so replicate the variable values across the desired regions
    if(sum(Coastwide_Logical == -1) > 0)
    {
      #browser()
      tmp_dat <- 
        tmp_dat %>%
        mutate(nrep = length(region_indices)) %>%
        tidyr::uncount(nrep) %>%
        mutate(Poly_ID = rep(region_indices, times=dim(tmp_dat)[1]))
      
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
    }
    if(sum(Coastwide_Logical == -1) == 0)
    {
    if(Time_Resolution[count] == 'Monthly')
    {
      # Define empty dataframe for filling
      tmp_dat2 <-
        expand.grid(
          Year=year_range,
          Month=month_range,
          Poly_ID=region_indices
        )
      
      # Loop through variables, years and months
      for(j in 1:length(DF_Variable_Names[[count]]))
      {
        for(k in year_range)
        {
          for(l in month_range)
          {
            for(m in region_indices)
            {
              # compute the spatially-weighted mean using Mapping Matrix
              # First, compute the fraction of values missing and re-weight matrix
              # to ensure the columns sum to 1, even after accounting for missing values
              weight <-
                sum(PACea::BC_Partition_Objects$Mapping_Matrix[
                  which(!is.na(as.numeric(
                    tmp_dat %>%
                      dplyr::filter(Year == k & Month == l) %>%
                      dplyr::pull(DF_Variable_Names[[count]][j])))),
                  m])
              
              tmp_dat2[tmp_dat2$Year==k & 
                         tmp_dat2$Month==l & 
                         tmp_dat2$Poly_ID==m,
                       DF_Variable_Names[[count]][j]] <-
                ifelse(sum(!is.na(as.numeric(
                  tmp_dat %>%
                     dplyr::filter(Year == k & Month == l) %>%
                    dplyr::pull(DF_Variable_Names[[count]][j])))
                  ) > 0,
                sum(PACea::BC_Partition_Objects$Mapping_Matrix[,m] * 
                as.numeric(tmp_dat %>%
                dplyr::filter(Year == k & Month == l) %>%
                pull(DF_Variable_Names[[count]][j])),
                na.rm=T)/weight,
                NA)
            }
          }
        }
      }
      tmp_dat <- tmp_dat2
    
    }
    if(Time_Resolution[count] == 'Annual')
    {
      # Define empty dataframe for filling
      tmp_dat2 <-
        expand.grid(
          Year=year_range,
          Poly_ID=region_indices
        )
      
      # Loop through variables, years and months
      for(j in 1:length(DF_Variable_Names[[count]]))
      {
        for(k in year_range)
        {
          for(m in region_indices)
            {
            weight <-
              sum(PACea::BC_Partition_Objects$Mapping_Matrix[
                which(!is.na(as.numeric(
                  tmp_dat %>%
                    dplyr::filter(Year == k) %>%
                    dplyr::pull(DF_Variable_Names[[count]][j])))),
                m])
            
              tmp_dat2[tmp_dat2$Year==k & 
                        tmp_dat2$Poly_ID==m,
                      DF_Variable_Names[[count]][j]] <-
                ifelse(sum(!is.na(as.numeric(
                  tmp_dat %>%
                    dplyr::filter(Year == k) %>%
                    dplyr::pull(DF_Variable_Names[[count]][j])))
                ) > 0,
                sum(PACea::BC_Partition_Objects$Mapping_Matrix[,m] * 
                      as.numeric(tmp_dat %>%
                                   dplyr::filter(Year == k) %>%
                                   pull(DF_Variable_Names[[count]][j])),
                    na.rm=T)/weight,
                NA)
            }
          
        }
      }
      
      tmp_dat <- tmp_dat2
    }
    if(Time_Resolution[count] == 'Fixed')
    {
      # Define empty dataframe for filling
      tmp_dat2 <-
        expand.grid(
          Poly_ID=region_indices
        )
      
      # Loop through variables
      for(j in 1:length(DF_Variable_Names[[count]]))
      {
          for(m in region_indices)
          {
            weight <-
              sum(PACea::BC_Partition_Objects$Mapping_Matrix[
                which(!is.na(as.numeric(
                  tmp_dat %>%
                    dplyr::pull(DF_Variable_Names[[count]][j])))),
                m])
            
            tmp_dat2[tmp_dat2$Poly_ID==m,
                    DF_Variable_Names[[count]][j]] <-
              ifelse(sum(!is.na(as.numeric(
                tmp_dat %>%
                  dplyr::pull(DF_Variable_Names[[count]][j])))
              ) > 0,
              sum(PACea::BC_Partition_Objects$Mapping_Matrix[,m] * 
                    as.numeric(tmp_dat %>%
                                 pull(DF_Variable_Names[[count]][j])),
                  na.rm=T)/weight,
              NA)
          }
      }
      tmp_dat <- tmp_dat2
    }
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
  
  # Have specific polygon names been requested?
  if(!is.null(poly_names))
  {
    # Subset the data by the requested poly_names
    # The following searches for '+' signs in the strings of polygons
    # If at least one exists, then we must combine these regions
    # This feature allows users to define their own custom regions
    # Most likely approximating complex regions not currently in PACea
    # with a union of the BC grid cells
    if(max(stringr::str_count(poly_names, pattern = '\\+')) > 0)
    {
      #browser()
      # Obtain the data over custom-defined regions
      # Do a weighted average where relevant

      # First, extract the standard regions
      # 'standard' means not-summed
      # Example - a user could request HG and WCVI+WCHG
      # The first region HG would be 'standard'
      Data_standard <-
        Data %>%
        dplyr::filter(Poly_Name %in% poly_names)
      
      # Which regions are non-standard (i.e. joined by '+' sign)?
      # Strip the '+' signs
      regions <- stringr::str_split(poly_names, pattern = '\\+')
      
      # Loop through the individual region combinations
      for(i in 1:length(regions))
      {
        # Are the requested polygons valid?
        if(sum(!(regions[[i]] %in% Data$Poly_Name)) > 0)
        {
          stop('Some of the Poly_Name requests are invalid. Please run
                 Pacea_regions() to see the available regions and then run
                 Pacea_regions("Name_of_region",plot=T) to see the polygon names')
        }
        # If the length of a region is longer than 1, then it is a summed region
        # I.e. non-standard
        if(length(regions[[i]]) > 1)
        {
          # Is this annual data?
          if(sum(c('Year','Month') %in% names(Data)) == 1) # Annual data
          {
            # Keep only the polygons inside the user-defined region
            # Group by Year, Polygon and multiply values by area of each polygon
            # Sum up and divide by the total area of the custom region
            # This performs area-weighted average. Account for missing data by
            # Multiplying by 0 if missing data and 1 if data are available
            # Adjust the total sum accordingly (for missing data) too.
            # Rename the polygon and assign it values beginning 10000 to indicate custom
            Data_nonstandard_tmp <-
              Data %>%
              dplyr::filter(Poly_Name %in% regions[[i]]) %>%
              group_by(Year, Poly_ID, Poly_Name) %>%
              summarize(across(.fns=function(x){(
                BC_Partition_Objects$Ocean_Intersection_Areas[
                  pmatch(Poly_Name,
                         BC_Partition_Objects$BC_Partition$Poly_Name)
                ] * x) /
                  sum(BC_Partition_Objects$Ocean_Intersection_Areas[
                    pmatch(regions[[i]],
                           BC_Partition_Objects$BC_Partition$Poly_Name)
                  ] * !is.na(x))})) %>%
              ungroup(Poly_ID, Poly_Name) %>%
              summarize(across(!c(Poly_ID, Poly_Name),
                               .fns = function(x){
                                 ifelse(sum(!is.na(x)) > 0,
                                        sum(x, na.rm=T),
                                        NA
                                 )
                               }
                               )) %>%
              mutate(Poly_Name = stringr::str_flatten(regions[[i]],'+'),
                     Poly_ID = 10000+i)
          }
          if(sum(c('Year','Month') %in% names(Data)) == 2) # Monthly data
          {
            # Keep only the polygons inside the user-defined region
            # Group by Year, Month, Polygon and multiply values by area of each polygon
            # Sum up and divide by the total area of the custom region
            # This performs area-weighted average. Account for missing data by
            # Multiplying by 0 if missing data and 1 if data are available
            # Adjust the total sum accordingly (for missing data) too.
            # Rename the polygon and assign it values beginning 10000 to indicate custom
            Data_nonstandard_tmp <-
              Data %>%
              dplyr::filter(Poly_Name %in% regions[[i]]) %>%
              group_by(Year, Month, Poly_ID, Poly_Name) %>%
              summarize(across(.fns=function(x){(
                BC_Partition_Objects$Ocean_Intersection_Areas[
                  pmatch(Poly_Name,
                         BC_Partition_Objects$BC_Partition$Poly_Name)
                ] * x) /
                  sum(BC_Partition_Objects$Ocean_Intersection_Areas[
                    pmatch(regions[[i]],
                           BC_Partition_Objects$BC_Partition$Poly_Name)
                  ] * !is.na(x))})) %>%
              ungroup(Poly_ID, Poly_Name) %>%
              summarize(across(!c(Poly_ID, Poly_Name),
                               .fns = function(x){
                                 ifelse(sum(!is.na(x)) > 0,
                                        sum(x, na.rm=T),
                                        NA
                                 )
                               }
              )) %>%
              mutate(Poly_Name = stringr::str_flatten(regions[[i]],'+'),
                     Poly_ID = 10000+i)
          }
          if(sum(c('Year','Month') %in% names(Data)) == 0) # Fixed data
          {
            # Keep only the polygons inside the user-defined region
            # Group by Polygon and multiply values by area of each polygon
            # Sum up and divide by the total area of the custom region
            # This performs area-weighted average. Account for missing data by
            # Multiplying by 0 if missing data and 1 if data are available
            # Adjust the total sum accordingly (for missing data) too.
            # Rename the polygon and assign it values beginning 10000 to indicate custom
            Data_nonstandard_tmp <-
              Data %>%
              dplyr::filter(Poly_Name %in% regions[[i]]) %>%
              group_by(Poly_ID, Poly_Name) %>%
              summarize(across(.fns=function(x){(
                BC_Partition_Objects$Ocean_Intersection_Areas[
                  pmatch(Poly_Name,
                         BC_Partition_Objects$BC_Partition$Poly_Name)
                ] * x) /
                  sum(BC_Partition_Objects$Ocean_Intersection_Areas[
                    pmatch(regions[[i]],
                           BC_Partition_Objects$BC_Partition$Poly_Name)
                  ] * !is.na(x))})) %>%
              ungroup(Poly_ID, Poly_Name) %>%
              summarize(across(!c(Poly_ID, Poly_Name),
                               .fns = function(x){
                                 ifelse(sum(!is.na(x)) > 0,
                                        sum(x, na.rm=T),
                                        NA
                                 )
                               }
              )) %>%
              mutate(Poly_Name = stringr::str_flatten(regions[[i]],'+'),
                     Poly_ID = 10000+i)
          }
          if(i==1)
          {
            # If first region, assign subsetted data
            Data_nonstandard <- 
              Data_nonstandard_tmp
          }
          if(i>1)
          {
            # All further regions get concatenated onto existing data
            Data_nonstandard <-
              rbind(Data_nonstandard,
                    Data_nonstandard_tmp)
          }
        }
        
      }
      if(dim(Data_standard)[1] > 0)
      {
        # Concatenate the standard regions with the custom regions
        Data <- dplyr::full_join(
          Data_nonstandard,
          Data_standard
        )
      }
      if(dim(Data_standard)[1] == 0)
      {
        # If no standard regions, simply return the custom regions
        Data <- Data_nonstandard
      }
      # The following warning is useful to remind users about how the data were created
      print('Warning: An area-weighted mean was computed to define the variable values across the user-defined regions. In some cases, this procedure will not be appropriate. ')
    }
    if(max(stringr::str_count(poly_names, pattern = '\\+')) == 0)
    {
      # Are the requested polygons valid?
      if(sum(!(poly_names %in% Data$Poly_Name)) > 0)
      {
        stop('Some of the Poly_Name requests are invalid. Please run
                 Pacea_regions() to see the available regions and then run
                 Pacea_regions("Name_of_region",plot=T) to see the polygon names')
      }
      # If no custom (nonstandard) regions defined, then return requested polygons
      Data <-
        Data %>%
        dplyr::filter(Poly_Name %in% poly_names)
    }
  }
  
  if(!output_as_csv)
  {
    return(Data)
  }
  if(output_as_csv)
  {
    write.csv(
      Data, 
      file=paste0('PACea_',
                  stringr::str_flatten(regions,'_'),
                  as.character(min(year_range)), '_',
                  as.character(max(year_range)), '_',
                  '.csv')
    )
    print(paste0('The .csv file ',
                 'PACea_',
                 stringr::str_flatten(regions,'_'),
                 as.character(min(year_range)), '_',
                 as.character(max(year_range)), '_',
                 '.csv',
                 ' has been written to the directory ',getwd()))
  }
}
