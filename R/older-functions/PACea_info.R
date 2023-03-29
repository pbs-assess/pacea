#' @export
# Function for viewing all available datasets with information on them
PACea_info <- function(
  fetch_name=NULL,
  vignette_mode=F
)
{
  if(vignette_mode==F)
  {
    if(is.null(fetch_name))
    {
      return(View(PACea::Data_Key[,c('Fetch_Name','Time_Resolution','Data_Type','Units','Author','Citation','Comments','DF_Name')]))
    }
    if(!is.null(fetch_name))
    {
      return(View(PACea::Data_Key[
        grepl(tolower(PACea::Data_Key$Fetch_Name),
              pattern = tolower(fetch_name)),
        c('Fetch_Name','Time_Resolution','Data_Type','Units','Author','Citation','Comments','DF_Name')
      ]))
    }
  }
  if(vignette_mode==T)
  {
    if(is.null(fetch_name))
    {
      return((PACea::Data_Key[,c('Fetch_Name','Time_Resolution','Data_Type','Units','Author','Citation','Comments','DF_Name')]))
    }
    if(!is.null(fetch_name))
    {
      return((PACea::Data_Key[
        grepl(tolower(PACea::Data_Key$Fetch_Name),
              pattern = tolower(fetch_name)),
        c('Fetch_Name','Time_Resolution','Data_Type','Units','Author','Citation','Comments','DF_Name')
      ]))
    }
  }
}
