# Function for viewing all available datasets with information on them
PACea_info <- function(
  common_name=NULL
)
{
  if(is.null(common_name))
  {
    return(View(PACea::Data_Key))
  }
  if(!is.null(common_name))
  {
    return(View(PACea::Data_Key[
      grepl(tolower(PACea::Data_Key$Common_Name),
            pattern = tolower(common_name)),
    ]))
  }
}
