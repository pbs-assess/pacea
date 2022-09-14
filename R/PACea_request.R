#' @export
# function for directing the user to the data request page.
PACea_request <- function(vignette_mode=F)
{
  print('Please provide details on the possible sources and/or authors of the desired datasets to make our jobs easier!')
  if(!vignette_mode)
  {
    browseURL(url='https://github.com/pbs-assess/PACea/issues/3')
  }
}