#' @export
# Helper script for generating and plotting custom regions
PACea_custom <- function(poly_ids=NULL, poly_names=NULL)
{
  # The function expects poly_ids to be a numeric vector of ids which will 
  # be merged together to form a custom-region
  
  # poly_names must be a character vector of polygon names which will 
  # be merged together to form a custom-region. This is alternative specification
  
  if(!is.null(poly_ids) & !is.null(poly_names))
  {
    stop('Only one of poly_ids or poly_names can be specified')
  }
  
  region_poly <- 
    PACea::BC_Partition_Objects$BC_Partition 
  
  # Have specific polygon names been requested?
  if(!is.null(poly_names))
  {
    exact_region_poly <-
      region_poly %>%
      filter(Poly_Name %in% poly_names) %>%
      mutate(Poly_Name_New = 'Custom Region')
    
    # How is it approximated in PACea
    approx_region_poly <-
      region_poly %>%
      dplyr::filter(Poly_ID %in% 
                      do.call('c',
                              apply(PACea::BC_Partition_Objects$Mapping_Matrix[,
                                                                               exact_region_poly$Poly_ID
                              ],
                              2, FUN=function(x){which(x>0)}, 
                              simplify = F))
      ) %>%
      mutate(Poly_Name_New = 'Custom Region')
  }
  # Have specific polygon ids been requested?
  if(!is.null(poly_ids))
  {
    exact_region_poly <-
      region_poly %>%
      filter(Poly_ID %in% poly_ids) %>%
      mutate(Poly_Name_New = 'Custom Region')
    
    # How is it approximated in PACea
    approx_region_poly <-
      region_poly %>%
      dplyr::filter(Poly_ID %in% 
                      do.call('c',
                              apply(PACea::BC_Partition_Objects$Mapping_Matrix[,
                                                                               exact_region_poly$Poly_ID
                              ],
                              2, FUN=function(x){which(x>0)}, 
                              simplify = F))
      ) %>%
      mutate(Poly_Name_New = 'Custom Region')
  }
  
  # Plot the exact and approximated regions
  print(
    ggplot2::ggplot(PACea::Coastline_Expanded) +
      ggplot2::geom_sf(fill='lightblue') + 
      ggplot2::geom_sf(data=exact_region_poly, 
                       ggplot2::aes(fill=factor(Poly_Name_New, ordered = T),
                                    colour=factor(Poly_Name_New, ordered = T)), 
                       alpha=0.2) +
      ggplot2::geom_sf_label(data=exact_region_poly, ggplot2::aes(label=Poly_ID)) +
      ggplot2::guides(fill=ggplot2::guide_legend(title='Region'), colour=ggplot2::guide_legend(title='Region')) +
      #ggplot2::guides(fill="none", colour="none") +
      ggplot2::ggtitle(paste0('Polygons Defining Custom Region Exactly'),
                       subtitle = 'Polygons labelled by exact Poly_ID')
  )
  
  print(
    ggplot2::ggplot(PACea::Coastline_Expanded) +
      ggplot2::geom_sf(fill='lightblue') + 
      ggplot2::geom_sf(data=approx_region_poly, 
                       ggplot2::aes(fill=factor(Poly_Name_New, ordered = T),
                                    colour=factor(Poly_Name_New, ordered = T)), 
                       alpha=0.2) +
      ggplot2::geom_sf_label(data=approx_region_poly, ggplot2::aes(label=Poly_ID)) +
      ggplot2::guides(fill=ggplot2::guide_legend(title='Region'), colour=ggplot2::guide_legend(title='Region')) +
      #ggplot2::guides(fill="none", colour="none") +
      ggplot2::ggtitle(paste0('PACea-Approximation of Custom Region'),
                       subtitle = 'Polygons labelled by Poly_ID from the BC Approximation Grid')
  )
  
  # Tell the user how to request data across this custom region
  print(paste0("To request data across this custom region using PACea_fetch(), use the argument poly_names='",paste0(exact_region_poly$Poly_Name,collapse='+'),
               "'"))
}