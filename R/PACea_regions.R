#' @export
# Script for searching and plotting the available regions
Pacea_regions <- function(regions = NULL, plot=F, return_sf_Polys=F, approximate=F)
{
  if(plot)
  {
    print('Warning: The plotting function requires the ggplot2 R package to be installed. 
          If not installed already, run install.packages("ggplot2", dep=T)')
  }
  if(is.null(regions))
  {
    print(c('The available regions are called ',names(PACea::BC_Partition_Objects$index_vectors)))
    if(return_sf_Polys)
    {
    sf_Polys = vector('list', length(names(PACea::BC_Partition_Objects$index_vectors)))
    names(sf_Polys) <- names(PACea::BC_Partition_Objects$index_vectors)
    }
    
    if(plot | return_sf_Polys)
    {
      for(i in names(PACea::BC_Partition_Objects$index_vectors))
      {
        if(approximate==T)
        {
          region_Poly <- 
            PACea::BC_Partition_Objects$BC_Partition %>%
            dplyr::filter(Poly_ID %in% 
                            do.call('c',
                                    apply(PACea::BC_Partition_Objects$Mapping_Matrix[,
                                                                                     PACea::BC_Partition_Objects$index_vectors[[i]]
                                    ],
                                    2, FUN=function(x){which(x>0)}, 
                                    simplify = F))
            )
          # Which Poly_ID corresponds to which 'exact polygon'?
          region_Poly$Exact_Poly_ID <-
            do.call('c',
                    lapply(FUN=function(x){
                      paste0(PACea::BC_Partition_Objects$index_vectors[[i]][x],
                             collapse=' and ')},
                      apply(PACea::BC_Partition_Objects$Mapping_Matrix[region_Poly$Poly_ID,
                                                                       PACea::BC_Partition_Objects$index_vectors[[i]]], 
                            1, FUN=function(x){which(x>0)}, simplify = T)
                      )
                    )
          # And the names?
          region_Poly$Exact_Poly_Name <-
          do.call('c',
                  lapply(FUN=function(x){
                    paste0(PACea::BC_Partition_Objects$BC_Partition$Poly_Name[PACea::BC_Partition_Objects$index_vectors[[i]][x]],
                           collapse=' and ')},
                    apply(PACea::BC_Partition_Objects$Mapping_Matrix[region_Poly$Poly_ID,
                                                                     PACea::BC_Partition_Objects$index_vectors[[i]]], 
                          1, FUN=function(x){which(x>0)}, simplify = T)
                  )
          )
          
          if(plot)
          {
            if(length(unique(region_Poly$Exact_Poly_Name)) > 10)
            {
              print(
                ggplot2::ggplot(PACea::Coastline_Expanded) +
                  ggplot2::geom_sf(fill='lightblue') + 
                  ggplot2::geom_sf(data=region_Poly, 
                                   ggplot2::aes(fill=factor(Exact_Poly_Name, ordered = T),
                                                colour=factor(Exact_Poly_Name, ordered = T)), 
                                   alpha=0.2) +
                  ggplot2::geom_sf_label(data=region_Poly, ggplot2::aes(label=Exact_Poly_ID)) +
                  #ggplot2::guides(fill=ggplot2::guide_legend(title='Poly_Name'), colour=ggplot2::guide_legend(title='Poly_Name')) +
                  ggplot2::guides(fill="none", colour="none") +
                  ggplot2::ggtitle(paste0('PACea-Approximated Polygons Defining Region "',i,'"'),
                                   subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
              )
            }
            if(length(unique(region_Poly$Exact_Poly_Name)) <= 10)
            {
              print(
                ggplot2::ggplot(PACea::Coastline_Expanded) +
                  ggplot2::geom_sf(fill='lightblue') + 
                  ggplot2::geom_sf(data=region_Poly, 
                                   ggplot2::aes(fill=factor(Exact_Poly_Name, ordered = T),
                                                colour=factor(Exact_Poly_Name, ordered = T)), 
                                   alpha=0.2) +
                  ggplot2::geom_sf_label(data=region_Poly, ggplot2::aes(label=Exact_Poly_ID)) +
                  ggplot2::guides(fill=ggplot2::guide_legend(title='Poly_Name'), colour=ggplot2::guide_legend(title='Poly_Name')) +
                  #ggplot2::guides(fill="none", colour="none") +
                  ggplot2::ggtitle(paste0('PACea-Approximated Polygons Defining Region "',i,'"'),
                                   subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
              )
            }
          }
        }
        if(approximate==F)
        {
          region_Poly <- 
            PACea::BC_Partition_Objects$BC_Partition %>%
            dplyr::filter(Poly_ID %in% BC_Partition_Objects$index_vectors[[i]]
            )
          
          if(plot)
          {
            print(
              ggplot2::ggplot(PACea::Coastline_Expanded) +
                ggplot2::geom_sf(fill='lightblue') + 
                ggplot2::geom_sf(data=region_Poly, 
                                 ggplot2::aes(fill=factor(Poly_Name, levels=Poly_Name, ordered = T),
                                              colour=factor(Poly_Name, levels=Poly_Name, ordered = T)),
                                 alpha=0.2) +
                ggplot2::geom_sf_label(data=region_Poly, ggplot2::aes(label=Poly_ID)) +
                ggplot2::guides(fill=ggplot2::guide_legend(title='Poly_Name'), colour=ggplot2::guide_legend(title='Poly_Name')) +
                ggplot2::ggtitle(paste0('Exact Polygons Defining Region "',i,'"'),
                                 subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
            )
          }
        }
        if(return_sf_Polys)
        {
          sf_Polys[[i]] <- region_Poly
        }
      }
    }
  }
  if(!is.null(regions))
  {
    if(return_sf_Polys)
    {
      sf_Polys = vector('list', length(regions))
      names(sf_Polys) <- regions
    }
    
    if(plot | return_sf_Polys)
    {
      for(i in regions)
      {
        if(approximate==T)
        {
          if(!(i %in% names(PACea::BC_Partition_Objects$index_vectors)))
          {
            stop(paste0(i, ' is not a valid region. Please run Pacea_regions() to see the names of all the valid regions'))
          }
          
          region_Poly <- 
            PACea::BC_Partition_Objects$BC_Partition %>%
            dplyr::filter(Poly_ID %in% 
                            do.call('c',
                                    apply(PACea::BC_Partition_Objects$Mapping_Matrix[,
                                                                                     PACea::BC_Partition_Objects$index_vectors[[i]]
                                    ],
                                    2, FUN=function(x){which(x>0)}, simplify = F))
            )
          # Which Poly_ID corresponds to which 'exact polygon'?
          region_Poly$Exact_Poly_ID <-
            do.call('c',
                    lapply(FUN=function(x){
                      paste0(PACea::BC_Partition_Objects$index_vectors[[i]][x],
                             collapse=' and ')},
                      apply(PACea::BC_Partition_Objects$Mapping_Matrix[region_Poly$Poly_ID,
                                                                       PACea::BC_Partition_Objects$index_vectors[[i]]], 
                            1, FUN=function(x){which(x>0)}, simplify = T)
                    )
            )
          # And the names?
          region_Poly$Exact_Poly_Name <-
            do.call('c',
                    lapply(FUN=function(x){
                      paste0(PACea::BC_Partition_Objects$BC_Partition$Poly_Name[PACea::BC_Partition_Objects$index_vectors[[i]][x]],
                             collapse=' and ')},
                      apply(PACea::BC_Partition_Objects$Mapping_Matrix[region_Poly$Poly_ID,
                                                                       PACea::BC_Partition_Objects$index_vectors[[i]]], 
                            1, FUN=function(x){which(x>0)}, simplify = T)
                    )
            )
          if(plot)
          {
            if(length(unique(region_Poly$Exact_Poly_Name)) > 10)
            {
              print(
                ggplot2::ggplot(PACea::Coastline_Expanded) +
                  ggplot2::geom_sf(fill='lightblue') + 
                  ggplot2::geom_sf(data=region_Poly, 
                                   ggplot2::aes(fill=factor(Exact_Poly_Name, ordered = T),
                                                colour=factor(Exact_Poly_Name, ordered = T)), 
                                   alpha=0.2) +
                  ggplot2::geom_sf_label(data=region_Poly, ggplot2::aes(label=Exact_Poly_ID)) +
                  #ggplot2::guides(fill=ggplot2::guide_legend(title='Poly_Name'), colour=ggplot2::guide_legend(title='Poly_Name')) +
                  ggplot2::guides(fill="none", colour="none") +
                  ggplot2::ggtitle(paste0('PACea-Approximated Polygons Defining Region "',i,'"'),
                                   subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
              )
            }
            if(length(unique(region_Poly$Exact_Poly_Name)) <= 10)
            {
              print(
                ggplot2::ggplot(PACea::Coastline_Expanded) +
                  ggplot2::geom_sf(fill='lightblue') + 
                  ggplot2::geom_sf(data=region_Poly, 
                                   ggplot2::aes(fill=factor(Exact_Poly_Name, ordered = T),
                                                colour=factor(Exact_Poly_Name, ordered = T)), 
                                   alpha=0.2) +
                  ggplot2::geom_sf_label(data=region_Poly, ggplot2::aes(label=Exact_Poly_ID)) +
                  ggplot2::guides(fill=ggplot2::guide_legend(title='Poly_Name'), colour=ggplot2::guide_legend(title='Poly_Name')) +
                  #ggplot2::guides(fill="none", colour="none") +
                  ggplot2::ggtitle(paste0('PACea-Approximated Polygons Defining Region "',i,'"'),
                                   subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
              )
            }
          }
        }
        if(approximate==F)
        {
          if(!(i %in% names(PACea::BC_Partition_Objects$index_vectors)))
          {
            stop(paste0(i, ' is not a valid region. Please run Pacea_regions() to see the names of all the valid regions'))
          }
          
          region_Poly <- 
            PACea::BC_Partition_Objects$BC_Partition %>%
            dplyr::filter(Poly_ID %in% BC_Partition_Objects$index_vectors[[i]]
            )
          
          if(plot)
          {
            print(
              ggplot2::ggplot(PACea::Coastline_Expanded) +
                ggplot2::geom_sf(fill='lightblue') + 
                ggplot2::geom_sf(data=region_Poly, 
                                 ggplot2::aes(fill=factor(Poly_Name, levels=Poly_Name, ordered = T),
                                              colour=factor(Poly_Name, levels=Poly_Name, ordered = T)), 
                                 alpha=0.2) +
                ggplot2::geom_sf_label(data=region_Poly, ggplot2::aes(label=Poly_ID)) +
                ggplot2::guides(fill=ggplot2::guide_legend(title='Poly_Name'), colour=ggplot2::guide_legend(title='Poly_Name')) +
                ggplot2::ggtitle(paste0('Exact Polygons Defining Region "',i,'"'),
                                 subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
            )
          }
        }
        if(return_sf_Polys)
        {
          sf_Polys[[i]] <- region_Poly
        }
      }
    } 
  }
  if(return_sf_Polys)
  {
    return(sf_Polys)
  }
}