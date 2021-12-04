# Script for searching and plotting the available regions
Pacea_regions <- function(regions = NULL, plot=F, return_sf_Polys=F)
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
    sf_Polys = vector(list, length(names(PACea::BC_Partition_Objects$index_vectors)))
    names(sf_Polys) <- names(PACea::BC_Partition_Objects$index_vectors)
    }
    
    if(plot | return_sf_Polys)
    {
      for(i in names(PACea::BC_Partition_Objects$index_vectors))
      {
        region_Poly <- 
          PACea::BC_Partition_Objects$BC_Partition %>%
          filter(Poly_ID %in% 
                   do.call('c',
                           apply(PACea::BC_Partition_Objects$Mapping_Matrix[
                             PACea::BC_Partition_Objects$index_vectors[[i]],
                             ],
                             1, FUN=function(x){which(x==1)}, 
                             simplify = F))
                   )
        
        if(plot)
        {
          print(
            ggplot(PACea::Coastline_Expanded) +
              geom_sf(fill='lightblue') + 
              geom_sf(data=region_Poly, 
                      aes(fill=factor(Poly_Name, levels=Poly_Name, ordered = T),
                          colour=factor(Poly_Name, levels=Poly_Name, ordered = T)),
                      alpha=0.2) +
              geom_sf_label(data=region_Poly, aes(label=Poly_ID)) +
              guides(fill=guide_legend(title='Poly_Name'), colour=guide_legend(title='Poly_Name')) +
              ggtitle(paste0('Polygons Defining Region "',i,'"'),
                      subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
          )
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
      sf_Polys = vector(list, length(regions))
      names(sf_Polys) <- regions
    }
    
    if(plot | return_sf_Polys)
    {
      for(i in regions)
      {
        if(!(i %in% names(PACea::BC_Partition_Objects$index_vectors)))
        {
          stop(paste0(i, ' is not a valid region. Please run Pacea_regions() to see the names of all the valid regions'))
        }
        
        region_Poly <- 
          PACea::BC_Partition_Objects$BC_Partition %>%
          filter(Poly_ID %in% 
                   do.call('c',
                           apply(PACea::BC_Partition_Objects$Mapping_Matrix[
                             PACea::BC_Partition_Objects$index_vectors[[i]],
                           ],
                           1, FUN=function(x){which(x==1)}))
          )
        
        if(plot)
        {
          print(
            ggplot(PACea::Coastline_Expanded) +
              geom_sf(fill='lightblue') + 
              geom_sf(data=region_Poly, 
                      aes(fill=factor(Poly_Name, levels=Poly_Name, ordered = T),
                          colour=factor(Poly_Name, levels=Poly_Name, ordered = T)), 
                      alpha=0.2) +
              geom_sf_label(data=region_Poly, aes(label=Poly_ID)) +
              guides(fill=guide_legend(title='Poly_Name'), colour=guide_legend(title='Poly_Name')) +
              ggtitle(paste0('Polygons Defining Region "',i,'"'),
                      subtitle = 'Polygons coloured by Poly_Name and labelled by Poly_ID \nWhen numbers are present, they ascend in the same order')
          )
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