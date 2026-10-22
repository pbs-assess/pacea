

# Need to use older verion of R (e.g. R4.2.3) for PBSdata and PBSmapping to work

library(PBSmapping)
library(PBSdata)
library(sf)
library(dplyr)
library(usethis)


load_all()

# ============================================================
# 1. Function to convert a PolySet object to a list of separate sf polygon objects
# ============================================================

polyset_to_sf_list <- function(polyset, prefix, crs = 4326) {
  
  # Extract polygon metadata
  polydata <- as.data.frame(attr(polyset, "PolyData"))
  
  # if (is.null(polydata) || !"label" %in% names(polydata)) {
  #   stop("PolySet must have a PolyData attribute with a 'label' column.")
  # }
  
  # Get unique polygon IDs
  pids <- unique(polyset$PID)
  
  # Convert each polygon
  sf_list <- lapply(pids, function(pid) {
    
    # Subset polygon
    p <- polyset[polyset$PID == pid, ]
    
    # Extract coordinates
    coords <- as.matrix(p[, c("X", "Y")])
    
    # Close polygon if necessary
    if (!all(coords[1, ] == coords[nrow(coords), ])) {
      coords <- rbind(coords, coords[1, ])
    }
    
    # Create sf polygon
    geom <- st_sfc(
      st_polygon(list(coords)),
      crs = crs
    )
    
    # Create sf object
    out <- st_sf(
      PID = pid,
      geometry = geom
    )
    
    # Join PolyData attributes
    metadata <- polydata %>%
      filter(PID == pid)
    
    if (nrow(metadata) == 1) {
      out <- left_join(out, metadata, by = "PID")
    }
    
    out
  })
  
  # Get labels corresponding to each PID
  labels <- polydata$PID[
    match(pids, polydata$PID)
  ]
  
  # Clean labels for object names
  labels <- gsub("[^A-Za-z0-9]+", "_", labels)
  labels <- gsub("^_|_$", "", labels)
  
  # Name individual polygon objects
  names(sf_list) <- paste0(prefix, "_", labels)
  
  # Combine all polygons into one sf object
  combined_sf <- dplyr::bind_rows(sf_list)
  
  # Add combined object to list
  sf_list <- c(
    setNames(list(combined_sf), prefix),
    sf_list
  )
  
  return(sf_list)
}

# ============================================================
# 2. Convert polygons
# ============================================================

#####
## PFMC areas

# major PFMC areas
data("major", package = "PBSdata")
major_list <- polyset_to_sf_list(
  major,
  prefix = "pmfc_major"
)

# minor PFMC areas
data("minor", package = "PBSdata")
minor_list <- polyset_to_sf_list(
  minor,
  prefix = "pmfc_minor"
)

#####

# ============================================================
# 3. Inspect output
# ============================================================

#####
# PMFC major

# names of objects
names(major_list)

# inspect polygons
major_list$pmfc_major
major_list$pmfc_major_1

plot(st_geometry(major_list$pmfc_major))
plot(st_geometry(major_list$pmfc_major_1))

#####
# PMFC minor

# names of objects
names(minor_list)

# inspect polygons
minor_list$pmfc_minor
minor_list$pmfc_minor_1

plot(st_geometry(minor_list$pmfc_minor))
plot(st_geometry(minor_list$pmfc_minor_1))


# ============================================================
# 4. Save everything to r package
# ============================================================

#####
# PMFC major

# Save each object individually
for (nm in names(major_list)) {
  
  assign(nm, major_list[[nm]])
  
  eval(
    substitute(
      usethis::use_data(
        OBJ,
        overwrite = TRUE,
        compress = "xz"
      ),
      list(OBJ = as.name(nm))
    )
  )
}


#####
# PMFC minor

# Save each object individually
for (nm in names(minor_list)) {
  
  assign(nm, minor_list[[nm]])
  
  eval(
    substitute(
      usethis::use_data(
        OBJ,
        overwrite = TRUE,
        compress = "xz"
      ),
      list(OBJ = as.name(nm))
    )
  )
}

