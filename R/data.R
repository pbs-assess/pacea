##' ONI -- Oceanographic Niño Index
##'
##' The Oceanic Niño Index is a monthly index which is one measure of the El
##' Niño-Southern Oscillation.
##'
##' The Oceanic Niño Index (ONI) is a 3-month running
##' mean of sea surface temperature (SST) anomalies in the Niño 3.4 region
##' (5 deg N to 5 deg S, 120 deg W to 170 deg W) plotted on
##' the center month. The SST anomalies are calculated based on 30-year base
##' periods that are updated every 5 years, which accounts for global warming
##' and some of the decadal-scale SST variability (as seen in the Pacific
##' Decadal Oscillation index).
##' The ONI is provided by the NOAA’s National Weather
##' Service National Centers for Environmental Prediction CPC:
##' http://www.cpc.ncep.noaa.gov/products/analysis_monitoring/ensostuff/ensoyears.shtml
##'
##' Preceding paragraph adapted from: Ross, T., and Robert, M. (2022). Normal
##' temperatures despite strong cool
##' climate indices and an emerging freshening trend. Pages 23-30 of
##' Boldt, J.L., Joyce, E., Tucker, S., and Gauthier, S. (Eds.). 2022. State of
##' the physical, biological and selected fishery resources of Pacific Canadian
##' marine ecosystems in 2021. Canadian Technical Report of Fisheries and
##' Aquatic Sciences. 3482 vii+242 p.
##'
##' The above website is updated automatically on the first Thursday of each
##' month, and states that:
##'
##' Because of the high frequency filter applied to the
##' ERSSTv5 data, ONI values may change up to two months after
##' the initial "real time" value is posted. Therefore, the most recent ONI
##' values should be considered an estimate.
##' On the site, Warm (red) and cold (blue) periods are based on a threshold of
##' +/- 0.5 deg C for the Oceanic Niño Index (ONI) [3 month running mean of
##' ERSST.v5 SST anomalies in the Niño 3.4 region (5 deg N-5 deg S, 120 deg W
##' -170 deg W)], based on centered 30-year base periods updated every 5 years.
##' For historical purposes, periods of below and above normal SSTs are colored in
##' blue and red (on the website) when the threshold is met for a minimum of 5
##' consecutive overlapping seasons. The ONI is one measure of the El
##' Niño-Southern Oscillation, and other indices can confirm whether features
##' consistent with a coupled ocean-atmosphere phenomenon accompanied these periods.
##'
##' Also see https://www.ncei.noaa.gov/access/monitoring/enso/sst
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{month:}{month (1 to 12) of value}
##'   \item{val:}{absolute values of three-month averages (preceding, current, and
##'    next month), deg C}
##'   \item{anom:}{anomalies based on 30-year base periods that are updated every
##'   5 years, deg C}
##'  }
##' @examples
##' \dontrun{
##' oni
##' plot(oni)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"oni"

##' NPI -- North Pacific Index monthly values
##'
##' The North Pacific index measures interannual to decadal
##' variations in atmospheric circulation across the North Pacific.
##'
##' The North Pacific Index (NPI) is the area-weighted sea level
##' pressure over the region 30 deg N to 65 deg N, 160 deg E to 140 deg W. The
##' dominant atmosphere-ocean relation in the North Pacific is one where
##' atmospheric changes lead changes in sea surface temperatures (SST) by one to two
##' months. However, strong ties exist with events in the tropical Pacific, with
##' changes in tropical Pacific SSTs leading SSTs in the north Pacific by three
##' months. Taken from
##' https://climatedataguide.ucar.edu/climate-data/north-pacific-np-index-trenberth-and-hurrell-monthly-and-winter
##' Key reference is Trenberth and Hurrell (1994): Decadal atmosphere-ocean
##' variations in the Pacific, Climate Dynamics 9:303-319.
##'
##' `npi_monthly` contains monthly absolute values. Annual average winter values
##' and anomalies are in `npi_annual`.
##' Ross and Robert (2022; see `?oni` for reference): The NPI is a useful
##' indicator of the intensity and real extent of the Aleutian Low Pressure
##' system. The NPI was generally positive from 1950 to 1976, and generally
##' negative (red) from 1977 to 2008; a change than can be attributed to the
##' strengthening of the Aleutian Low Pressure system after 1977. From 2008 to
##' present [2021], the NPI was mostly positive, due to weaker Aleutian
##' Lows. The NPI anomaly, plotted in Figure 7-2 [of Ross and Robert (2022)],
##' was calculated from the NPI by removing the 1950-2018 mean.
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{month:}{month (1 to 12) of value}
##'   \item{val:}{absolute monthly value (average?), hPa, with an `NA` for
##'   December 1944}
##'  }
##'
##' @examples
##' \dontrun{
##' npi_monthly
##' plot(npi_monthly)
##' }
##'
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"npi_monthly"

##' NPI -- North Pacific Index annual values and anomalies
##'
##' The North Pacific index measures interannual to decadal
##' variations in atmospheric circulation across the North Pacific.
##'
##' See `?npi_monthly` for details.
##'
##' `npi_annual` contains annual average winter values and anomalies.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{val:}{absolute average winter value, hPa. The value for year $N$
##'   refers to an average of the values in November  $N-1$, December $N-1$, and
##'   January, February, and March for year $N$. For example, the 1999 annual
##'   value contains the average of November and December 1998 and January,
##'   February, and March 1999. Hence there is an `NA` for 1899 as no monthly
##'   data for 1898.}
##'   \item{anom:}{anomalies of the annual winter values compared to the 1925-1989 mean
##'   of 1008.9 hPa, again with an `NA` for 1899 TODO check.}
##' }
##' @examples
##' \dontrun{
##' npi_annual
##' plot(npi_annual)
##' }
##'
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"npi_annual"

##' PDO -- Pacific Decadal Oscillation
##'
##' The Pacific Decadal Oscillation is a monthly index which is a long-lived El
##' Niño-like pattern of Pacific climate variability.
##'
##' Adapted from http://research.jisao.washington.edu/pdo/
##' The Pacific Decadal Oscillation (PDO) is a long-lived El Niño-like pattern
##' of Pacific climate variability. While the two climate oscillations have
##' similar spatial climate fingerprints, they have very different behavior in
##' time. Fisheries scientist Steven Hare coined the term PDO
##' in 1996 while researching connections between Alaska
##' salmon production cycles and Pacific climate.
##' Two main characteristics distinguish PDO from El
##' Niño/Southern Oscillation (ENSO): first, 20th century PDO "events" persisted
##' for 20-to-30 years, while typical ENSO events persisted for 6 to 18 months;
##' second, the climatic fingerprints of the PDO are most visible in the North
##' Pacific/North American sector, while secondary signatures exist in the
##' tropics, whereas the opposite is true for ENSO. Several independent studies find
##' evidence for just two full PDO cycles in the past century: "cool" PDO
##' regimes prevailed from 1890-1924 and again from 1947-1976, while "warm" PDO
##' regimes dominated from 1925-1946 and from 1977 through (at least) the
##' mid-1990's. Shoshiro Minobe  has shown that 20th century PDO fluctuations
##' were most energetic in two general periodicities, one from 15-to-25 years,
##' and the other from 50-to-70 years.
##'
##' Major changes in northeast Pacific marine ecosystems have been correlated
##' with phase changes in the PDO; warm eras have seen enhanced coastal ocean
##' biological productivity in Alaska and inhibited productivity off the west
##' coast of the contiguous United States, while cold PDO eras have seen the
##' opposite north-south pattern of marine ecosystem productivity.
##'
##' Causes for the PDO are not currently known. Likewise, the potential
##' predictability for this climate oscillation are not known. Some climate
##' simulation models produce PDO-like oscillations, although often for different
##' reasons. The mechanisms giving rise to PDO will determine whether skillful
##' decades-long PDO climate predictions are possible. For example, if PDO arises
##' from air-sea interactions that require 10 year ocean adjustment times, then
##' aspects of the phenomenon will (in theory) be predictable at lead times of up to
##' 10 years. Even in the absence of a theoretical understanding, PDO climate
##' information improves season-to-season and year-to-year climate forecasts for
##' North America because of its strong tendency for multi-season and multi-year
##' persistence. From a societal impacts perspective, recognition of PDO is
##' important because it shows that "normal" climate conditions can vary over time
##' periods comparable to the length of a human's lifetime .
##'
##' https://www.ncei.noaa.gov/access/monitoring/pdo/
##' The NCEI (NOAA's National Centers for Environmental Information) PDO index
##' (used in pacea) is based on NOAA's extended reconstruction of
##' SSTs (ERSST Version 5). It is constructed by regressing the ERSST anomalies
##' against the Mantua PDO index for their overlap period, to compute a PDO
##' regression map for the North Pacific ERSST anomalies. The ERSST anomalies
##' are then projected onto that map to compute the NCEI index. The NCEI PDO
##' index closely follows the Mantua PDO index.
##'
##' Good explanation (and see figures) from https://www.worldclimateservice.com/2021/09/01/pacific-decadal-oscillation/
##' The Pacific Decadal Oscillation is a sea surface temperature (SST) climate
##' cycle (or teleconnection) describing sea surface temperature anomalies over
##' the Northeastern Pacific Ocean. The PDO can influence the weather conditions
##' across North America with characteristic patterns occurring at different
##' times of the year.
##' The PDO oscillates between positive and negative phases. The positive phase
##' is characterized by cool SSTs north of Hawaii and warmer than normal sea
##' surface temperatures along the western coast of North America (their Figure
##' 1). The negative phase is a mirror image with warm surface waters in the
##' Central North Pacific and cooler than normal waters along the western coast
##' of North America (their Figure 2).
##'
##' Also, from https://en.wikipedia.org/wiki/Pacific_decadal_oscillation :
##' Over the past century, the amplitude of this climate pattern has varied
##' irregularly at interannual-to-interdecadal time scales (meaning time periods
##' of a few years to as much as time periods of multiple decades). There is
##' evidence of reversals in the prevailing polarity (meaning changes in cool
##' surface waters versus warm surface waters within the region) of the
##' oscillation occurring around 1925, 1947, and 1977; the last two reversals
##' corresponded with dramatic shifts in salmon production regimes in the North
##' Pacific Ocean. This climate pattern also affects coastal sea and continental
##' surface air temperatures from Alaska to California. A PDO 'signal' has been
##' reconstructed as far back as 1661 through tree-ring chronologies in the Baja
##' California area.
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{month:}{month (1 to 12) of value}
##'   \item{anom:}{anomaly calculated as the leading empirical orthogonal
##'   function of monthly sea surface temperature anomalies over the North
##'   Pacific (poleward of 20 deg N) after the global average sea surface
##'   temperature has been removed.}
##' }
##'
##' \dontrun{
##' pdo
##' plot(pdo)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"pdo"

##' Pacific Hake annual age-0 recruitments as estimated by the 2023 stock
##' assessment.
##'
##' The Pacific Hake stock is managed and assessed through an Agreement between
##' Canada and the United States. The recruitment estimates come from the most
##' recent joint stock assessment, and, importantly, are for the coastwide stock
##' from California to British Columbia.
##'
##' Pacific Hake are a migratory species, generally spawning off southern
##' California in the winter spawning season and migrating to coastal areas
##' between northern California and northern British Columbia during the rest of
##' the year. The stock tends to move farther to the north in the summer during
##' warmer years compared to cool years.
##'
##' The stock is important to ecosystem dynamics in the Eastern
##' Pacific Ocean due to its relatively large total biomass and potentially large
##' role as both prey and predator. It has highly variable recruitment, as seen
##' by typing `plot(hake_recruitment)`.
##'
##' The stock is assessed as a single migratory stock, so it is important to
##' realise (in the context of analyses for Canadian waters) that these estimates are for the
##' full coastwide stock. Results from the biannual coastwide acoustic survey show how the
##' biomass of age-2 and older fish in Canadian waters can vary between years
##' (Figure 2 of most recent assessment).
##'
##' The `hake_recruitment` (and `hake_biomass`) estimates are from a Bayesian statistical catch-at-age
##' model, that is fit to the acoustic survey index of biomass, an index of
##' age-1 fish from the survey, annual commercial catch-at-age data, and
##' age-composition data from the survey and commercial fisheries.
##'
##' Note that recruitments for current and recent years have little data to
##' estimate them, and so can essentially come from the assumed distribution of
##' recruitments. They should therefore likely be excluded from any analyses, as
##' they are not based on data. Exactly how many years are affected can depend
##' upon the timing of the assessment relative to the most recent biannual
##' survey.
##'
##' Results are from the most recent stock assessment:
##'
##' Berger, A.M., C.J. Grandin, K.F. Johnson and A.M. Edwards. 2023. Status of
##' the Pacific Hake (whiting) stock in U.S. and Canadian waters in
##' 2023. Prepared by the Joint Technical Committee of the U.S. and Canada
##' Pacific Hake/Whiting Agreement, National Marine Fisheries Service and
##' Fisheries and Oceans Canada. 208 p.
##' https://media.fisheries.noaa.gov/2023-02/2023-hake-assessment-post-srg_web.pdf
##'
##' @format A tibble also of class `pacea_recruitment` with columns:
##' \describe{
##'   \item{year:}{year of the recruitment estimate}
##'   \item{low:}{low end (2.5th percentile) of the 95\% credible interval for recruitment}
##'   \item{median:}{median estimate of recruitment}
##'   \item{low:}{high end (97.5th percentile) of the 95\% credible interval for recruitment}
##'  }
##'
##' @examples
##' \dontrun{
##' hake_recruitment
##' plot(hake_recruitment)
##' }
##' @author Andrew Edwards
##' @source Generated from Andy running (in the hake repository)
##'   `hake-assessment/sandbox/andy/pacea-save/pacea-save.R` and then here
##'   `data-raw/groundfish/hake.R`.
"hake_recruitment"

##' Pacific Hake annual age-0 spawning stock biomass as estimated by the 2023 stock
##' assessment.
##'
##' The Pacific Hake stock is managed and assessed through an Agreement between
##' Canada and the United States. The spawning stock biomass (mature females) estimates come from the most
##' recent joint stock assessment, and, importantly, are for the coastwide stock
##' from California to British Columbia.
##'
##' See `?hake_recruitment` for further details and reference.
##'
##' @format A tibble also of class `pacea_biomass` with columns:
##' \describe{
##'   \item{year:}{year of the estimate of spawning stock biomass (mature females)}
##'   \item{low:}{low end (2.5th percentile) of the 95\% credible interval for biomass}
##'   \item{median:}{median estimate of biomass}
##'   \item{low:}{high end (97.5th percentile) of the 95\% credible interval for biomss}
##'  }
##'
##' @examples
##' \dontrun{
##' hake_biomass
##' plot(hake_biomass)
##' }
##' @author Andrew Edwards
##' @source Generated from Andy running (in the hake repository)
##'   `hake-assessment/sandbox/andy/pacea-save/pacea-save.R` and then here
##'   `data-raw/groundfish/hake.R`.
"hake_biomass"


##' Pacific Hake annual age-0 recruitments divided by recruitments in 2010, as estimated by the 2023 stock assessment.
##'
##' This is calculated to improve understanding when comparing recruitments
##' between years. A survey of participants in the stock assessment process
##' found that all but one respondent correctly inferred from the standard plot
##' `plot(hake_recruitment)` that the 2014 recruitment has zero probability of
##' being as large as the (well-known large) 2010 recruitment.
##'
##' For each Markov chain Monte Carlo (MCMC) sample the recruitment in a year is
##' divided by the recruitment in 2010, and then median and credible intervals
##' are calculated for each year across all samples.
##'
##' For more details see Appendix H of the 2022 assessment at
##' https://media.fisheries.noaa.gov/2022-02/2022-hake-assessment-post-srg.pdf.
##' Figure 31 of the 2023 assessment shows the 2023 results.
##'
##' See `?hake_recruitment` for further details and reference regarding hake.
##'
##' @format A tibble also of class `pacea_recruitment` with columns:
##' \describe{
##'   \item{year:}{year of the estimate of scaled recruitment}
##'   \item{low:}{low end (2.5th percentile) of the 95\% credible interval for
##'   the scaled recruitment}
##'   \item{median:}{median estimate of scaled recruitment}
##'   \item{low:}{high end (97.5th percentile) of the 95\% credible interval for
##'   the scaled recruitment}
##'  }
##'
##' @examples
##' \dontrun{
##' hake_recruitment_over_2010
##' plot(hake_recruitment_over_2010)  # the code automatically plots in red and
##'   adds the line at 1
##' }
##' @author Andrew Edwards
##' @source Generated from Andy running (in the hake repository)
##'   `hake-assessment/sandbox/andy/pacea-save/pacea-save.R` and then here
##'   `data-raw/groundfish/hake.R`.
"hake_recruitment_over_2010"

##' Pacific Hake annual age-0 recruitments divided by unfished
##' equilibrium recruitment, as estimated by the 2023 stock assessment.
##'
##' This is calculated to improve understanding when comparing recruitments
##' between years, and somewhat scales out the uncertainty in the unfished
##' equilibrium recruitment ($R_0$).
##'
##' See `?hake_recruitment_over_2010` and `?hake_recruitment` for further details and reference regarding hake.
##'
##' @format A tibble also of class `pacea_recruitment` with columns:
##' \describe{
##'   \item{year:}{year of the estimate of scaled recruitment}
##'   \item{low:}{low end (2.5th percentile) of the 95\% credible interval for
##'   the scaled recruitment}
##'   \item{median:}{median estimate of scaled recruitment}
##'   \item{low:}{high end (97.5th percentile) of the 95\% credible interval for
##'   the scaled recruitment}
##'  }
##'
##' @examples
##' \dontrun{
##' hake_recruitment_over_R0
##' plot(hake_recruitment_over_R0)  # the code automatically plots in red and
##'   adds the line at 1
##' }
##' @author Andrew Edwards
##' @source Generated from Andy running (in the hake repository)
##'   `hake-assessment/sandbox/andy/pacea-save/pacea-save.R` and then here
##'   `data-raw/groundfish/hake.R`.
"hake_recruitment_over_R0"

#' BC coastline
#'
#' A simple features object of the BC coastline 
#' 
#' Extent of BC coast includes the BC coast and Pacific portion of Canada's Exclusive Economic Zone, and any neighbouring land (e.g. US States). 
#' 
#' Source of initial bc_coast geometry shapes were loaded from 'rnaturalearth::ne_countries' at a 1:10M scale.
#' 
#' @format A simple features multipolygon dataframe.
#' @examples 
#' \dontrun{
#' bc_coast
#' plot(bc_coast)
#' }
#' 
#' @source Generated from running `data-raw/coastline/coastline-eez.R`.
"bc_coast"

#' BC Exclusive Economic Zone (EEZ)
#'
#' A simple features object of the BC EEZ 
#' 
#' Source of BC EEZ is from 'PBSdata'.
#' 
#' @format A simple features polygon dataframe.
#' @examples 
#' \dontrun{
#' bc_eez
#' plot(bc_eez)
#' }
#' 
#' @source Generated from running `data-raw/coastline/coastline-eez.R`.
"bc_eez"

#' Polygons masking layers for BC waters
#' 
#' Simple features objects of ROMs data, inshore, and offshore regions of BC Exclusive Economic Zone (EEZ) waters.
#' 
#' Bathymetric classifications were used to derive a line to separate continental shelf (inshore) and deep sea regions. Includes a 10km buffer around the EEZ and limited by ROMs data coverage.
#' 
#' Source of bathymetric classifications:
#' GIS hub Pacific Marine Habitat Classes - obtained from: https://www.gis-hub.ca/dataset/marine-habitat-classes
#' 
#' @format A simple features polygon dataframe.
#' \describe{
#'   \item{romseez_poly}{BC EEZ boundary witha 10km buffer and clipped with ROMs data output boundary}
#'   \item{inshore_poly}{inshore (continental shelf) region of romseez_poly}
#'   \item{offshore_poly}{offshore (deep seafloor) region of roms eez_poly}
#' }
#' @examples 
#' \dontrun{
#' romseez_poly
#' inshore_poly
#' offshore_poly
#' plot(romseez_poly)
#' plot(inshore_poly)
#' plot(offshore_poly)
#' }
#' 
#' @source Generated from running `data-raw/grids/make-mask-layer.R`.
"romseez_poly"

#' @rdname romseez_poly
"inshore_poly"

#' @rdname romseez_poly
"offshore_poly"
