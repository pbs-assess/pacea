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
##' plot(npi_monthly)  # TODO doesn't work, updated defaults
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
##' @examples
##' \dontrun{
##' pdo
##' plot(pdo)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"pdo"

##' NPGO -- North Pacific Gyre Oscillation
##'
##' The North Pacific Gyre Oscillation (NPGO) is a climate pattern that
##' is significantly correlated with fluctuations of salinity, nutrients, and chlorophyll-a
##' in long-term observations in the California Current (CalCOFI) and Gulf of
##' Alaska (Line P).
##'
##' Adapted from http://www.o3d.org/npgo/
##' The NPGO emerges
##' as the second dominant mode of sea-surface height variability in the
##' Northeast Pacific. The term NPGO is used because fluctuations reflect
##' changes in the intensity of the central and eastern branches of the North
##' Pacific gyre circulations as evident from the NPGO sea-surface height
##' anomalies. Fluctuations in the NPGO are driven by regional and basin-scale
##' variations in wind-driven upwelling and horizontal advection -- the
##' fundamental processes controlling salinity and nutrient
##' concentrations. Nutrient fluctuations drive concomitant changes in
##' phytoplankton concentrations, and may force similar variability in higher
##' trophic levels. The NPGO thus provides a strong indicator of fluctuations in
##' the mechanisms driving planktonic ecosystem dynamics.
##'
##' Key reference is Di Lorenzo et al. (2008): North Pacific Gyre Oscillation
##' links ocean climate and ecosystem change. Geophysical Research Letters, 35:
##' L08607. doi:10.1029/2007GL032838
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{month:}{month (1 to 12) of value}
##'   \item{anomaly:}{calculated as the second dominant mode of sea-surface height
##'   anomaly over the Northeast Pacific (180 deg W to 100 deg W, 25 deg N to 62
##'   deg N), and then maybe converted to standard deviation units as in Figure
##'   1 of Di Lorenzo et al. (2008), though the numbers do not exactly match
##'   that figure (and come from Di Lorenzo's website, which
##'   notes that values after 2004 were updated. This might be why
##'   numbers do not seem to quite agree.}
##' }
##' @examples
##' \dontrun{
##' npgo
##' plot(npgo)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"npgo"

##' ENSO MEI -- Multivariate ENSO Index
##'
##' The Multivariate ENSO (El Niño-Southern Oscillation) Index
##' combines both oceanic and atmospheric variables, to an assessment of ENSO in
##' in a single index.
##'
##' The bi-monthly Multivariate ENSO index (MEI.v2) is the time series of the
##' leading combined Empirical Orthogonal Function (EOF) of five different
##' variables:
##' \itemize{
##'   \item{sea level pressure (SLP)}
##'   \item{sea surface temperature (SST)}
##'   \item{zonal components of the surface wind}
##'   \item{meridional components of the surface wind}
##'   \item{outgoing longwave radiation (OLR)}
##' }
##' These are calculated  over the tropical Pacific basin (30 deg S to 30 deg N and
##' 100 deg E to 70 deg W). The EOFs are calculated for 12 overlapping
##' bi-monthly "seasons" (Dec-Jan, Jan-Feb, Feb-Mar, ..., Nov-Dec) in order to
##' take into account ENSO's seasonality, and reduce effects of higher frequency
##' intraseasonal variability.
##'
##' Key features of composite positive MEI events (warm, El Niño) include:
##' \itemize{
##' \item{anomalously warm SSTs across the east-central equatorial Pacific}
##' \item{anomalously high SLP over Indonesia and the western tropical Pacific}
##' \item{anomalously low SLP over the eastern tropical Pacific}
##' \item{reduction or reversal of tropical Pacific easterly winds (trade winds)}
##' \item{suppressed tropical convection(positive OLR) over Indonesia and
##' the Western Pacific}
##' \item{enhanced convection (negative OLR) over the central Pacific}
##' }
##' Key features of composite negative
##' MEI events (cold, La Niña) are of mostly opposite phase. For any
##' single El Niño or La Niña situation, the atmospheric articulations may
##' depart from this canonical view.
##'
##' The above description is adapted from NOAA's Physical Sciences Laboratory
##' website https://www.psl.noaa.gov/enso/mei/, which has more details and
##' references(and is
##' where the values come from). The data on that website are updated on the
##' 10th of each month (if source data are available). Warm and cold thresholds
##' are considered as being +0.5 (warm) and -0.5 (cold) away from 0. Values are
##' calculated for overlapping two-month `seasons', with, for example, February
##' 2023 in `pacea` representing the January-February calculation.
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{month:}{month (1 to 12) of value, representing the two-month
##'   calculation covering the given month and the preceding month (i.e. month 2
##'   in year 2023 represents the January-February 2023 value)}
##'   \item{anomaly:}{anomalies based on a reference period of 1980-2018}
##'  }
##' @examples
##' \dontrun{
##' mei
##' plot(mei)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"mei"

##' AO - Arctic Oscillation
##'
##' The Arctic Oscillation (AO) is a back-and-forth shifting of atmospheric
##' pressure between the Arctic and the mid-latitudes of the North Pacific and
##' North Atlantic.
##'
##' The most obvious reflection of the phase of the AO is the
##' north-to-south location of the storm-steering, mid-latitude jet
##' stream. Thus, the AO can have a strong influence on weather and climate in
##' major population centers in North America, Europe, and Asia, especially
##' during winter.

##' A strongly positive AO is characterized by lower-than-average air pressure
##' over the Arctic paired with higher-than-average pressure over the northern
##' Pacific and Atlantic Oceans. The jet stream is farther north than average
##' under these conditions, and storms can be shifted northward of their usual
##' paths. Thus, the mid-latitudes of North America, Europe, Siberia, and East
##' Asia generally see fewer cold air outbreaks than usual during the positive
##' phase of the AO.
##'
##' Conversely, AO's negative phase has higher-than-average air pressure over
##' the Arctic region and lower-than-average pressure over the northern Pacific
##' and Atlantic Oceans. The jet stream shifts toward the equator under these
##' conditions, so the globe-encircling river of air is south of its average
##' position. Consequently, locations in the mid-latitudes are more likely to
##' experience outbreaks of frigid, polar air during winters when the AO is
##' negative. In New England, for example, higher frequencies of coastal storms
##' known as "Nor'easters" are linked to AO's negative phase.
##'
##' Above adapted from
##' https://www.climate.gov/news-features/understanding-climate/climate-variability-arctic-oscillation
##'
##' Also see
##' https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/ao.shtml
##' https://www.cpc.ncep.noaa.gov/products/precip/CWlink/daily_ao_index/history/method.shtml
##' https://en.wikipedia.org/wiki/Arctic_oscillation
##'
##' For reference see
##' Thompson, D.W.J., and J.M. Wallace, 2001: Regional Climate Impacts of the
##' Northern Hemisphere Annular Mode. Science, 293, 85-89.
##' and other references on the above websites.
##'
##' Associated code adapted from code generously shared by Chris Rooper.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{month:}{month (1 to 12) of value}
##'   \item{anomaly:}{The AO index is defined using the daily or monthly 1000 hPa
##'   geopotential height anomalies from latitudes 20 deg N to 90 deg N. The anomalies
##'   are projected onto the AO loading pattern, which is
##'   defined as the first empirical orthogonal function of monthly mean
##'   1000 hPa geopotential height during the 1979-2000 period. The time series
##'   is then normalized with the monthly mean index's standard deviation.}
##' }
##' @examples
##' \dontrun{
##' ao
##' plot(ao)
##' plot(ao, smooth_over_year=TRUE)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"ao"

##' ALPI - Aleutian Low Pressure Index
##'
##' The Aleutian Low Pressure Index (ALPI) has been used to describe
##' decadal-scale changes in North Pacific climate-ocean conditions and is linked
##' to patterns in marine productivity.
##'
##' The Aleutian Low atmospheric pressure system is a semi-permanent feature of
##' the North Pacific, generally centered over the Aleutian Islands. The Aleutian
##' Low strengthens in winter, and weakens the following spring; however the relative intensity of low pressures can vary
##' greatly from year to year. The Aleutian Low affects the intensity of winter
##' storms and the direction of atmospheric circulation off the west coast of
##' North America, since the size and position of the Aleutian Low determines
##' the relative waviness of the westerlies.
##'
##' A relatively weak and northward
##' positioned Aleutian Low results in a direct westerly flow in the atmosphere
##' over the North Pacific (i.e. westerly winds).
##'
##' The ALPI was developed at the Pacific Biological Station (Fisheries and
##' Oceans Canada) in 1993, and updated with documented code by Surry and King (2015). It requires the
##' computation of the mean area (in \eqn{10^6 \mbox{km}^2}) in the North Pacific that has a sea level pressure
##' lower than 100.5 kPa in winter months (December to March). The ALPI is computed as the
##' anomaly from a long-term mean area (1950-1997) from a gridded sea surface pressure data
##' obtained from the National Center of Atmospheric Research (Surry and King 2015).
##' Positive ALPI values indicate an intense Aleutian Low relative to the long-term mean.
##'
##' Note that ALPI is currently only available up to 2015, as calculated by
##' Surry and King (2015). If desired the documented R code provided by Surry and King
##' (2015) could be updated (and incorporated into pacea) to enable annual
##' updating of ALPI. Some of the spatial R packages used have since been
##' replaced by newer ones, so updating the code would likely be doable but a bit of work.
##'
##' Much of the above is adapted from:
##'
##' Surry, A.M., and King, J.R (2015). A new method for calculating ALPI: The
##' Aleutian Low Pressure Index. Canadian Technical Report of Fisheries and Aquatic Sciences. 3135: v + 31 p.
##' https://waves-vagues.dfo-mpo.gc.ca/library-bibliotheque/359380.pdf
##'
##' and ALPI's use in
##'
##' Haigh, R., P.J. Starr, A.M. Edwards, J.R. King and J. Lecomte (2019). Stock
##' assessment for Pacific Ocean Perch (Sebastes alutus) in Queen Charlotte
##' Sound, British Columbia in 2017. DFO Canadian Science Advisory Secretariat
##' Research Document 2018/038. v + 227 p.
##' http://www.dfo-mpo.gc.ca/csas-sccs/Publications/ResDocs-DocRech/2018/2018_038-eng.pdf
##'
##' Also see references within Surry and King (2015), plus
##' https://open.canada.ca/data/en/dataset/4bb821ce-bef7-46d3-95d2-064065f1bda4
##' that houses the original values.
##'
##' @format A tibble also of class `pacea_index` with columns:
##' \describe{
##'   \item{year:}{year of value}
##'   \item{anomaly:}{the area of the Aleutian Low is calculated as the mean area of the North Pacific with sea level
##' pressure less than or equal to 100.5 kPa, where the mean is over the four
##' monthly values from December to March (the value for 2015 corresponds to
##' December 2014 to March 2015). The `anomaly` given here is the resulting ALPI
##' calculated as the anomaly of the area of the Aleutian Low from the long-term
##' (1950-1997) mean of \eqn{5.524183 \times 10^6 \mbox{km}^2}. Values are
##'   those in Table 4 of King and Surry (2015), and units of APLI
##' are \eqn{10^6 \mbox{km}^2}.}
##' }
##'
##' @examples
##' \dontrun{
##' alpi
##' plot(alpi)
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"alpi"

##' Summary of climatic and oceanographic indices in pacea
##'
##' The Aleutian Low Pressure Index (ALPI) has been used to describe
##' decadal-scale changes in North Pacific climate-ocean conditions and is linked
##' to patterns in marine productivity.
##'
##' @format A tibble with columns:
##' \describe{
##'   \item{Object:}{name in pacea (use ?<object> for help file}
##'   \item{Description:}{Long name}
##'   \item{Resolution:}{Monthly or annual resolution}
##'   \item{Start year:}{Start year of time series}
##'   \item{End year:}{End year of time series}
##' }
##'
##' @examples
##' \dontrun{
##' pacea_indices
##' knitr::kable(pacea_indices)   # in vignette and README
##' }
##' @author Andrew Edwards
##' @source Generated from running `data-raw/coastwide-indices/coastwide-indices.R`.
"pacea_indices"


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

##' Metadata regarding buoys for sea surface temperature.
##'
##' Details regarding the buoys that are used for the data in
##' `buoy_sst`. Adapted from Andrea Hilborn's code at
##' https://github.com/IOS-OSD-DPG/Pacific_SST_Monitoring/blob/main/scripts/POI_latlon.R
##'
##' @format A tibble with columns:
##' \describe{
##'   \item{wmo_id:}{World Meteorological Organisation weather station id}
##'   \item{name:}{name describing the location}
##'   \item{type:}{type of buoy}
##'   \item{latitude:}{latitude}
##'   \item{longitude:}{longitude}
##'   \item{water_depth_m:}{depth of water (m) in which the buoy resides}
##'   \item{col_key:}{a colour key for plotting each buoy location or data}
##'   \item{stn_id:}{station id, as `C` followed by `wmo_id`, which is how
##'   Environment and Climate Change Canada refers to them, and to be consistent
##'   with Andrea's existing plots}
##'   \item{name_key:}{`station_id` followed by `name` for labelling plots if desired}
##'  }
##' @examples
##' \dontrun{
##' buoy_metadata       # and will get used in plotting code
##' }
##' @author Andrew Edwards and Andrea Hilborn
##' @source Generated from running `data-raw/buoys/buoy-metadata.R`.
"buoy_metadata"

##' Daily average sea surface temperatures for 19 buoys (yielding over 170,000
##' values) calculated with data from Environment and Climate Change Canada and DFO.
##'
##' A tibble of daily average calculations of sea surface temperature in coastal
##' Canadian Pacific waters. The earliest data are from September 1987, and 14 buoys
##' were still providing data as of May 2023. See the example code below to see the
##' start and end dates for each buoy.
##'
##' All buoys are ECCC buoys, with DFO providing the historical and updated data
##' for 17 of them, and ECCC providing data for the other two that started
##' in October 2019 and are related to the Oceans Protection Plan.
##' Data are downloaded from https://data.cioospacific.ca/erddap/ when we
##' (developers) update `pacea` using `data-raw/buoys/buoy-sst.R`. That code
##' does a lot of wrangling of the data (so you don't have to!), such as using
##' some of the flags from Kellog et al. (2021), dealing with timezones,
##' ignoring pesky daylight savings time changes, and averaging over a day
##' (which we will refine, see Issue #26).
##'
##' All data is stored in `pacea`, and will be updated monthly. Data for the
##' two buoys from ECCC seems to be available with a lag of only an hour, whereas it's a
##' few days for the DFO data. If you need data as up-to-date as possible then
##' contact Andrew Edwards and he can update the package. Or do the following if
##' you understand it: clone the `pacea` repo from GitHub, `load_all()`, run
##' `data-raw/buoys/buoy-sst.R`, and you'll have your own up-to-date version of
##' pacea. Some of the DFO-based data are in the ECCC-based data, and so there is overlap.
##' We stuck with the download the DFO data for pacea, though if you really want ECCC data to be used (as
##' it's slightly more up-to-date) we can look at checking the values are the
##' same and switching the workflow to used ECCC.
##'
##' See `buoy_metadata` for details on each buoy.
##'
##' Adapted from Andrea Hilborn's code at
##' https://github.com/IOS-OSD-DPG/Pacific_SST_Monitoring/blob/main/scripts/POI_latlon.R
##'
##' Kellogg, J.P., Rosenstock, N.L, Page, S.J., Hourston, R.A.S., Devred, E., and
##' Hannah, C.G. (2021). Quality Control of Weather/Wave Buoy Temperature Data Provided
##' by Environment and Climate Change
##' Canada. https://drive.google.com/file/d/1J6I8PFuDN0Ca-8wdjfmAWRmeylPGn_s4/view
##'
##' @format A tibble with columns:
##' \describe{
##'   \item{date:}{date (of class `Date`) used to calculate the `sst`, based on
##'   UTC -8 hours (i.e. Pacific Standard Time, not changing due to daylight savings)}
##'   \item{stn_id:}{station identifier, which is the World Meteorolgical
##'   Organisation id number
##'   (https://www.ndbc.noaa.gov/faq/staid.shtml#:~:text=The%20World%20Meteorological%20Organization%20(WMO,identifier%20assigned%20by%20deployment%20location)
##'   preceded by a `C`, which is how ECCC refers to them}
##'   \item{sst:}{calculated mean sea surface for that day at that station
##'   (based on Pacific Standard Time)}
##'  }
##' @examples
##' \dontrun{
##' buoy_sst
##'
##' # Calculate the range of dates for each buoy
##' library(dplyr)
##' buoy_ranges <- buoy_sst %>% group_by(stn_id) %>%
##'   summarise(start = min(date),
##'             end = max(date))
##' buoy_ranges
##' sort(buoy_ranges$start) # This start dates won't change as data are updated
##' # "1987-09-20" "1987-09-22" "1988-08-04" "1988-11-22" "1988-11-22"
##' # "1988-11-22" "1989-09-07" "1989-10-18" "1990-07-12" "1991-04-17"
##' # "1991-05-15" "1991-09-12" "1992-03-13" "1992-10-20" "1993-06-17"
##' # "1994-05-05" "2001-02-19" "2019-10-01" "2019-10-01"
##' sort(buoy_ranges$end)   # This end dates change as data are updated, except for
##'   ones that have ceased recording; these values are as of 2023-06-14 (the
##'   last two are the two for which data come from ECCC):
##' # "2016-12-09" "2020-07-04" "2022-04-17" "2022-05-15" "2022-09-08"
##' # "2023-05-14" "2023-06-11" "2023-06-11" "2023-06-11" "2023-06-11"
##' # "2023-06-11" "2023-06-11" "2023-06-11" "2023-06-11" "2023-06-11"
##' # "2023-06-11" "2023-06-11" "2023-06-14" "2023-06-14"
##'
##' ##' TODO Add plotting function example once written the code.
##' }
##' @author Andrew Edwards and Andrea Hilborn
##' @source Generated from running `data-raw/buoys/buoy-sst.R`.
"buoy_sst"
