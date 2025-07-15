Users: `pacea_installed` will show the date that you last installed `pacea`.
Compare to the dates below to see if you wish to update the package.

Developers: for some general tips for writing good bullets see https://style.tidyverse.org/news.html

## Updates by date

* 2025-07-15 New data: Fraser River discharge monthly means
  `fraser_discharge_mean` and peaks `fraser_discharge_peak` from 1912
  onwards, with new help files, tests, and vignettes. Also now preserve/check
  attributes (issue #87).

* 2025-05-26 Update buoy data (665 new daily temperature calculations), and indices `ao`, `mei`
  `npi_annual`, `npi_monthly`, `pdo` and `pacea_indices` since several now extend
  into 2025.

* 2025-04-04 Update indices `ao`, `mei`, `pdo`, `npgo`, and `soi`, and
  `buoy_sst`. Had some warnings when converting dates for `buoy_sst` that may or
  may no have been occurring before, so let me know if you spot anything strange.

* 2025-02-25 Update indices `ao`, `mei`, `oni`, `pdo`, and `soi`, some now up to
  2025.

* 2025-02-05 Monthly updates for indices `ao`, `oni`, `pdo`, `soi`. Monthly
  updates for `oisst_7day` and `oisst_month` (2024 complete).

* 2024-12-04 Added Pacific Hake age-1 total biomass and recruitment deviations,
  and necessarily updated several plotting and tickmark functions (should all be back
  compatible).

* 2024-11-25 New oceanographic index: `bi`, the North Pacific Current
  Bifurcation Index. See README and the indices vignette.

* 2024-11-21 Monthly updates for `oisst_7day`, `oisst_month`, `buoy_sst` (2725
  new daily means), and indices `ao`, `mei`, `oni`, and `pdo`. And 'tis the season.

* 2024-11-12 Major update:
 - added results from the Hindcast of the Salish Sea (HOTSSea) physical
 oceanography model;
 - added results from the BCCM model over its full original domain (not just
 Canadian waters);
 - added depths for the `grid26` model domain;
 - updated citation info to add Greig Oldford.

* 2024-09-25 Update citation info (increment year to 2024, add Kelsey Flynn as
  co-author) and obtain a DOI. See README or run `citation("pacea")`.

* 2024-09-20 Monthly updates for: `buoy_sst`, and indices `ao`, `mei`, `npgo`, `oni`, `pdo`, and `soi`.

* 2024-08-21 Monthly update for: `oisst_7day` and `oisst_month`.

* 2024-08-06 Monthly update for: `oisst_7day`, `oisst_month` (both updated two
  weeks earlier), `buoy_sst`, and indices `ao`, `oni`, `pdo`, and `soi`.

* 2024-06-10 Monthly update for May: `oisst_7day`, `oisst_month` (both updated 2024-05-20), `buoy_sst`, and indices `ao`, `oni`, `pdo`, and `soi`.

* 2024-05-08 Added Pacific Herring stock assessment results, including new
  plotting functions (see `populations` vignette linked from README).

* 2024-05-07 Add helper function `a()`, shorthand for `as.data.frame()`.

* 2024-04-22 Monthly update: `buoy_sst`, and indices `ao`, `mei`, `npi_monthly`,
  `npi_annual`, `oni`, `pdo`, and `soi`. NOTE: NOAA again recalculated the whole
  time series for the MEI; see `?mei`.

* 2024-04-17 Update `alpi` with values from 2016 to 2022.

* 2024-04-12 New time series: zooplankton biomass anomalies for the Strait of Georgia,
  includes new functions and vignette.

* 2024-04-02 Update hake variables with those from 2024 assessment; save 2023
  assessment values as `hake_recruitment_2023` etc.; see the updated help files
  and Populations vignette.

* 2024-03-26 Monthly update: `oisst_7day`, `oisst_month` (both updated 2024-03-20), `buoy_sst`, and indices `ao`, `oni`, `pdo`, and `soi`.

* 2024-02-20 Monthly update: `buoy_sst` and indices `ao`, `mei`, `oni`, `pdo`, and
  `soi`.

* 2024-01-29 Monthly update: `oisst_7day`, `oisst_month`, `buoy_sst`, and
  indices `ao`, `mei`, `npgo`, `pdo`, and `soi`. NOTE: some historical values
  of `pdo` (e.g. Nov 2015) changed. 2023 data inadvertently got omitted from
  the OISST data and will be corrected shortly.

* 2023-12-21 Monthly update: `oisst_7day`, `oisst_month`, `buoy_sst`, and indices `ao`, `mei`, `oni`, and `pdo`. NOTE: NOAA recalculated the whole time series for the MEI; see `?mei`.

* 2023-11-20 `buoy_sst` data updated.

* 2023-11-10 Package version 1.0.0 launched.
