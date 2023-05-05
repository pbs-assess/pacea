# pacea

<!-- badges: start -->
[![R-CMD-check](https://github.com/pbs-assess/pacea/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pbs-assess/pacea/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/pbs-assess/pacea/branch/main/graph/badge.svg)](https://app.codecov.io/gh/pbs-assess/pacea?branch=main)
<!-- badges: end -->

An R package to house Pacific Region ecosystem data to help operationalise an ecosystem approach to fisheries management.

pacea stands for PACific Ecosystem Approach. 

pacea is intended to be a data platform containing somewhat disparate data sets (we basically wrangle the data sets behind the scenes to get them into usable consistent formats in R, along with related plotting functions). Is is *not* a primary database of lots of raw data. All data sets include documentation regarding the original authors, who should be cited as appropriate. 

# Installation

We are still developing pacea, and so it is not meant to be fully operational yet. However, to install a version that we know builds:

```
install.packages("remotes")    # If you do not already have the "remotes" package
remotes::install_github("pbs-assess/pacea@51b47d3")
```

Currently, this version includes some climate indices and Pacific Hake stock assessment results, plus other code we are working on, including to use output from a British Columbia ROMS (Regional Ocean Modeling System) model. We do have context-specific plotting functions that relate to each type of data set.

Here's a very brief list of some useful commands for what's currently available (that we will turn into more detailed vignettes):

## Pacific Hake stock assessment results

Type these to see the values, help file, and then a plot:
```
library(pacea)

hake_recruitment              # estimated annual recruitment
?hake_recruitment
plot(hake_recruitment)

# Similar commands work for scaled versions:
hake_recruitment_over_2010    # see for details
?hake_recruitment_over_2010 
plot(hake_recruitment_over_2010)

hake_recruitment_over_R0
?hake_recruitment_over_R0
plot(hake_recruitment_over_R0)

hake_biomass                  # estimated annual spawning stock biomass
?hake_biomass                 
# plot(biomass) not finished yet 
```

## Climatic and oceanographic indices

See `?oni` etc. for descriptions. `plot(oni)` etc. somewhat work but not all default options are sensible yet. See `?plot.pacea_index`. 
```
oni
pdo
soi
npi_monthly
npi_annual 
```

# Audience and motivation

The primary audience is stock assessment scientists who want to analyse environmental variables in the context of their stock assessment. The Fisheries Act requires management of fisheries to take into account "[the biology of the fish and the environmental conditions affecting the stock](https://laws-lois.justice.gc.ca/eng/acts/f-14/page-3.html#h-1175547)". Such an Ecosystem Approach to Fisheries Management requires data. 

A comprehensive [2022 analysis of Canadian stock assessments](https://publications.gc.ca/collections/collection_2022/mpo-dfo/Fs97-6-3473-eng.pdf) found that availability of data on environmental variables was the leading cause of not integrating such information into assessments. pacea aims to help make data availability more streamlined, and avoid each individual assessment scientist having to figure out themselves where to get appropriate data.

This work is strongly motivated by, and based on, the [GSLea](https://github.com/duplisea/gslea) R package by Dan Duplisea and colleagues for the Gulf of St Lawrence.

# Rough notes for developers

Below are some rough notes and references from our original planning meeting, so just ignore these. Work is ongoing.

## Geographic area - rough ideas

GSLea has 9 or so Ecosystem Approach Regions. We had some preliminary discussion of how we would define such regions for Pacific Region waters. One point - is there a reason to have them mutually exclusive (i.e. can allow overlapping regions)?

Could we have user-defined polygons? Seems unlikely as would require a lot of computation for some data sets.  

Perhaps we can have various spatial scales, with smaller spatial scales being able to be joined to form larger ones if necessary. Possible Goal: Find out what spatial scales on which indices are useful/required for which species.

Pacific Bioregions (too large-scale for our purposes) are shown [here](https://cpawsbc.org/northern-shelf-bioregion/).

## Data ideas

Charles: [SST and Chl-a summaries](https://bio-rsg.github.io/).

Several indices were extracted and analysed in Appendix F of a [Pacific Ocean Perch assessment](https://waves-vagues.dfo-mpo.gc.ca/Library/40803569.pdf). See that for links to websites. In the assessment we used:

- East-Pacific/North-Pacific index
- Pressure-adjusted sea level anomalies at Prince Rupert
- Standardized maximum area covered by Haida eddies
- Aleutian Low Pressure Index
- Standardized North Pacific Index
- Pacific Decadal Oscillation
- North Pacific Gyre Oscillation
- Oceanic Niño Index
- Southern Oscillation Index.
- Aleutian low pressure index


Following paper discusses how the Strait of Georgia transitions to Queen Charlotte Sound through Johnstone Strait. 
It really makes the point that the separation is abrupt not gradual. And thus that somewhere about Chatham Point is a reasonable place to start the Northern Shelf Bioregion (and thus the marine component of the great bear rainforest):
Dosser, H.V., Waterman, S., Jackson, J.M., Hannah, C.G., Evans, W. and Hunt, B.P.V., [Stark physical and biogeochemical differences and implications for ecosystem stressors in the Northeast Pacific coastal ocean](https://agupubs.onlinelibrary.wiley.com/doi/abs/10.1029/2020JC017033
). Journal of Geophysical Research: Oceans, p.e2020JC017033.


## Other thoughts stemming from 13/10/21 planning meeting

GSLea - some people wanted stock assessment output to be included in the package, some were strongly against.

Could be good to have uncertainties associated with these indices, where possible.

Main structure in GSLea is simply the four headings:

| Year | Ecosystem Approach Region | Variable | Value |

## Notes for developers

Lower case as much as possible, with underscores.

Document everything.

Add unit tests as much as possible (though not for functions that might not end up being used).

Add examples in functions, and vignettes (once we've finalised some aspects).

## Funding

This work is funded by a Competitive Science Research Fund grant.