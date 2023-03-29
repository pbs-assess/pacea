# pacea

<!-- badges: start -->
[![R-CMD-check](https://github.com/pbs-assess/pacea/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pbs-assess/pacea/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

An R package to house Pacific Region ecosystem data to help operationalise an ecosystem approach to fisheries.

pacea stands for PACific Ecosystem Approach.

The primary audience is stock assessment scientists who want to analyse environmental variables in the context of their stock assessment. The Fisheries Act requires management of fisheries to take into account "[the biology of the fish and the environmental conditions affecting the stock](https://laws-lois.justice.gc.ca/eng/acts/f-14/page-3.html#h-1175547)".  
Such an Ecosystem Approach to Fisheries Management requires data. 

A comprehensive [2022 analysis of Canadian stock assessments](https://publications.gc.ca/collections/collection_2022/mpo-dfo/Fs97-6-3473-eng.pdf) found that availability of data on environmental variables was the leading cause of not integrating such information into assessments. pacea aims to help make data availability more streamlined, and avoid each individual assessment scientist having to figure out themselves where to get appropriate data.

pacea is intended to be a data platform, *not* a primary database of lots of raw data. All data sets include documentation regarding the original authors, who should be cited as appropriate. 

It is not operational yet (some of the data are just example data to set up the infrastructure of the package).

While being an R package, for non-R users we hope to add functionality to extract the data as a simple .csv file. 

Below are some of the notes and references from our original planning meeting (later notes are elsewhere). Work is ongoing.

# Motivation

This work is strongly motivated by, and based on, the [GSLea](https://github.com/duplisea/gslea) R package by Dan Duplisea and colleagues for the Gulf of St Lawrence.

# Notes

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


## Funding

This work is funded by a Competitive Science Research Fund grant.