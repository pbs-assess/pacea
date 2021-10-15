# PACea
An R package to house Pacific Region ecosystem data to help facilitate an ecosystem approach to fisheries. 

While being an R package, for non-R users it will allow for extraction of all the data as a simple .csv file. It is intended to be a data platform, *not* a primary database of lots of raw data.

For now, just adding some notes and references from our first planning meeting, while we scope out this project.

# Motivation

The Fisheries Act requires management of fisheries to take into account "[the biology of the fish and the environmental conditions affecting the stock](https://laws-lois.justice.gc.ca/eng/acts/f-14/page-3.html#h-1175547)".  
Such an Ecosystem Approach to Fisheries Management requires data. This package will help Pacific Region stock assessment scientists by housing usable broad-scale data in one place, so each person does not have to go looking for it themselves. 

This work is strongly motivated by, and based on, the [GSLea](https://github.com/duplisea/gslea) R package by Dan Duplisea and colleagues for the Gulf of St Lawrence.

# Notes

## Geographic area - rough ideas

GSLea has 9 or so Ecosystem Approach Regions. We had some preliminary discussion of how we would define such regions for Pacific Region waters. One point - is there a reason to have them mutually exclusive (i.e. can allow overlapping regions)?

Could we have user-defined polygons? Seems unlikely as would require a lot of computation for some data sets.  

Perhaps we can have various spatial scales, with smaller spatial scales being able to be joined to form larger ones if necessary. Possible Goal: Find out what spatial scales on which indices are useful/required for which species.

Pacific Bioregions (too large-scale for our purposes) are shown [here](https://cpawsbc.org/northern-shelf-bioregion/).

## Data ideas

Charles: [SST and Chl-a summaries](https://bio-rsg.github.io/).

Chris Rooper sent us some useful code for automatic extraction from websites, which we will convert into functions - look at that first. 

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