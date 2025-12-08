Andy experimenting with using the code from marea to extract GLORYS results.

Copying several files over from marea (8/12/08) into
from-marea/
but only glorys-bottom-temperature.R is on GitHub, so going to start with that
as Emily had written it.

These were in R-temp but moved into:
from-marea-scoping-meeting/
as presumably they came from marea workshop:
Download_Copernicus_Marine_Data.r
glorys_helper_functions.r

So Andy starting:
glorys-mld-for-hake.r  - adapting Emily's glorys-bottom-temperature.R to extract
mixed layer depth for hake. Specifically try MLD_late_larv, which is the mean
mixed-layer deth along the shfl break, Mar-Jun, 31-37degN, and in 2025
assessment Figure H.2 actually had large positive values corresponding to 1999,
2010, and 2021 recuirment (also had some large-ish values that didn't give big
recruitment, but seems worth picking).

Idea is to use this example to get going (need for 2026 assessment anyway), then hopefully generalise. 

