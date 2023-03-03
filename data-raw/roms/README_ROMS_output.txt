README_ROMS_output.txt

Files are too big to share through GitHub, so ask Andy or Angelica for them if
you need them.

From Angelica Pena (9/11/22):

Grid file = BCC03_grd_b5.nc
File to mask values at inlet and SoG  = Roms_bcc42_mask.nc
Example of temperature output file = Roms_bcc42_mon_2008to2011_sst.nc

You are guessing right [xi_rho and eta_rho are the physical model
co-ordinates]. The model domain has 236 grid cells in the x-axis and 410 in the
y-axis (1,1 is the left bottom corner of the model and 236,410 the upper right
corner of the domain).

ocean_time - monthly value from 2008 to 2011 (4x12=48);
lon_rho - longitude at the center of each cell
lat_rho - latitude at the center of each cell
mask_rho - land/ocean mask (1=ocean, 0=land)
sst - sea surface temperature at each grid cell every month from 2008 to 2011

--

ROMS_test.R - quick test using RNetCDF package, but ncdf4 seems better.

roms_ncdf4.Rmd - trying ncdf4 package, see HERE. Based on Lindsay Davidson's file:
 RomsExtraction_forAndy.Rmd - from Lindsay

--

base-3km-raster.R from Jessica Nephin:

I have written R code to process the ROMS output using ncdf4 (an example here:
https://gitlab.com/jnephin/futurehabitats-bcroms-grid/-/blob/master/interp-yearly-vars.R). It
doesn’t do anything as fancy as transforming the grid, I think this could be
done using the stars package depending on how Angelica is storing the spatial
information in the netcdf file now, but I haven’t attempted it.

Instead I used the lat/lon positions at the center of each grid cell and just
interpolated them onto a new grid (e.g. Template_Grid_3km/FH-3km-template.tif in
the example). If you were moving to a much larger grid cell (e.g. 15x15km) you
could probably just use a simple bilinear interpolation with resample() but for
smaller grid cell sizes (e.g. 3x3km) this left me with empty grid cells.

base-3km-raster.R

--

Andy's notes:

Looks useful:

https://www.r-bloggers.com/2016/08/a-netcdf-4-in-r-cheatsheet/

Graham Epstein UVic Ecostats Slack channel, tried stars package and would
definitely recommend it.
