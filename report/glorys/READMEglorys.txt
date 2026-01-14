Andy experimenting with using the code from marea to extract GLORYS results.

Copying several files over from marea (8/12/08) into
from-marea/
but only glorys-bottom-temperature.R and R/get_CMEMS_ncdf.R are on GitHub, so
going to start with the first as Emily had written it.

These were in R-temp but moved into:
from-marea-scoping-meeting/
as presumably they came from marea workshop:
Download_Copernicus_Marine_Data.r
glorys_helper_functions.r


So Andy was going to do this (didn't do, as then got stuff from Megan, see below):
glorys-mld-for-hake.r  - adapting Emily's glorys-bottom-temperature.R to extract
mixed layer depth for hake. Specifically try MLD_late_larv, which is the mean
mixed-layer deth along the shfl break, Mar-Jun, 31-37degN, and in 2025
assessment Figure H.2 actually had large positive values corresponding to 1999,
2010, and 2021 recuirment (also had some large-ish values that didn't give big
recruitment, but seems worth picking).

Idea is to use this example to get going (need for 2026 assessment anyway), then hopefully generalise. 

For future users:
Need the Copernicus Marine Toolbox
 https://help.marine.copernicus.eu/en/articles/7949409-copernicus-marine-toolbox-introduction


from-noaa-folks/
****************

glorys_examples_for_andy_isobathprocessing.qmd - going through this
first. Running code line by line then build just using R Markdown I reckon.

glorys_examples_for_andy_isobathprocessing-ame.Rmd  - andy editing, changed to .Rmd

Emails from Kristin, Megan and Eric, in reverse order.

Hi Andy,

I know this is on the shelf for a bit for you but I added the last piece of the workflow to Eric's code. Attached is the bathymetry file (databathymetry-m.csv) needed for the isobath part of summarising the time series. I added a quick example with MLDyolk to the end of Eric's code to give you an idea of how you implement it for that variable in the updated .qmd (glorys_examples_for_andy_isobathprocessing.qmd). I also did a quick check on the old variable I have from last year (DATA_Combined_glorys_hake_UNSTANDARDIZED.csv).

Hopefully this gives you the full picture of the downloading through processing workflow! 

Megan

On Wed, Dec 10, 2025 at 3:58 PM Edwards, Andrew (DFO/MPO) <Andrew.Edwards@dfo-mpo.gc.ca> wrote:
Unclassified - Non-Classifié

Hi all,
 
Thanks very much Megan for offering help – those explanations are very useful.
 
Thanks Eric for the code, and Kristin for getting the ball rolling.
 
I do a quick look the other day and did find the “R via CopernicusMarine” route, which I think is probably the most straightforward; the reticulate python approach did freeze my R. 
 
So I’ll probably try Eric’s code first, and expect to have questions for him and Megan. Realistically I might not have time before the holidays so apologies if you don’t hear from me, but these emails are super helpful.
 
Cheers,
 
Andy
 
From: Eric Ward - NOAA Federal <eric.ward@noaa.gov> 
Sent: Tuesday, December 9, 2025 5:44 PM
To: Megan Feddern - NOAA Federal <megan.feddern@noaa.gov>
Cc: Kristin Marshall - NOAA Federal <kristin.marshall@noaa.gov>; Mary Hunsicker - NOAA Federal <mary.hunsicker@noaa.gov>; Edwards, Andrew (DFO/MPO) <Andrew.Edwards@dfo-mpo.gc.ca>
Subject: Re: workflow for updating GLORYS indicators for hake
 
Hey hey Andy, happy to help! I have a few implementations of this, and am attaching 3 examples of ways you can access the data (I assume you've already registered on Copernicus) 
- R via CopernicusMarine
- R via reticulate
- Python
 
I think a couple caveats are that the dataset id will change depending on what layer you're interested in extracting -- but should be pretty straightforward via their data viewer (let me know if it's not). Second, the global reanalysis product ends ~ August 2024, and the global re-analysis and forecast product spans ~ 2022 to the present, so there's some decisions that you'll need to make about joining. In these examples, I'm querying everything in the WCGBTS survey grid, and then I filter out cells not in the survey domain -- a third thing to note is that some of the different data products have different resolution (temp is on a 1/12 degree grid, but I think the biogeochemistry is coarser)
 
Lemme know if you run into roadblocks
 
Eric
 
 
On Tue, Dec 9, 2025 at 3:18 PM Megan Feddern - NOAA Federal <megan.feddern@noaa.gov> wrote:
Eric's code would be great to model this off of. 
 
I think the main functionality that would need to be added to it for a hake application is that the depth/longitudinal extent is defined by an isobath for hake - so you need to use a isobath/bathymetry dataset (which is static data) as a cookie cutter for what longitudinal extent you want as opposed to giving it a max/min longitudinal extent. 
 
Eric - maybe you already have implemented something like this that I am unaware of? Other than defining longitudinal extent you should be able to just feed in lat/depth/months as defined in Appendix H/Kathleen's paper directly into the code Eric is already using to pull the updated GLORYs data and summarise it for the time series for the assessment.
 
On Tue, Dec 9, 2025 at 2:12 PM Kristin Marshall - NOAA Federal <kristin.marshall@noaa.gov> wrote:
Thanks Megan - this is all really helpful.  I'm adding Eric, too, because we were talking about this at home, and he also has code to pull/update GLORYS, in case that is helpful to what Andy is thinking about for the Appendix for this year.
 
I appreciate the reminder of the changing interpretation of some of the indicators across the different oceanographic products.  I haven't looked into it critically, but I recall MLD was an important indicator in one of the hake recruitment analyses but not the other, and wonder if that might be one reason for the changes in seeming importance in the recruitment model (along with all of the other factors like time series length and using LOOCV vs LFOCV as performance indicators).
 
Kristin
 
On Tue, Dec 9, 2025 at 9:49 AM Megan Feddern - NOAA Federal <megan.feddern@noaa.gov> wrote:
Hi Kristin and Andy,
 
The short answer is no I do not have streamlined code for GLORYs processing for hake but looking at appendix H it should be very easy to do. I think MOM6 should be used/considered for 2027 assessment but not 2026 (see below for details). 
 
The code I have for yellowtail which is modifed from Nick's scripts is pretty transferable across species (although the hake variables use isobaths to define which part of longitude/water column to use which may be harder than I think it will be...). Andy, let me know if there is a way to best support you on this - either as a resource or if it would be helpful if I put a streamlined/annotated script together based on Nick's code for GLORYs that can be easily updated from year to year that you can incorporate into a workflow. 
 
One thing to note with the transferability GLORYs/ROMs/MOM6: cross/along shore transport and mixed layer depth are different between models. Specifically cross/alongshore from ROMs processed by Mike Jacox is a true transport variable (accounts for specific shape of coastline and water movement) whereas data from MOM6 and GLORYs is actually sea water velocity which is being used as a proxy for transport. Generally, this is ok given the scales we are working at and the broad shape of the coastline but not ideal since the sign of velocity as a variable is consequential for denoting direction which has implications when it is standardized. This is one thing Brooke and I are working on for our MOM6 post-processing tool, calculating a true transport from the MOM6 data. For mixed layer depth, MOM6 and ROMs are a true depth variable whereas GLORYs is actually mixed layer thickness. My opinion is that when we transfer to MOM6, we want to be using the "correct" transport and mixed layer variables from the onset and Brooke and I do not have the transport calculations ready yet (but hope to by the end of FY 26 and thus the next hake assessment for 2027). I will also note all these transferability caveats the Lapenta intern is documenting in the report he is drafting.
 
Let me know if you have more questions or if you would like me to assist with this,
 
Megan
 
On Mon, Dec 8, 2025 at 2:48 PM Kristin Marshall - NOAA Federal <kristin.marshall@noaa.gov> wrote:
Hi Megan -
 
Do you have a workflow for pulling and updating the GLORYS indicators for hake recruitment?  I'm working with Andy Edwards (from DFO) and Mary, who are updating the hake ecosystem considerations and risk table from last year's stock assessment (see appendix H).  Andy has been working on developing a workflow, and I told him I would connect the two of you in case you have one already.  In 2024, Nick had pulled them for me for the recruitment modeling work I was doing. 
 
At some point, this could transition over to MOM6, but I'm not sure how much work that would be to do, and at least to me it doesn't seem super high priority to do this year, but open to others opinions on that last point.
 
Kristin
 
