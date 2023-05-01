# Outputs from the hake stock assessment. Run code line-by-line and check plots.
#  See ?hake for details.

load_all()

# Get this file from Andy running
#  hake-assessment/sandbox/andy/pacea-save/pacea-save.R
#  after 2023 hake assessment and copying it here.
#  For future assessments will maybe convert that to
#  a function. Check with Andy or Chris Grandin.

load("hake_recruitment_new.rda")                #

class(hake_recruitment_new) <- c("pacea_t",
                                 class(hake_recruitment_new))

attr(hake_recruitment_new, "axis_name") <-
  "Pacific Hake recruitment (billions of age-0 fish)"

if(check_index_changed(hake_recruitment,
                       hake_recruitment_new)){
  hake_recruitment <- hake_recruitment_new
  usethis::use_data(hake_recruitment,
                    overwrite = TRUE)
  plot(hake_recruitment, value = "val", style = "plain")
}
