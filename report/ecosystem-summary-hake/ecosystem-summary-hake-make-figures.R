# Make figures for 2025 and 2026 hake assessment

# Saving the resulting index tibbles as .rds objects to then try in
# pbsEDM. Currently they are already shifted to line up with the year of hake
# recruitment; will likely unshift at some point TODO, but would want to
# separate construction and plottig (and only do the shifting in the plotting
# stage).

load_all()
source("ecosystem-summary-hake.R")
source("ecosystem-summary-hake-glorys.R")

assess_yr = 2026

    dir_to_save <- paste0(here::here(),
                          "/../pacea/data-raw/groundfish/hake-",
                          assess_yr)

# Specific to Andy's laptop:
hake_assessment_fig_dir <- paste0(getwd(), "/")
# hake_assessment_fig_dir <- paste0(here::here(),
# This was used for 2025 and 2026:                                  "/../hake-assessment-2024/doc/image-files/") # 2024 is generic directory for 2024 onwards

assess_yr <- 2026

png(paste0(hake_assessment_fig_dir,
           "ecosystem_summary_from_pacea.png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200)
hake_indices_pacea <- ecosystem_summary_hake()
dev.off()

hake_indices_pacea_name <- paste0("hake_indices_pacea_", assess_yr)

assign(hake_indices_pacea_name,
       hake_indices_pacea)

saveRDS(get(hake_indices_pacea_name),
        file = paste0(hake_indices_pacea_name, ".rds"))

png(paste0(hake_assessment_fig_dir,
           "ecosystem_summary_from_glorys.png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200)
hake_indices_glorys <- ecosystem_summary_hake_glorys()
dev.off()

hake_indices_glorys_name <- paste0("hake_indices_glorys_", assess_yr)

assign(hake_indices_glorys_name,
       hake_indices_glorys)

saveRDS(get(hake_indices_glorys_name),
        file = paste0(hake_indices_glorys_name, ".rds"))
