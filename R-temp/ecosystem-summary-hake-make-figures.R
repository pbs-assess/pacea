# Make figures for 2025 hake assessment

load_all()
source("ecosystem-summary-hake.R")
source("ecosystem-summary-hake-glorys.R")

# Specific to Andy's laptop:
hake_assessment_fig_dir <- paste0(here::here(),
                                  "/../hake-assessment-2024/doc/image-files/") # 2024 is generic directory for 2024 onwards

png(paste0(hake_assessment_fig_dir,
           "ecosystem_summary_from_pacea.png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200)
ecosystem_summary_hake()
dev.off()


png(paste0(hake_assessment_fig_dir,
           "ecosystem_summary_from_glorys.png"),
    width = 6,
    height = 8,
    units = "in",
    res = 1200)
ecosystem_summary_hake_glorys()
dev.off()
