twindow <- 14
n.dates.sim <- 28
nsim <- 1000
datasource <- "HealthMap"
places <- c("LBR", "GIN", "SLE")
all_files <- list(
  WHO = list(
    raw = "data/raw/rstb20160308supp1.csv",
    incidfile =
      "data/processed/01032019_who_bycountry.csv",
    weekly_incidfile =
      "data/processed/01032019_who_bycountry_weekly.csv",
    stanfits_dir = "data/who_stanfits_gamma_ul_10",
    outdir = "data/who_stanfits_gamma_ul_10_output"
  ),

  ProMED = list(
    raw = "data/raw/promed_2014-2016-renamed.csv",
    incidfile =
      "data/processed/20122018_promed_loglinear_wide.csv",
    weekly_incidfile =
      "data/processed/20122018_promed_loglinear_weekly.csv",
    stanfits_dir = "data/stanfits",
    outdir = "data/output"
  ),

  HealthMap = list(
    incidfile = "data/processed/16042019_healthmap_loglinear_wide.csv",
    weekly_incidfile = "data/processed/16042019_healthmap_loglinear_weekly.csv",
    stanfits_dir = "data/healthmap_stanfits",
    outdir = "data/healthmap_output"
  )
)
