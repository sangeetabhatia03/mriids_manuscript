all_files <- list(
  WHO = list(
    raw = "data/raw/rstb20160308supp1.csv",
    incidfile =
      "data/processed/01032019_who_bycountry.csv",
    weekly_incidfile =
        "data/processed/01032019_who_bycountry_weekly.csv",
    weekly_wide =
      "data/processed/wide_01032019_who_bycountry_weekly.csv",
    stanfits_dir = "data/who_stanfits",
    outdir = "data/who_output",
    metadatafile = "data/processed/all_african_centroids.csv",
    dist_obj = "data/processed/allafrica_distances.rds",
    twindow = 14,
    n.dates.sim = 42,
    nsim = 1000,
    places = c("LBR", "GIN", "SLE")
  ),

  ProMED10 = list(
    raw = "data/raw/promed_2014-2016-renamed.csv",
    incidfile =
      "data/processed/20122018_promed_loglinear_wide.csv",
    weekly_incidfile =
        "data/processed/20122018_promed_loglinear_weekly.csv",
    weekly_wide =
      "data/processed/wide_20122018_promed_loglinear_weekly.csv",
    stanfits_dir = "data/promed_stanfits_gamma_ul_10",
    outdir = "data/promed_stanfits_gamma_ul_10_output",
    metadatafile = "data/processed/all_african_centroids.csv",
    dist_obj = "data/processed/allafrica_distances.rds",
    twindow = 14,
    n.dates.sim = 28,
    nsim = 1000,
    places = c("LBR", "GIN", "SLE")
  ),
  ProMED = list(
    raw = "data/raw/promed_2014-2016-renamed.csv",
    incidfile =
      "data/processed/20122018_promed_loglinear_wide.csv",
    weekly_incidfile =
      "data/processed/20122018_promed_loglinear_weekly.csv",
    weekly_wide =
      "data/processed/wide_20122018_promed_loglinear_weekly.csv",
    stanfits_dir = "data/stanfits",
    outdir = "data/output",
    metadatafile = "data/processed/all_african_centroids.csv",
    dist_obj = "data/processed/allafrica_distances.rds",
    twindow = 14,
    n.dates.sim = 28,
    nsim = 1000,
    places = c("LBR", "GIN", "SLE")
  ),

  HealthMap10 = list(
    incidfile = "data/processed/16042019_healthmap_loglinear_wide.csv",
    weekly_incidfile = "data/processed/16042019_healthmap_loglinear_weekly.csv",
    stanfits_dir = "data/healthmap_stanfits_gamma_ul_10",
    outdir = "data/healthmap_gamma_ul_10_output",
    metadatafile = "data/processed/all_african_centroids.csv",
    dist_obj = "data/processed/allafrica_distances.rds",
    twindow = 14,
    n.dates.sim = 28,
    nsim = 1000,
    places = c("LBR", "GIN", "SLE")
  ),

  HealthMap = list(
    incidfile = "data/processed/16042019_healthmap_loglinear_wide.csv",
    weekly_incidfile = "data/processed/16042019_healthmap_loglinear_weekly.csv",
    weekly_wide =
      "data/processed/wide_16042019_healthmap_loglinear_weekly.csv",
    stanfits_dir = "data/healthmap_stanfits",
    outdir = "data/healthmap_output",
    metadatafile = "data/processed/all_african_centroids.csv",
    dist_obj = "data/processed/allafrica_distances.rds",
    twindow = 14,
    n.dates.sim = 28,
    nsim = 1000,
    places = c("LBR", "GIN", "SLE")
  ),

  DRC = list(
    incidfile = "data/incid.csv",
    stanfits_dir = "data/ebola2018_stanfits",
    outdir = "data/ebola2018_output",
    twindow = 14,
    n.dates.sim = 28,
    nsim = 1000,
    places = c("LBR", "GIN", "SLE")
  ),

  NE_DRC = list(
    incidfile = "data/processed/ne_drc_incidence.csv",
    stanfits_dir = "data/NE_DRC_stanfits/",
    outdir = "data/NE_DRC_output/",
    metadatafile = "data/processed/ne_drc_centroids.csv",
    dist_obj = "data/processed/ne_drc_distances.rds",
    twindow = 14,
    n.dates.sim = 28,
    nsim = 1000,
    places = c("LBR", "GIN", "SLE")
  )
)
