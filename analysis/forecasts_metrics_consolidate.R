consolidate_metrics <- function(infiles, indir) {
  metrics <- purrr::map_dfr(
    infiles,
    ~ readr::read_csv(here::here(indir, .x)),
    .id = "params"
  )
  metrics <- tidyr::separate(
    metrics,
    col = params,
    into = c("country", "tproj", "twindow", "n.dates.sim"),
    convert = TRUE
  )
  metrics
}
message("Reading from and writing to ", all_files[[datasource]]$outdir)

pattern <- "*daily_metrics*csv"
infiles <- list.files(
  path = paste0(all_files[[datasource]]$outdir, "/metrics/daily"),
  pattern = glob2rx(pattern),
)

names(infiles) <- stringr::str_remove_all(
  infiles,
  "daily_metrics_"
)

indir <- paste0(all_files[[datasource]]$outdir, "/metrics/daily")
daily_metrics <- consolidate_metrics(infiles, indir)


##  Weekly Metrics
infiles <- stringr::str_replace_all(infiles,
  pattern = "daily",
  replacement = "weekly"
)

names(infiles) <- stringr::str_remove_all(
  infiles,
  "weekly_metrics_"
)

indir <- stringr::str_replace_all(indir,
  pattern = "daily",
  replacement = "weekly"
)

weekly_metrics <- consolidate_metrics(infiles, indir)

weekly_metrics$week_of_year <- paste(
    lubridate::year(weekly_metrics$date),
    "Week",
    lubridate::week(weekly_metrics$date)
)

week_projection <- function(date) {
    as.integer(
        cut(date, "7 days")
    )
}

weekly_metrics <-dplyr::group_by(
    weekly_metrics,
    country, tproj, twindow, n.dates.sim) %>%
    mutate(week_of_projection = week_projection(date))




readr::write_csv(
  x = daily_metrics,
  path = here::here(
    all_files[[datasource]]$outdir,
    "daily_metrics.csv"
  )
)


readr::write_csv(
  x = weekly_metrics,
  path = here::here(
    all_files[[datasource]]$outdir,
    "weekly_metrics.csv"
  )
)


## Proportion of observations in 95% CrI
infiles <- here::here(
  paste0(all_files[[datasource]]$outdir, "/metrics/daily"),
  paste0(places, "_prop_in_ci.csv")
)
names(infiles) <- places

prop_in_ci <- purrr::map_dfr(
  infiles,
  ~ readr::read_csv(.x, col_names = FALSE),
  .id = "country"
)

colnames(prop_in_ci) <- c("country", "tproj", "twindow", "prop_in_ci")

readr::write_csv(
  x = prop_in_ci,
  path = here::here(
    all_files[[datasource]]$outdir,
    "prop_in_ci_overall.csv"
  )
)
