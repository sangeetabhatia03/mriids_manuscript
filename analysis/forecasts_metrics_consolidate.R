week_projection <- function(date) {
    as.integer(
        cut(date, "7 days")
    )
}

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
indir <- paste0(all_files[[datasource]]$outdir, "/metrics/daily")
pattern <- "*daily_metrics*csv"
infiles <- list.files(
  path = indir,
  pattern = glob2rx(pattern)
)

names(infiles) <- stringr::str_remove_all(
  infiles,
  "daily_metrics_"
)

daily_metrics <- consolidate_metrics(infiles, indir)

## Weekly averages of daily mmetrics.
indir <- paste0(all_files[[datasource]]$outdir, "/metrics/weekly")
pattern <- "*weekly_avgd_metrics*csv"
infiles <- list.files(
  path = indir,
  pattern = glob2rx(pattern)
)

names(infiles) <- stringr::str_remove_all(
  infiles,
  "weekly_avgd_metrics_"
)


weekly_avgd <- consolidate_metrics(infiles, indir)
weekly_avgd$week_of_year <- week_of_year(weekly_avgd$date)
weekly_avgd <-dplyr::group_by(
    weekly_avgd,
    country, tproj, twindow, n.dates.sim) %>%
    mutate(week_of_projection = week_projection(date))



##  Weekly Metrics
indir <- paste0(all_files[[datasource]]$outdir, "/metrics/weekly")
pattern <- "*weekly_metrics*csv"

infiles <- list.files(
  path = indir,
  pattern = glob2rx(pattern)
)

names(infiles) <- stringr::str_remove_all(
  infiles,
  "weekly_metrics_"
)

weekly_metrics <- consolidate_metrics(infiles, indir)

weekly_metrics$week_of_year <- week_of_year(weekly_metrics$date)


weekly_metrics <-dplyr::group_by(
    weekly_metrics,
    country, tproj, twindow, n.dates.sim) %>%
    mutate(week_of_projection = week_projection(date))


## weekly_metrics <- select(
##     weekly_metrics,
##     -logaccuracy,
##     -sharpness
## )

## ## Add relative sharpness
## infiles <- paste0(
##     places,
##     "_rel_sharpness.csv"
## )
## infiles <- here::here(
##     all_files[[datasource]]$outdir,
##     infiles
##     )
## names(infiles) <- places

## rel_sness <- purrr::map_dfr(
##     infiles, ~ readr::read_csv(.x, col_names = FALSE), .id = "country"
##     )

## colnames(rel_sness) <- c(
##     "country",
##     "tproj",
##     "twindow",
##     "n.dates.sim",
##     "rel_sharpness",
##     "day_of_projection"
## )

readr::write_csv(
  x = weekly_avgd,
  path = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_weekly_avgd_metrics.csv")
  )
)


readr::write_csv(
  x = daily_metrics,
  path = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_daily_metrics.csv")
  )
)


readr::write_csv(
  x = weekly_metrics,
  path = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_weekly_metrics.csv")
  )
)


## Proportion of observations in 95% CrI
infiles <- here::here(
  paste0(all_files[[datasource]]$outdir, "/metrics/daily"),
  paste0(all_files[[datasource]]$places, "_prop_in_ci.csv")
)
names(infiles) <- all_files[[datasource]]$places

prop_in_ci <- purrr::map_dfr(
  infiles,
  ~ readr::read_csv(.x, col_names = FALSE),
  .id = "country"
)

colnames(prop_in_ci) <- c("country", "ran_on", "tproj", "twindow", "prop_in_ci")

readr::write_csv(
  x = prop_in_ci,
  path = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_prop_in_ci_overall.csv")
  )
)
