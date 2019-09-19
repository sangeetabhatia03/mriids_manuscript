forecasts_files <- list.files(
  path = all_files[[datasource]]$outdir,
  pattern = "^weekly_forecasts_quantiles_[0-9]*_[0-9]*_[0-9]*.Rds",
)

names(forecasts_files) <- stringr::str_replace(
  forecasts_files,
  "weekly_forecasts_quantiles_",
  ""
) %>%
  stringr::str_replace(".Rds", "")

all_forecasts <- purrr::map_dfr(
  forecasts_files,
  ~ readRDS(here::here(
    all_files[[datasource]]$outdir,
    .x
  )),
  .id = "parameters"
)

all_forecasts <- tidyr::separate(all_forecasts,
  col = parameters,
  into = c(
    "tproj",
    "time_window",
    "n.dates.sim"
  ),
  convert = TRUE
)

all_forecasts <- arrange(all_forecasts, tproj)
all_forecasts$week_of_year <- week_of_year(all_forecasts$date)



all_forecasts <- dplyr::group_by(
  all_forecasts,
  country, tproj, time_window, n.dates.sim) %>%
    mutate(week_of_projection = seq_len(n()))


## Save this for mapping later.
readr::write_csv(
  x = all_forecasts,
  path = here::here(
    all_files[[datasource]]$outdir,
    "all_forecasts_quantiles_consolidated.csv"
  )
)
