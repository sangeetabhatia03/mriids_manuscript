library(dplyr)
## observations

weekly_incid <- readr::read_csv(
  here::here(
    all_files[[datasource]]$weekly_incidfile
    )
  )
weekly_incid$week_of_year <- week_of_year(weekly_incid$date)

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
  ~ readr::read_rds(here::here(
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

## Combine with forecasts
incid_pred <- dplyr::left_join(
  all_forecasts,
  weekly_incid,
  by = c("week_of_year", "country")
)

## Get the date on which we projected forward;
incid_wide <- readr::read_csv(
  all_files[[datasource]]$incidfile
)

incid_pred$date_of_projection <- incid_wide$date[incid_pred$tproj]


readr::write_csv(
  x = incid_pred,
  path = here::here(
    all_files[[datasource]]$outdir,
    "incidence_forecasts.csv"
  )
)
