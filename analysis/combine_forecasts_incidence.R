library(dplyr)
## observations

weekly_incid <- readr::read_csv(
  here::here(
    all_files[[datasource]]$weekly_incidfile
    )
  )

weekly_incid$week_of_year <- week_of_year(weekly_incid$date)

all_forecasts <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "all_forecasts_quantiles_consolidated.csv"
  )
  )


## Combine with forecasts
incid_pred <- dplyr::left_join(
   weekly_incid,
   all_forecasts,
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
