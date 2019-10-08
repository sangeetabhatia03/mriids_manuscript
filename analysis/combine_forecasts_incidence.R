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

## None of the columns in weekly_incid have NA
## weekly_incid[!complete.cases(weekly_incid), ]
## None of the columns in all_forecasts have NA
## all_forecasts[!complete.cases(all_forecasts), ]
## Combine with forecasts

incid_pred <- dplyr::left_join(
   all_forecasts,
   weekly_incid,
   by = c("week_of_year", "country"),
   suffix = c("_pred", "_obs")
)
## The join has NAs for dates that are beyond the max date in
## weekly_incid.
## withnas <- incid_pred[!complete.cases(incid_pred), ]
## which(withnas$date_pred <=  max(weekly_incid$date))

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
