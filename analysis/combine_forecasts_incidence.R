library(dplyr)
source(here::here("analysis/parameters.R"))

## observations

weekly_incid <- readr::read_csv(here::here(all_files[[datasource]]$weekly_incidfile))

forecasts_files <- list.files(
  path = all_files[[datasource]]$outdir,
  pattern = paste0(
    "^forecasts_[0-9]*_",
    twindow,
    "_",
    n.dates.sim,
    ".csv"
  )
)

names(forecasts_files) <- stringr::str_replace(
  forecasts_files,
  "forecasts_",
  ""
) %>%
  stringr::str_replace(".csv", "")

all_forecasts <- purrr::map_dfr(forecasts_files,
  ~ readr::read_csv(here::here(
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
all_forecasts$week_of_year <-
  paste(
    lubridate::year(all_forecasts$date),
    "Week",
    lubridate::week(all_forecasts$date)
  )

all_forecasts <- group_by(all_forecasts, country, tproj) %>%
    mutate(week_of_projection = seq_len(n()))


## Save this for mapping later.
readr::write_csv(
  x = all_forecasts,
  path = here::here(
    all_files[[datasource]]$outdir,
    paste0(
      "all_forecasts_",
      twindow,
      "_",
      n.dates.sim,
      ".csv"
    )
  )
)

## Combine with forecasts
incid_pred <- dplyr::left_join(
  all_forecasts,
  weekly_incid,
  by = c("date", "country")
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
    paste0(
      "incidence_forecasts_",
      twindow,
      "_",
      n.dates.sim,
      ".csv"
    )
  )
)