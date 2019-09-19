## Suppose we only want to look at weeks with new observed cases
## That is, that had no cases in the previous week.

weekly_incid <- readr::read_csv(
  here::here(
    all_files[[datasource]]$weekly_incidfile
    )
  )

## what is the earliest date on which cases were observed anywhere?
earliest <- min(dplyr::filter(weekly_incid, incid > 0) %>%
    dplyr::pull(date))


weekly_incid$last_week <- weekly_incid$date - 7
weekly_incid <- dplyr::left_join(
    weekly_incid,
    weekly_incid,
    by = c("last_week" = "date", "country" = "country"),
    suffix = c("_this_week", "_last_week")
)

## weeks after the earliest cases have already been observed.
weekly_incid <-dplyr::filter(weekly_incid, date > earliest)

## an NA in last_week indicates that there were no cases in this
## country in the last week. 6 such rows - om GHA, LBR, SLE, NGA, SEN
## and MLI.
weekly_incid$incid_last_week[is.na(weekly_incid$incid_last_week)] <-  0

## Filter where last_week was 0 but this week is not.
weekly_incid <- dplyr::filter(
    weekly_incid,
    incid_this_week > 0 & incid_last_week == 0
)

weekly_incid$week_of_year <- week_of_year(weekly_incid$date)

## all_forecasts <- readr::read_csv(
##   file = here::here(
##     all_files[[datasource]]$outdir,
##     "all_forecasts_quantiles_consolidated.csv"
##   )
##  )


## Combine with forecasts
## incid_pred <- dplyr::left_join(
##    weekly_incid,
##    all_forecasts,
##   by = c("week_of_year", "country")
## )

## ## Get the date on which we projected forward;
## incid_wide <- readr::read_csv(
##   all_files[[datasource]]$incidfile
## )

## incid_pred$date_of_projection <- incid_wide$date[incid_pred$tproj]



readr::write_csv(
  x = weekly_incid,
  path = here::here(
    all_files[[datasource]]$outdir,
    "new_weekly_incidence.csv"
  )
)
