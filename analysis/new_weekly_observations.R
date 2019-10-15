## Suppose we only want to look at weeks with new observed cases
## That is, that had no cases in the previous week.

weekly_incid <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_processed_weekly_incidence.csv")
  )
)

weekly_incid <- weekly_incid[weekly_incid$n.dates.sim != 14, ]
byparams <- split(
  weekly_incid,
  list(weekly_incid$time_window, weekly_incid$n.dates.sim),
  sep = "_"
)

newweekly_obs <- purrr::map_dfr(
  byparams,
  function(df) {
    ## what is the earliest date on which cases were observed anywhere?
    earliest <- min(dplyr::filter(df, incid > 0) %>%
      dplyr::pull(date))
    df$last_week <- df$date - 7
    df <- dplyr::left_join(
      df,
      df,
      by = c("last_week" = "date",
             "country",
             "time_window",
             "n.dates.sim"
             ),
      suffix = c("_this_week", "_last_week")
    )
    ## NAs for the earliest week for which we have no record of last week
    ## unique(df[!complete.cases(df), "date"])
    ##   date
    ##   <date>
    ## 1 2014-03-22

    ## weeks after the earliest cases have already been observed.
    df <- dplyr::filter(df, date > earliest)

    ## Filter where last_week was 0 but this week is not.
    df <- dplyr::filter(
      df,
      incid_this_week > 0 & incid_last_week == 0
      )
      df
  }
)


## newweekly_obs[!complete.cases(newweekly_obs), ]
## # A tibble: 0 x 10



## all_forecasts <- readr::read_csv(
##   file = here::here(
##     all_files[[datasource]]$outdir,
##     "all_forecasts_quantiles_consolidated.csv"
##   )
##  )


## Combine with forecasts
## incid_pred <- dplyr::left_join(
##    df,
##    all_forecasts,
##   by = c("week_of_year", "country")
## )

## ## Get the date on which we projected forward;
## incid_wide <- readr::read_csv(
##   all_files[[datasource]]$incidfile
## )

## incid_pred$date_of_projection <- incid_wide$date[incid_pred$tproj]



readr::write_csv(
  x = newweekly_obs,
  path = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_new_weekly_incidence.csv")
  )
)
