library(dplyr)
## observations

## weekly_incid <- readr::read_csv(
##   here::here(
##     all_files[[datasource]]$weekly_incidfile
##     )
##   )

## weekly_incid$week_of_year <- week_of_year(weekly_incid$date)

all_forecasts <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "all_forecasts_quantiles_consolidated.csv"
  )
)
## None of the columns in all_forecasts have NA
## all_forecasts[!complete.cases(all_forecasts), ]

incid_wide <- readr::read_csv(
  all_files[[datasource]]$incidfile
)
message("Read ", all_files[[datasource]]$incidfile)

incid_tall <- readr::read_csv(
  all_files[[datasource]]$incidtall
  )
message("Read ", all_files[[datasource]]$incidtall)


## 11-10-2019 After discussion with Natsuko, realised that
## we need to make sure we are capturing the same dates
## in a week for both obs and predicted values while aggregating to
## weekly. What I was doing earlier was very nearly the same
## because all my projections were happening at multiples of 7
## (day 21, day 28 etc). But the aggregation of observed and predicted
## was misaligned by a single day. This would make a difference if that
## 1 day for example had many cases.

byparams <- split(
    all_forecasts,
    list(all_forecasts$time_window, all_forecasts$n.dates.sim),
    sep = "_"
)

weekly_incid <- purrr::map_dfr(
    byparams,
    function(df) {
        pred_dates <- unique(df$date)
        names(pred_dates) <- pred_dates
        ## Since the forecasts are weekly, we will get the dates for
        ## which we made projections by looking at the date of the
        ## weekly forcast (which is that start of the 7 day interval)
        pred_dates <- purrr::map(
            pred_dates,
            function(x) seq(from = x, length.out = 7, by = "1 day")
        )
        ## Get exactly these dates from daily incidence.
        weekly <- purrr::map(
            pred_dates,
            function(dates) incid_tall[incid_tall$date %in% dates, ]
        )
        ## It is possible that these dates are not present.
        weekly <- purrr::keep(weekly, function(x) nrow(x) > 0)
        weekly <- purrr::map_dfr(
            weekly,
            function(x) {
                x <- dplyr::group_by(x, country) %>%
                    dplyr::summarise(
                        incid = sum(incid),
                        interpolated = any(interpolated)
                    )
                x
            },
            .id = "date"
         )
        weekly
    },
    .id = "params"
   )

weekly_incid$week_of_year <- week_of_year(weekly_incid$date)
weekly_incid <- tidyr::separate(
    weekly_incid,
    col = params,
    into = c("time_window", "n.dates.sim"),
    convert = TRUE
    )

## Check weekly_incid[!complete.cases(weekly_incid), ]
## Combine with forecasts
incid_pred <- dplyr::left_join(
   all_forecasts,
   weekly_incid,
   by = c("week_of_year", "country", "time_window", "n.dates.sim"),
   suffix = c("_pred", "_obs")
 )

## The join has NAs for dates that are beyond the max date in
## weekly_incid. Also for dates where we did not full 7 days observed
## data.
## withnas <- incid_pred[!complete.cases(incid_pred), ]
## which(withnas$date_pred <=  max(weekly_incid$date))

## Get the date on which we projected forward;

incid_pred$date_of_projection <- incid_wide$date[incid_pred$tproj]


readr::write_csv(
  x = incid_pred,
  path = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_incidence_forecasts.csv")
  )
)


readr::write_csv(
  x = weekly_incid,
  path = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_processed_weekly_incidence.csv")
  )
)
