## observations
weekly_incid <- readr::read_csv(
   here::here(
      all_files[[datasource]]$outdir,
      "2019-10-07_new_weekly_incidence.csv"
  )
)

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



## Only NAs for dates for which we have no projections.
incid_pred[!complete.cases(incid_pred), ]
## # A tibble: 1 x 55
##   date.x     country incid_this_week interpolated_th… last_week  incid_last_week
##   <date>     <chr>             <dbl> <lgl>            <date>               <dbl>
## 1 2014-04-05 LBR                  24 FALSE            2014-03-29               0
## # … with 49 more variables: interpolated_last_week <lgl>,
## #   last_week_last_week <date>, week_of_year <chr>, tproj <dbl>,
## #   time_window <dbl>, n.dates.sim <dbl>, date.y <date>, `0%` <dbl>,
## #   `2.5%` <dbl>, `5%` <dbl>, `7.5%` <dbl>, `10%` <dbl>, `12.5%` <dbl>,
## #   `15%` <dbl>, `17.5%` <dbl>, `20%` <dbl>, `22.5%` <dbl>, `25%` <dbl>,
## #   `27.5%` <dbl>, `30%` <dbl>, `32.5%` <dbl>, `35%` <dbl>, `37.5%` <dbl>,
## #   `40%` <dbl>, `42.5%` <dbl>, `45%` <dbl>, `47.5%` <dbl>, `50%` <dbl>,
## #   `52.5%` <dbl>, `55%` <dbl>, `57.5%` <dbl>, `60%` <dbl>, `62.5%` <dbl>,
## #   `65%` <dbl>, `67.5%` <dbl>, `70%` <dbl>, `72.5%` <dbl>, `75%` <dbl>,
## #   `77.5%` <dbl>, `80%` <dbl>, `82.5%` <dbl>, `85%` <dbl>, `87.5%` <dbl>,
## #   `90%` <dbl>, `92.5%` <dbl>, `95%` <dbl>, `97.5%` <dbl>, `100%` <dbl>,
## #   week_of_projection <dbl>
## > min(all_forecasts$date)
## [1] "2014-04-12"

## Get the date on which we projected forward;
incid_wide <- readr::read_csv(
  all_files[[datasource]]$incidfile
)

incid_pred$date_of_projection <- incid_wide$date[incid_pred$tproj]

incid_pred <- na.omit(incid_pred)
## Temporal trend in true, false and missed alerts.
## Using central estimate.
qntls <- dplyr::select(incid_pred, `0%`:`100%`)

alerts <- purrr::map_dfr(
    qntls,
    function(pred) alert_type(obs = incid_pred$incid_this_week, pred = pred)
)
##colnames(alerts) <- paste0("alert_using_", colnames(alerts))
weekly_alerts <- cbind(
    dplyr::select(incid_pred, date.x:date.y),
    alerts
)

weekly_alerts <- tidyr::gather(
    weekly_alerts,
    key = "threshold",
    value = "alert_type",
    `0%`:`100%`
)



weekly_alerts <- weekly_alerts[weekly_alerts$n.dates.sim != 14, ]
## Plot TPR vs FPR
## TPR = # of TRUE Alerts/ (# of True Alerts + # of Missed Alerts)
## FPR = # of FALSE Alerts/ (# of No Alerts + # of False Alerts)

res <- dplyr::group_by(
    weekly_alerts,
    time_window,
    n.dates.sim,
    threshold,
    alert_type
) %>%
  dplyr::summarise(n = dplyr::n())

## noalerts <- length(which(res$alert_type == "No Alert"))
## falsealerts <- length(which(res$alert_type == "False Alert"))

res <- tidyr::spread(
    res,
    key = alert_type,
    value = n,
    fill = 0
    )


res$tpr <- res$`True Alert` / (res$`True Alert` + res$`Missed Alert`)
## No False Alerts or No Alerts were raised in this case
## res$fpr <- res$`False Alert` / (res$`No Alert` + res$`False Alert`)

out <- split(res, list(res$time_window, res$n.dates.sim))
for (o in out) {
    idx_max <- which.max(o$tpr)
    idx_min <- which.min(o$tpr)
    message(o$time_window[1], " ", o$n.dates.sim[1])
    message("Minimum TPR using ", o$threshold[idx_min], " ", o$tpr[idx_min])
    message("Maximum TPR using ", o$threshold[idx_max], " ", o$tpr[idx_max])
}
