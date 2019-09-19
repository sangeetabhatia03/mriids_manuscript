## observations
weekly_incid <- readr::read_csv(
   here::here(
      all_files[[datasource]]$outdir,
       "new_weekly_incidence.csv"
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

## Get the date on which we projected forward;
incid_wide <- readr::read_csv(
  all_files[[datasource]]$incidfile
)

incid_pred$date_of_projection <- incid_wide$date[incid_pred$tproj]

incid_pred <- na.omit(incid_pred)
## Temporal trend in true, false and missed alerts.
## Using central estimate.
qntls <- dplyr::select(incid_pred, `2.5%`:`97.5%`)

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
    `2.5%`:`97.5%`
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

res <- tidyr::spread(
    res,
    key = alert_type,
    value = n)

res$tpr <- res$`True Alert` / (res$`True Alert` + res$`Missed Alert`)
## No False Alerts were raised in this case
##res$fpr <- res$`False Alert` / (res$`No Alert` + res$`False Alert`)

out <- split(res, list(res$time_window, res$n.dates.sim))
for (o in out) {
    idx_max <- which.max(o$tpr)
    idx_min <- which.min(o$tpr)
    message(o$time_window[1], " ", o$n.dates.sim[1])
    message("Minimum TPR using ", o$threshold[idx_min], " ", o$tpr[idx_min])
    message("Maximum TPR using ", o$threshold[idx_max], " ", o$tpr[idx_max])
}
