library(dplyr)

incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "incidence_forecasts.csv"
  )
)

incid_pred <- na.omit(incid_pred)
## Temporal trend in true, false and missed alerts.
## Using central estimate.
qntls <- dplyr::select(incid_pred, `2.5%`:`97.5%`)
alerts <- purrr::map_dfr(
    qntls,
    function(pred) alert_type(obs = incid_pred$incid, pred = pred)
)
colnames(alerts) <- paste0("alert_using_", colnames(alerts))
weekly_alerts <- cbind(incid_pred, alerts)

weekly_alerts <- tidyr::gather(
    weekly_alerts,
    key = "threshold",
    value = "alert_type",
    `alert_using_2.5%`:`alert_using_97.5%`
)
## weekly_alerts <- dplyr::select(
##     weekly_alerts,
##     time_window,
##     n.dates.sim,
##     threshold,
##     alert_type
##  )

readr::write_csv(
  x =  weekly_alerts,
  path = here::here(
    all_files[[datasource]]$outdir,
    "weekly_alerts.csv"
  )
)
