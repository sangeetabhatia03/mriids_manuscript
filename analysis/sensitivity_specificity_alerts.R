library(dplyr)

incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "incidence_forecasts.csv"
  )
)

metrics_central <- dplyr::group_by(
    incid_pred,
    time_window,
    n.dates.sim,
    country,
    week_of_projection
    ) %>% do(metrics(.$incid, .$`50%`))

metrics_low <- dplyr::group_by(
  incid_pred,
  time_window,
  n.dates.sim,
  country,
  week_of_projection
) %>% do(metrics(.$incid, .$`2.5%`))


metrics_high <- dplyr::group_by(
  incid_pred,
  time_window,
  n.dates.sim,
  country,
  week_of_projection
) %>% do(metrics(.$incid, .$`97.5%`))

## combine to save as 1.
metrics_df <- dplyr::left_join(
    metrics_central,
    metrics_low,
    by = c(
      "time_window",
      "n.dates.sim",
      "country",
      "week_of_projection"
    ),
    suffix = c("_central", "_low")
) %>%
  dplyr::left_join(
        metrics_high,
        by = c(
          "time_window",
          "n.dates.sim",
          "country",
          "week_of_projection"
        )
    )


readr::write_csv(
  x =  metrics_df,
  path = here::here(
    all_files[[datasource]]$outdir,
    "sensitivity_specificity.csv"
    )
  )


## Temporal trend in true, false and missed alerts.
## Using central estimate.
qntls <- dplyr::select(incid_pred, `2.5%`:`97.5%`)
alerts <- purrr::map_dfr(
    qntls,
    function(pred) alert_type(obs = incid_pred$incid, pred = pred)
)
colnames(alerts) <- paste0("alert_using_", colnames(alerts))
weekly_alerts <- cbind(incid_pred, alerts)

readr::write_csv(
  x =  weekly_alerts,
  path = here::here(
    all_files[[datasource]]$outdir,
    "weekly_alerts.csv"
  )
)
