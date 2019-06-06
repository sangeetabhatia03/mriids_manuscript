library(dplyr)
sensitivity <- function(obs, pred) {
  sum(pred > 0 & obs > 0, na.rm = TRUE) / sum(obs > 0, na.rm = TRUE)
}

specificity <- function(obs, pred) {
  sum(pred == 0 & obs == 0, na.rm = TRUE) / sum(obs == 0, na.rm = TRUE)
}

false_negative <- function(obs, pred) {
  sum(pred == 0 & obs > 0, na.rm = TRUE) / sum(obs > 0, na.rm = TRUE)
}

incid_pred <- readr::read_csv(
  file = here::here(
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


metrics_central <- dplyr::group_by(incid_pred, country, week_of_projection) %>%
  dplyr::summarise(
    true_positive_central = sensitivity(obs = incid, pred = y),
    true_negative_central = specificity(obs = incid, pred = y),
    missed_alerts_central = false_negative(obs = incid, pred = y)
  )

metrics_low <- dplyr::group_by(incid_pred, country, week_of_projection) %>%
  dplyr::summarise(
    true_positive_low = sensitivity(obs = incid, pred = ymin),
    true_negative_low = specificity(obs = incid, pred = ymin),
    missed_alerts_low = false_negative(obs = incid, pred = ymin)
  )

metrics_high <- dplyr::group_by(incid_pred, country, week_of_projection) %>%
  dplyr::summarise(
    true_positive_high = sensitivity(obs = incid, pred = ymax),
    true_negative_high = specificity(obs = incid, pred = ymax),
    missed_alerts_high = false_negative(obs = incid, pred = ymax)
  )

## combine to save as 1.
metrics <- left_join(
    metrics_central,
    metrics_low
) %>%
    left_join(
        metrics_high,
    )


readr::write_csv(
  x =  metrics,
  path = here::here(
    all_files[[datasource]]$outdir,
    paste0(
      "sensitivity_specificity_",
      twindow,
      "_",
      n.dates.sim,
      ".csv"
    )
  )
)

## Temporal trend in true, false and missed alerts.
## Using central estimate.
incid_pred$alert_type_central <- dplyr::case_when(
  incid_pred$incid == 0 & incid_pred$y > 0 ~ "False Alert",
  incid_pred$incid > 0 & incid_pred$y > 0 ~ "True Alert",
  incid_pred$incid > 0 & incid_pred$y == 0 ~ "Missed Alert"
)

incid_pred$alert_type_low <- dplyr::case_when(
  incid_pred$incid == 0 & incid_pred$ymin > 0 ~ "False Alert",
  incid_pred$incid > 0 & incid_pred$ymin > 0 ~ "True Alert",
  incid_pred$incid > 0 & incid_pred$ymin == 0 ~ "Missed Alert"
)

incid_pred$alert_type_high <- dplyr::case_when(
  incid_pred$incid == 0 & incid_pred$ymax > 0 ~ "False Alert",
  incid_pred$incid > 0 & incid_pred$ymax > 0 ~ "True Alert",
  incid_pred$incid > 0 & incid_pred$ymax == 0 ~ "Missed Alert"
)


readr::write_csv(
  x =  incid_pred,
  path = here::here(
    all_files[[datasource]]$outdir,
    paste0(
      "alerts_per_week_",
      twindow,
      "_",
      n.dates.sim,
      ".csv"
    )
  )
)
