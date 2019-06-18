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

metrics <- function(obs, pred) {
  data.frame(
    true_positive = sensitivity(obs = obs, pred = pred),
    true_negative = specificity(obs = obs, pred = pred),
    missed_alerts = false_negative(obs = obs, pred = pred)
  )
}

incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "incidence_forecasts.csv"
  )
)

<<<<<<< HEAD
metrics_central <- dplyr::group_by(
    incid_pred,
    time_window,
    n.dates.sim,
    country,
    week_of_projection
    ) %>% do(metrics(.$incid, .$y))

metrics_low <- dplyr::group_by(
  incid_pred,
  time_window,
  n.dates.sim,
  country,
  week_of_projection
) %>% do(metrics(.$incid, .$ymin))


metrics_high <- dplyr::group_by(
  incid_pred,
  time_window,
  n.dates.sim,
  country,
  week_of_projection
) %>% do(metrics(.$incid, .$ymax))

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
    "alerts_per_week.csv"
  )
)
