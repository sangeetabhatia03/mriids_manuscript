incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_incidence_forecasts.csv")
    ),
  guess_max = 3000
  )

## 15102019 Analysis for West African countries only.
## 31102019 Official list of West African countries
##wafrica <-
## wafrica <- c(
##     "Benin",
##     "Burkina Fase",
##     "Gambia",
##     "Ghana",
##     "Guinea-Bissau",
##     "Côte d'Ivoire",
##     "Mali",
##     "Mauritania",
##     "Niger",
##     "Nigeria",
##     "Senegal",
##     "Togo"
## )
## wafrica <- countrycode::countrycode(
##     wafrica,
##     destination = "iso3c",
##     origin = "country.name"
## )


## incid_pred <- dplyr::filter(incid_pred, country %in% wafrica)

## 17102019 Analysis for all countries other than GIN, LBR and SLE.
## wafrica <- countrycode::countrycode(
##     c("Guinea",
##       "Sierra Leone",
##       "Liberia"
##     ),
##     destination = "iso3c",
##     origin = "country.name"
## )
## incid_pred <- dplyr::filter(incid_pred, ! (country %in% wafrica))
## We know what is being omitted. These are forecasts beyond the max
## date for which we have incidence.

incid_pred <- na.omit(incid_pred)

## 05102019 Changed 97.5% to 100% and 2.5% to 0%
## to explore the full range of forecast distribution
qntls <- dplyr::select(incid_pred, `0%`:`100%`)

alerts <- purrr::map_dfr(
    qntls,
    function(pred) alert_type(obs = incid_pred$incid, pred = pred)
 )

## No NAs alerts[!complete.cases(alerts), ]

weekly_alerts <- cbind(
    dplyr::select(incid_pred, tproj:country, week_of_year:date_of_projection),
    alerts
)

weekly_alerts <- tidyr::gather(
    weekly_alerts,
    key = "threshold",
    value = "alert_type",
    `0%`:`100%`
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
    glue::glue("{Sys.Date()}_weekly_alerts.csv")
  )
)
