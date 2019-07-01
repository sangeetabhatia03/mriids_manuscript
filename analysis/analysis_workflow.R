library(dplyr)
source(here::here("analysis/parameters.R"))

fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

pars <- data.frame(fit = fitfiles) %>%
  tidyr::separate(
    col = fit,
    into = c(
      "tproj",
      "twindow"
    ),
    convert = TRUE
  )

pars <- arrange(pars, tproj)
pars$n.dates.sim <- n.dates.sim

## Extract R used for formard simulation.
## Assign appropriate phase label.
## Depends on : extract_mcmc_draws.Rmd
## OUTFILE 1 ProMED_rquantiles_projection.csv
## OUTFILE 2 ProMED_rquantiles_real_time.csv
## OUTFILE 3 ProMED_rquantiles_retrospective.csv
for (ds in c("ProMED", "HealthMap", "WHO")) {
    datasource <- ds
    message("Working on ", datasource)
    source(
        here::here(
           "analysis/assign_epidemic_phase.R"
        )
    )
}

## Extract quantiles of gravity model parameters.
## Depends on: having a stanfit object.
## OUTFILE gravity_model_parameters_quantiles.csv
source(
    here::here(
       "analysis/gravity_model_pars_quantiles.R"
      )
)

## This will join the weekly forecasts with the weekly incidence
## Needed to calculate true/false/missing alerts.
## Depends on: cleaned weekly incidence file.
## Outfile 1: incidence_forecasts.csv
## Outfile 2: all_forecasts_consolidated.csv

for (ds in c("ProMED", "HealthMap", "WHO")) {
  datasource <- ds
  message("Working on ", datasource)
  source(
      here::here(
         "analysis/combine_forecasts_incidence.R"
      )
  )
}

## Number of true, false, and missing alerts.
## Depends on: combine_forecasts_incidence.R
## Outfile 1: sensitivity_specificity.csv
## Outfile 2: alerts_per_week.csv
for (ds in c("ProMED", "HealthMap", "WHO")) {
  datasource <- ds
  message("Working on ", datasource)
  source(
      here::here(
         "analysis/sensitivity_specificity_alerts.R"
      )
  )
}




## Compiling various files
## Compile forecast samples. Needed by forecast_assess tasks.
## Outfiles: consolidated_tproj_twindow_n.dates.sim.Rds
source(
    here::here(
       "analysis/forecasts_samples_consolidate.R"
      )
)

## Consolidates metrics for all countries.
## Outfile 1: daily_metrics.csv
## Outfile 2: weekly_metrics.csv
## Outfile 3: prop_in_ci_overall.csv
for (ds in "WHO") {
    datasource <- ds
    message("Working on ", datasource)
    source(
        here::here(
            "analysis/forecasts_metrics_consolidate.R"
       )
    )
}


## Proportion of observations in CrI by week.


for (ds in c("HealthMap", "WHO")) {
  datasource <- ds
  message("Working on ", datasource)
  forecasts <- list.files(
    path = all_files[[datasource]]$outdir,
    pattern = glob2rx("^consolidated_forecasts_samples*Rds"),
    full.names = FALSE
  )

  forecasts <- stringr::str_remove_all(
     forecasts,
     "consolidated_forecasts_samples_"
  )
  pars <- data.frame(forecast = forecasts) %>%
    tidyr::separate(
    col = forecast,
    into = c(
      "tproj",
      "twindow",
      "n.dates.sim"
    ),
    convert = TRUE
  )

  for (row in 1:nrow(pars)) {
      params <-
          list(
              tproj = pars$tproj[row],
              twindow = pars$twindow[row],
              n.dates.sim = pars$n.dates.sim[row],
              incid = all_files[[ds]]$incidfile,
              outdir = all_files[[ds]]$outdir,
              indir = all_files[[ds]]$outdir
          )
           for (place in places) {
               params$place <- place
               message(params)
               source("analysis/prop_in_ci_by_week.R")
           }

     }

}
