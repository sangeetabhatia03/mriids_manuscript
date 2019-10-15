library(dplyr)
source(here::here("analysis/parameters.R"))
source(here::here("analysis/utils.R"))
source(here::here("analysis/common_plot_properties.R"))
source(here::here("analysis/flow_matrix_estim.R"))


for (ds in c("ProMED", "HealthMap", "WHO")) {
    message("Working on ", datasource)
    tall <- readr::read_csv(
        here::here(all_files[[ds]]$weekly_incidfile)
        )
    tall <- dplyr::select(tall, -interpolated)
    wide <- tidyr::spread(
        tall, key = country, value = incid, fill = 0
        )
    outdir <- dirname((all_files[[ds]]$weekly_incidfile))
    outfile <- basename(all_files[[ds]]$weekly_incidfile)
    outfile <- glue::glue("wide_{outfile}")
    outfile <- glue::glue("{outdir}/{outfile}")
    outfile <- here::here(outfile)
    readr::write_csv(x = wide, path = outfile)
}



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

## Extract quantiles of weekly forecast samples.
## OUTFILE weekly forecast samples and
## weekly forecast quantiles
for (ds in c("HealthMap", "WHO")) {
  datasource <- ds
  message("Working on ", datasource)
  source(
    here::here(
       "analysis/weekly_forecasts_quantiles.R"
      )
  )
}


for (ds in c("ProMED", "HealthMap", "WHO")) {
  datasource <- ds
  message("Working on ", datasource)
  source(
      here::here(
         "analysis/forecasts_quantiles_consolidate.R"
      )
  )
}

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
# source(
#     here::here(
#        "analysis/forecasts_samples_consolidate.R"
#       )
# )

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


outdirs <- purrr::map(all_files, ~ .$outdir)

infiles <- list.files(
  path = as.character(outdirs),
  pattern = "consolidated_forecasts_samples_[0-9]*_[0-9]*_[0-9]*",
  recursive = FALSE,
  full.names = TRUE
)

purrr::pwalk(
    list(infiles),
    function(infile) {
        matches <- regmatches(
            x = basename(infile),
            m  = gregexpr("[[:digit:]]+", basename(infile))
        )
        matches <- as.integer(unlist(matches))
        tproj <- matches[1]
        twindow <- matches[2]
        ndates <- matches[3]
        predicted <- readr::read_rds(
            path = infile
            )
        for (place in places) {
            pred <- dplyr::select(predicted, starts_with(place))
            pred <- as.matrix(pred)
            rel_sness <- assessR:::rel_sharpness(pred)
            outfile <- paste0(
                place,
                "_rel_sharpness.csv"
            )
            outfile <- here::here(
                dirname(infile),
                outfile
            )
            out <- data.frame(
                tproj = tproj,
                twindow = twindow,
                n.dates.sim = ndates,
                rel_sharpness = rel_sness,
                day_of_projection = seq_len(ndates)
            )
            readr::write_csv(
                x = out,
                path = outfile,
                append = TRUE
             )

        }

    }
)




## Importation risk.
first_weekly_alert <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_date_of_first_weekly_alert.csv")
  )
)


out <- split(
    first_weekly_alert,
    list(
        first_weekly_alert$time_window,
        first_weekly_alert$n.dates.sim,
        first_weekly_alert$alert_type
    ),
    sep = "_"
)


purrr::iwalk(
    out,
    function(df, out) {
        message("Working on ", out)
        df <- dplyr::select(df, twindow = time_window, tproj)
        purrr::pwalk(
            df,
            function(twindow, tproj) {
                expect <- glue::glue(
                    "{datasource}_importation_risk_quantiles_{tproj}_{twindow}.csv"
                )
                expect <- here::here(
                    all_files[[datasource]]$outdir, expect
                )
                if (file.exists(expect)) {
                    message("already exists ", expect)
                    return
                } else {
                    tryCatch({
                        source("analysis/importation_risk2.R", local = TRUE)
                    }, error = function(e) {
                        message(e)
                        return
                    }
                   )
                }
            }
        )
    }
)


## Importation Risk right before the first case was observed.
## incid <- readr::read_csv(all_files[[datasource]]$incidfile)
## 15102019: Modified to use the tall file we have created
incid <- readr::read_csv(all_files[[datasource]]$incidfile)
incid_tall <- readr::read_csv(all_files[[datasource]]$incidtall)
##incid_tall <- tidyr::gather(incid, country, value, -date)
first_case <- dplyr::group_by(
    incid_tall,
    country
    ) %>%
    dplyr::filter(incid > 0) %>%
    dplyr::summarise(date_first_obs = min(date))

first_case <- dplyr::ungroup(first_case)
tprojs <- match(x = first_case$date_first_obs, table = incid$date)
## Nearest multiple of 7.
first_case$tproj <- round(tprojs / 7) * 7

first_case <- first_case[complete.cases(first_case), ]

first_case <- first_case[rep(1:nrow(first_case), each = 3), ]
first_case$twindow <- rep(c(14, 28, 42), length.out = nrow(first_case))

first_case <- dplyr::filter(first_case, tproj >= twindow + 7)
first_case$date <- incid$date[first_case$tproj]

readr::write_csv(
    x = first_case,
    path = here::here(
        all_files[[datasource]]$outdir,
        glue::glue("{Sys.Date()}_{datasource}_first_observed_case.csv")
    )
)

purrr::pmap(
    first_case,
    function(country, date, tproj, twindow) {
        expect <- glue::glue(
            "{datasource}_importation_risk_quantiles_{tproj}_{twindow}.csv"
        )
        expect <- here::here(
            all_files[[datasource]]$outdir, expect
        )
        if (file.exists(expect)) {
            message("already exists ", expect)
            return
        } else {
            tryCatch({
                source("analysis/importation_risk2.R", local = TRUE)
            }, error = function(e) {
                message(e)
                return
            }
          )
        }
    }
    )




for (ds in c("ProMED", "HealthMap", "WHO")) {
    datasource <- ds
    message("Working on ", datasource)
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
    all_twindows <- unique(pars$twindow)
    for (tw in all_twindows) {
        twindows <- tw
        tprojs <- pars[pars$twindow == tw, "tproj"]
        message("time_window is ", twindows)
        source(
            here::here(
               "analysis/importation_risk.R"
           )
        )

    }

}
