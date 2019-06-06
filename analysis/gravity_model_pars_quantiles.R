library(dplyr)
library(rstan)



## 1. Get a list of all fitted objects.
fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

names(fitfiles) <- stringr::str_remove_all(
  fitfiles,
  ".rds"
)

## Summary object for quantiles.
gravity_model_pars <-
  purrr::map_dfr(
    fitfiles,
    function(fit) {
      fitobj <-
        readr::read_rds(here::here(
          all_files[[datasource]]$stanfits_dir,
          fit
        ))
      fit_summary <- rstan::summary(fitobj)
      out <- fit_summary$summary[c("pstay", "gamma"), ]
      out <- tibble::rownames_to_column(as.data.frame(out), var = "param")
      out
    },
    .id = "parameters"
  )


gravity_model_pars <- tidyr::separate(
  data = gravity_model_pars,
  col = parameters,
  into = c("tproj", "twindow"),
  convert = TRUE
)

## Affix date
incid <- readr::read_csv(
  here::here(
    file = all_files[[datasource]]$incidfile
  )
)

gravity_model_pars$date <- incid$date[gravity_model_pars$tproj]

readr::write_csv(
  x = gravity_model_pars,
  path = here::here(
    all_files[[datasource]]$outdir,
    "gravity_model_parameters_quantiles.csv"
  )
)
