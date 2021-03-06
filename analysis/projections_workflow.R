library(dplyr)
library(ggmcmc)
library(rstan)

source(here::here("analysis/parameters.R"))
source(here::here("analysis/utils.R"))
## 1. Get a list of all fitted objects.
fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

## 2. Check convergence using ggmcmc
for (fit in fitfiles) {
  outfile <- stringr::str_replace(fit, ".rds", ".pdf")
  fit1 <- readr::read_rds(fit)
  S <- ggmcmc::ggs(fit1)
  ggmcmc::ggmcmc(S, file = outfile)

}



## Input parameters

day0 <- readr::read_csv(here::here(all_files[[datasource]]$incidfile),
  n_max = 1
) %>%
  pull(date)


## 3. Use the list of fits to construct the set of
## parameters.
## tidyr will throw warnings as fitfiles has other
## stuff.
## these can be ignored.
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

## 4. Flow matrices
## While creating these, we sample from pstay and
## gamma an create the indices so that a joint
## sample can be extracted from R.

source("analysis/flow_matrix_estim.R")

params <- common_params(
    metadatafile = all_files[[datasource]]$metadatafile,
    dist_obj = all_files[[datasource]]$dist_obj,
    countries_col = "ISO3",
    pop_col = "pop"
)


for (fit in fitfiles) {
  message("working on ", fit)
  fitobj <-
    readr::read_rds(here::here(
      all_files[[datasource]]$stanfits_dir,
      fit
    ))
  if (!inherits(fitobj, "stanfit")) {
    message("Not a stanfit object ", fit)
    next
  }
  list_of_draws <- rstan::extract(fitobj)
  nsamples <- length(list_of_draws[["gamma"]])
  selected <- sample(seq_len(nsamples), all_files[[datasource]]$nsim)
  ## we want to sample all parameters jointly
  ## so save the indices to be used later.
  readr::write_rds(
    x = selected,
    path = here::here(
      all_files[[datasource]]$stanfits_dir,
      paste0(
        "idx_",
        fit
      )
    )
  )
  gamma_samples <- list_of_draws[["gamma"]][selected]
  pstay_samples <- list_of_draws[["pstay"]][selected]

  l <- list(
    x = gamma_samples,
    y = pstay_samples,
    z = selected
  )

  purrr::pwalk(
    l,
    function(x, y, z) {
      outfile <- glue::glue("flow_matrices/flow_{z}_{fit}")
      outfile <- here::here(
          all_files[[datasource]]$stanfits_dir,
          outfile
          )
      if (file.exists(outfile)) return
      df <- flow_mat(
        gamma = x,
        pstay = y,
        params
      )
      readr::write_rds(
        x = df,
        path = outfile
      )
    }
  )
}

## Extract stuff from stanfit objects.
purrr::pwalk(
  pars,
  function(tproj, twindow) {
    rmarkdown::render(
      "analysis/extract_mcmc_draws.Rmd",
      params = list(
        tproj = tproj,
        twindow = twindow,
        day0 = day0,
        indir =
          all_files[[datasource]]$stanfits_dir
      )
    )
  }
)

## Forward simulation.
for(ndates in c(28, 42, 56)) {
purrr::pwalk(
  pars,
  function(tproj, twindow) {
    expect <- glue::glue("forecasts_{tproj}_{twindow}_{ndates}.csv")
    expect <- here::here(all_files[[datasource]]$outdir, expect)
    if (file.exists(expect)) {
      message("Already exists ", expect)
    } else {
      message("Running for ", expect)
    rmarkdown::render(
      "analysis/projection_using_fitted.Rmd",
      params = list(
        tproj = tproj,
        twindow = twindow,
        n.dates.sim = ndates,
        day0 = day0,
        indir =
          all_files[[datasource]]$stanfits_dir,
        outdir =
          all_files[[datasource]]$outdir
      )
    )
    }
  }
 )
}

## Forecast metrics
source(
    here::here(
       "analysis/forecasts_samples_consolidate.R"
      )
)
beepr::beep(sound = 2)

source(
    here::here(
       "analysis/weekly_forecasts_samples.R"
      )
)
beepr::beep(sound = 2)


## combine weekly forecasts and weekly observed.
## This will create
## glue::glue("{Sys.Date()}_incidence_forecasts.csv")
## glue::glue("{Sys.Date()}_{datasource}_processed_weekly_incidence.csv")
source(
    "analysis/combine_forecasts_incidence.R"
)

## Reshape glue::glue("{Sys.Date()}_{datasource}_processed_weekly_incidence.csv")
## to wide format as this is needed for forecasts_assess task.

weekly_incid <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_processed_weekly_incidence.csv")
  )
  )

weekly_wide <- dplyr::select(weekly_incid, -interpolated)
weekly_wide <- tidyr::spread(
    weekly_wide, key = country, value = incid
    )

bycountry <- split(
    weekly_wide,
    list(weekly_wide$time_window, weekly_wide$n.dates.sim),
    sep = "_"
)

outfiles <- glue::glue(
    "{Sys.Date()}_{datasource}_{names(bycountry)}_weekly_wide.csv"
)

outfiles <- here::here(
    all_files[[datasource]]$outdir, outfiles
    )

purrr::walk2(
    bycountry,
    outfiles,
    function(df, outfile) {
        df <- dplyr::select(df, -time_window, -n.dates.sim)
        readr::write_csv(df, outfile)
    }
  )

## This bit depends on the time window, tproj as well as n.dates.si.
## Update pars.
forecasts_files <- list.files(
  path = all_files[[datasource]]$outdir,
  pattern = "^weekly_forecasts_samples_[0-9]*_[0-9]*_[0-9]*.Rds",
  full.names = FALSE
)
forecasts_files <- stringr::str_remove_all(forecasts_files, "weekly_forecasts_samples_")

pars2 <- data.frame(forecast = forecasts_files) %>%
  tidyr::separate(
    col = forecast,
    into = c(
      "tproj",
      "twindow",
      "ndates"
    ),
    convert = TRUE
  )

## purrr::pwalk(
##   pars2,
##   function(tproj, twindow, ndates) {
##     for (place in all_files[[datasource]]$places) {
##       rmarkdown::render(
##         "analysis/forecasts_assess.Rmd",
##         params = list(
##           tproj = tproj,
##           twindow = twindow,
##           incid =
##             all_files[[datasource]]$incidfile,
##           n.dates.sim = ndates,
##           place = place,
##           outdir =
##             all_files[[datasource]]$outdir,
##           indir =
##             all_files[[datasource]]$outdir
##         )
##       )
##     } ## end of for
##   }
## )

purrr::pwalk(
  pars2,
  function(tproj, twindow, ndates) {
      for (place in all_files[[datasource]]$places) {
          weekly_wide <- glue::glue(
              "{all_files[[datasource]]$outdir}/",
              "{Sys.Date()}_{datasource}_{twindow}_{ndates}_weekly_wide.csv"
          )
      rmarkdown::render(
        "analysis/weekly_forecasts_assess.Rmd",
        params = list(
          tproj = tproj,
          twindow = twindow,
          incid = weekly_wide,
          n.dates.sim = ndates,
          place = place,
          outdir =
            all_files[[datasource]]$outdir,
          indir =
            all_files[[datasource]]$outdir
        ),
        output_dir = glue::glue("{all_files[[datasource]]$outdir}/{Sys.Date()}")
      )
    } ## end of for
  }
)


## Proportion of observations in 95% CrI by week.
## Proportion in CrI each week.
# purrr::pwalk(
#   pars,
#   function(tproj, twindow) {
#     for (place in all_files[[datasource]]$places) {
#       rmarkdown::render(
#         "analysis/prop_in_ci_by_week.Rmd",
#         params = list(
#           tproj = tproj,
#           twindow = twindow,
#           incid =
#             all_files[[datasource]]$incidfile,
#           n.dates.sim =
#            all_files[[datasource]]$n.dates.sim,
#           place = place,
#           outdir =
#             all_files[[datasource]]$outdir,
#           indir =
#             all_files[[datasource]]$outdir
#         )
#       )
#     } ## end of for
#   }
# )
