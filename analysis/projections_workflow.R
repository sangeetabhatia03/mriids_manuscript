library(dplyr)
library(ggmcmc)
library(rstan)

source(here::here("analysis/parameters.R"))

## 1. Get a list of all fitted objects.
fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

## 2. Check convergence using ggmcmc
for (fit in fitfiles) {
  outfile <- stringr::str_replace(fit, ".rds", ".pdf")
  outfile <- paste0("ggmcmc/", outfile)
  if (!file.exists(outfile)) {
    fit1 <- readr::read_rds(fit)
    S <- ggmcmc::ggs(fit1)
    message("Does not exist ", outfile)
    ggmcmc(S, file = outfile)
  }
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
  selected <- sample(seq_len(nsamples), nsim)
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
      df <- flow_mat(
        gamma = x,
        pstay = y
      )
      readr::write_rds(
        x = df,
        path = here::here(
          all_files[[datasource]]$stanfits_dir,
          paste("flow_matrices/flow",
            z,
            fit,
            sep = "_"
          )
        )
      )
    }
  )
}

##  Extract stuff from stanfit objects.
purrr::pwalk(
  pars,
  function(tproj, twindow) {
    rmarkdown::render(
      "analysis/extract_mcmc_draws.Rmd",
      output_file = paste0(
        "extract_mcmc_draws_",
        tproj,
        "_",
        twindow
      ),

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
purrr::pwalk(
  pars,
  function(tproj, twindow) {
    rmarkdown::render(
      "analysis/projection_using_fitted.Rmd",
      output_file = paste0(
        "projection_using_fitted_",
        tproj,
        "_",
        twindow
      ),
      params = list(
        tproj = tproj,
        twindow = twindow,
        n.dates.sim = n.dates.sim,
        day0 = day0,
        indir =
          all_files[[datasource]]$stanfits_dir,
        outdir =
          all_files[[datasource]]$outdir
      )
    )
  }
)

##  Forecast metrics

purrr::pwalk(
  pars,
  function(tproj, twindow) {
    for (place in places) {
      rmarkdown::render(
        "analysis/forecasts_assess.Rmd",
        output_file = paste0(
          "forecasts_assess_",
          tproj,
          "_",
          twindow
        ),

        params = list(
          tproj = tproj,
          twindow = twindow,
          incid =
            all_files[[datasource]]$incidfile,
          n.dates.sim =
            n.dates.sim,
          place = place,
          outdir =
            all_files[[datasource]]$outdir,
          indir =
            all_files[[datasource]]$outdir
        )
      )
    } ## end of for
  }
)




## Consolidates metrics for all countries.
purrr::map(
  c(14, 28, 42),
  function(tw) {
    idx <- which(pars$twindow == tw)
    tproj <- pars$tproj[idx]
    for (p in places) {
      message("working on ", tw, " and ", p)
      rmarkdown::render("analysis/forecasts_metrics_consolidate.Rmd",
        params = list(
          twindow = tw,
          tproj = tproj,
          n.dates.sim = 28,
          place = p,
          indir =
            paste0(all_files[[datasource]]$outdir, "/metrics/weekly"),
          outdir =
            paste0(all_files[[datasource]]$outdir, "/metrics/weekly")
        )
      )
    }
  }
)

## Projections for each country for non-overlapping periods.
## give weekly incid for visualisation
purrr::walk(
  c(14, 28, 42),
  function(tw) {
    idx <- which(pars$twindow == tw)
    tproj <- pars$tproj[idx]
    ##  We want non-overlapping projections
    tproj <- seq(
      from = min(tproj),
      to = max(tproj),
      by = n.dates.sim
    )
    rmarkdown::render("analysis/projections_viz_fixed_country.Rmd",
      params = list(
        tproj = tproj,
        twindow = tw,
        incid = all_files[[datasource]]$weekly_incidfile,
        n.dates.sim = n.dates.sim,
        places = places,
        maxtproj = max(tproj),
        indir =
          all_files[[datasource]]$stanfits_dir,
        outdir =
          all_files[[datasource]]$outdir,
        datasource = datasource
      )
    )
  }
)

## Proportion in CrI each week.
purrr::pwalk(
  pars,
  function(tproj, twindow) {
    for (place in places) {
      rmarkdown::render(
        "analysis/prop_in_ci_by_week.Rmd",
        params = list(
          tproj = tproj,
          twindow = twindow,
          incid =
            all_files[[datasource]]$incidfile,
          n.dates.sim =
            n.dates.sim,
          place = place,
          outdir =
            all_files[[datasource]]$outdir,
          indir =
            all_files[[datasource]]$outdir
        )
      )
    } ## end of for
  }
)
