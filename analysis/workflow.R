library(dplyr)
library(ggmcmc)

fitfiles <- list.files(path = ".",,
                       pattern = "^[0-9]*_[0-9]*.rds",
                       full.names = FALSE)

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

## From main dir. Extract tproj and twindow from the
## name of the fit object.

## Work on latest files only.
fitfiles <- list.files(
  path = "./data/stanfits/",
  pattern = "^[0-9]*_[0-9]*.rds",
)

## mtime <- purrr::map(fitfiles, ~ file.mtime(paste0("data/stanfits/", .x)))
## latest <- which(mtime >= as.POSIXct("2018-11-26"))

fitfiles <- fitfiles %>%
  stringr::str_replace(".rds", "")

incidfile <- "data/processed/promed_loglinear_wide_from_march_ghana_fixed.csv"
metadatafile <- "data/processed/all_african_centroids.csv"
places <- c("LBR", "GIN", "SLE")
for (fit in fitfiles) {
  message("###############################")
  message("Working on ", fit)
  tproj <- strsplit(fit, split = "_")[[1]][1] %>%
    as.numeric()
  twindow <- strsplit(fit, split = "_")[[1]][2] %>%
      as.numeric()
  message("working on ", tproj, "_", twindow)
  params2 <- list(
    tproj = tproj,
    twindow = twindow,
    incid = incidfile
  )
  rmarkdown::render(
    "analysis/extract_mcmc_draws.Rmd",
    params = params2
  )
  params2$n.dates.sim <- 28
  params2$centroids <- metadatafile
  rmarkdown::render(
    "analysis/projection_using_fitted.Rmd",
    params = params2
  )

  ## rmarkdown::render(
  ##   "analysis/projections_viz_fixed_tproj.Rmd",
  ##   params = list(tproj = tproj,
  ##                 twindow = twindow,
  ##                 incid = incidfile,
  ##                 n.dates.sim = 28,
  ##                 place = places)
  ## )

  for (place in places) {
    ## rmarkdown::render(
    ##   "analysis/projections_viz_fixed_country.Rmd",
    ##   params = list(tproj = tproj,
    ##                 twindow = twindow,
    ##                 incid = incidfile,
    ##                 n.dates.sim = 28,
    ##                 place = place)
    ## )

    rmarkdown::render(
      "analysis/forecasts_assess.Rmd",
      params = list(tproj = tproj,
                    twindow = twindow,
                    incid = incidfile,
                    n.dates.sim = 28,
                    place = place)
    )
  } ## end of for
}

## Check for existence of files

twindows <- c(14, 28, 42)
pars <- data.frame(tproj = c(),
                   twindow = c())
for (tw in twindows) {
    tproj = seq(from = tw + 7,
                 to = 656 - tw,
                by = 7)
    for (tp in tproj) {
        outfile <- paste0("forecasts_",
                          tp,
                          "_",
                          tw,
                          "_28.csv")
        outfile <- here::here("data/output",
                              outfile)
        if (! file.exists(outfile)) {
            message(outfile, " does not exist.")
            pars <- rbind(pars,
                          data.frame(tproj = tp,
                                     twindow = tw))
        }
    }

}

## Consolidate metrics for all countries.
for (tw in twindows) {
    tproj <- seq(from = tw + 7,
              to = 656 - tw,
              by = 7)

    for (p in places) {
        message("working on ", tw, " and ", p)
        rmarkdown::render("analysis/forecasts_metrics_consolidate.Rmd",
                          params = list(twindow = tw,
                                        tproj = tproj,
                                        incid = incidfile,
                                        n.dates.sim = 28,
                                        place = p))
    }


}
## Projections for each country for non-overlapping periods.
pars <- data.frame()
for (tw in twindows) {
    for (i in 1:4) {
        s1 <- seq(from = tw + (7 * i),
                  to = 656 - tw,
                  by = 28)
        pars <- rbind(pars, expand.grid(twindow = tw,
                                        tproj = list(s1)))
    }

}

for (place in places) {
  for (row in seq_len(nrow(pars))) {
    rmarkdown::render("analysis/projections_viz_fixed_country.Rmd",
                       params = list(
                           tproj = pars$tproj[[row]],
                           twindow = pars$twindow[row],
                           incid = incidfile,
                           n.dates.sim = 28,
                           place = place

                       ))
  }
}

