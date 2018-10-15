library(dplyr)
library(ggmcmc)
## twindow <- seq(from = 14, to = 42, by = 14)

twindow <- 42
## for (tw in twindow) {
##   t_proj = seq(from = 7 + tw,
##                to = 175,
##                by = 7)

##     pars <- rbind(pars, expand.grid(t_proj = t_proj,
##                                   time_window = tw))
## }
t_proj <- seq(from = 49,
              to = 245,
              by = 7)
pars <- expand.grid(t_proj = t_proj,
                    time_window = twindow)

incidfile <- "data/processed/promed_loglinear_wide_from_march.csv"
metadatafile <- "data/processed/all_african_centroids.csv"
apply(pars, 1, function(row) {
    ## prefix <- paste0(row[1], "_", row[2])
    ## fit1 <- here::here("data/stanfits",
    ##                    paste0(prefix, ".rds")) %>%
    ##     readr::read_rds()
    ## S <- ggmcmc::ggs(fit1)
    ## ggmcmc(S,
    ##        file = here::here("data/output",
    ##                          paste0(prefix,
    ##                                  ".pdf")))
    tproj <- row[1]
    twindow <- row[2]
    params2 = list(
        tproj = tproj,
        twindow = twindow,
        incid = incidfile,
        N = 55
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

})

apply(pars, 1, function(row) {
    tproj <- row[1]
    twindow <- row[2]
    params2 = list(
        tproj = tproj,
        twindow = twindow,
        incid = "data/processed/promed_wide allcountries_from_01july.csv"
      )
    params2$n.dates.sim <- 28
    params2$place <- "GIN"
    rmarkdown::render(
                   "analysis/forecasts_visual_check.Rmd",
                   params = params2
               )

})



## Now get the assessment metrics

pars <- data.frame(t_proj = c(),
                   time_window = c(),
                   place = c())
for (tw in twindow) {
  t_proj = seq(from = 2 * tw,
               to = 567,
               by = tw)
  pars <- rbind(pars,
                expand.grid(
                  t_proj = t_proj,
                  time_window = tw,
                  place = places
                ))
}

for (i in 1:nrow(pars)) {
  row <- pars[i,]
  message("Working on row ", row)

  tproj <- row$t_proj
  twindow <- row$time_window
  place <- as.character(row$place)

  rmarkdown::render(
    "analysis/forecasts_assess.Rmd",
    params = list(
      tproj = tproj,
      twindow = twindow,
      n.dates.sim = 28,
      incid = "healthmap_wide_from_july.csv",
      place = place
    )
  )
}



for (tw in twindow) {
  tproj <- seq(from = 2 * tw,
               to = 567,
               by = tw)
  for (place in places) {
      rmarkdown::render(
                     "analysis/forecasts_visual_check.Rmd",
                     params = list(
                         tproj = tproj,
                         twindow = tw,
                         n.dates.sim = 28,
                         incid = "healthmap_wide_from_july.csv",
                         place = place
                     )
                 )
  }
  ## rmarkdown::render(
  ##   "analysis/forecasts_assess_viz.Rmd",
  ##   params = list(
  ##     tproj = tproj,
  ##     twindow = tw,
  ##     n.dates.sim = 28,
  ##     incid = "healthmap_wide_from_july.csv",
  ##     place = "Guinea"
  ##   )
  ## )
}


apply (pars, 1 , function(row) {
  tproj <- row[1]
  twindow <- row[2]
  infile <- paste0(tproj, "_", twindow, ".rds")
  fit <- here::here("data/stanfits", infile) %>%
    readr::read_rds()
   rhats <- bayesplot::rhat(fit)
   if (any(rhats > 1.1)) message("Bad rhat ", infile)
  ##ratios_cp <- bayesplot::neff_ratio(fit)
  ##if (all(ratios_cp > 0.5))
  ##  message("Good neff ", infile)
})


## Read all files in a directory
