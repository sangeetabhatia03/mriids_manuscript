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

## Input parameters
incidfile <- "data/processed/20122018_promed_loglinear_wide.csv"
metadatafile <- "data/processed/all_african_centroids.csv"
places <- c("LBR", "GIN", "SLE")
day0 <- readr::read_csv(incidfile,
                        n_max = 1) %>%
    pull(date)
twindows <- c(14, 28, 42)
n.dates.sim <- 28

## From main dir. Extract tproj and twindow from the
## name of the fit object.


fitfiles <- list.files(
  path = "./data/stanfits/",
  pattern = "^[0-9]*_[0-9]*.rds",
)

## Work on latest files only.
mtimes <- purrr::map(fitfiles,
                     ~ file.mtime(here::here("data/stanfits",
                                             .x)) %>% as.Date)

idx <- purrr::map(mtimes, ~ .x >= as.Date("2018-12-28"))
fitfiles <- fitfiles[unlist(idx)]

fitfiles <- stringr::str_replace(fitfiles,
                                 ".rds",
                                 "")



for (fit in fitfiles) {
  message("###############################")
  message("Working on ", fit)
  tproj <- strsplit(fit, split = "_")[[1]][1] %>%
    as.numeric()
  twindow <- strsplit(fit, split = "_")[[1]][2] %>%
      as.numeric()
  message("working on ", tproj, "_", twindow)

  ## rmarkdown::render(
  ##   "analysis/extract_mcmc_draws.Rmd",
  ##   params = list(tproj = tproj,
  ##                 twindow = twindow,
  ##                 day0 = day0)
  ##   )

  rmarkdown::render(
    "analysis/projection_using_fitted.Rmd",
    params = list(tproj = tproj,
                  twindow = twindow,
                  n.dates.sim = n.dates.sim,
                  day0 = day0)
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
                                   n.dates.sim = n.dates.sim,
                                   place = place)
                     )

  } ## end of for
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
        maxtproj <- 7 * floor((656 - tw) / 7)
        pars <- rbind(pars, expand.grid(maxtproj = maxtproj,
                                        twindow = tw,
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
                           place = place,
                           maxtproj = pars$maxtproj[[row]]

                       ))
  }
}



## R quantiles
fitfiles <- list.files(
  path = "./data/stanfits/",
  pattern = "*_rquantiles_[0-9]*_[0-9]*.rds",
)

names(fitfiles) <- stringr::str_replace_all(fitfiles,
                                            ".rds",
                                            "")

rquantiles <- purrr::map_dfr(fitfiles,
                             function(x) {
                                 x <- here::here("data/stanfits",
                                                 x)
                                 out <- readr::read_rds(x)
                                 out <- slice(out, n())
                                 out
                             },
                             .id = "params")


rquantiles <- tidyr::separate(rquantiles,
                              params,
                              into = c("country",
                                       "what",
                                       "tproj",
                                       "twindow"),
                              sep = "_")

rquantiles <- select(rquantiles,
                     -what,
                     -var)


## Affix a column indicating if the ll of the 95%
## CI is greater than 1, the ul is less than 1 or
## if the CI straddles 1.
rquantiles <- mutate(rquantiles,
                     ci = case_when(
                         `2.5%` > 1 ~ "ll_greater_than_1",
                         `97.5%` < 1 ~ "ul_less_than_1",
                         TRUE  ~ "ci_includes_1"
                     ))


readr::write_csv(x = rquantiles,
                path = "data/processed/rquantiles_projection.csv")



##
fitfiles <- list.files(
  path = "./data/stanfits/",
  pattern = "^[0-9]*_[0-9]*.rds",
)

fits <- purrr::map(fitfiles,
                   ~ readr::read_rds(here::here("data/stanfits",
                                                .x)))

fitfiles <- stringr::str_replace_all(fitfiles,
                                     ".rds",
                                     "")

outdir <- "data/stanfits/flow_matrices/"

for (i in 1:length(fits)) {
    prefix <- fitfiles[[i]]
    infile <- here::here(outdir,
                         paste0("flow_900_",
                                prefix,
                                ".rds"))

    if (! file.exists(infile)) {
        message("running for ", i, " and ", prefix)
        res <- flow_mat_samples(i,
                                fits = fits,
                                nsim = 30)

        message("flow matrices for ", i)
        message("dumping them at ", prefix)
        outfiles <- paste0(outdir,
                           "flow_",
                           seq_along(res),
                           "_",
                           prefix,
                           ".rds")

        purrr::walk2(res,
                     outfiles,
                     ~ readr::write_rds(x = .x,
                                        path = .y))

    }

}


## 28-12-2018
## Some jobs are still running.
## Copying previous fits into current dir.
df <- readr::read_csv("data/stanfits/still_running.csv")
prefix <- paste0("*",
                 df$tproj,
                 "_",
                 df$twindow,
                 "*")

oldfiles <- purrr::map(prefix, ~ list.files(
                                  path = "./data/stanfits.25122018/",
                                  pattern = .x,
                                  ))


file.copy(from = paste0("data/stanfits.25122018/",
                        unlist(oldfiles)),
                        to = "data/stanfits/")


oldflowmats <- purrr::map(prefix, ~ list.files(
                                     path = "./data/stanfits.25122018/flow_matrices/",
                                     pattern = .x,
                                     ))


file.copy(from = paste0("data/stanfits.25122018/flow_matrices/",
                        unlist(oldflowmats)),
          to = "data/stanfits/flow_matrices/")



