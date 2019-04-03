library(dplyr)
library(ggmcmc)
library(rstan)
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

indir <- "data/who_stanfits"
outdir <- "data/who_output"
nsim <- 1000
## Flow matrices
fitfiles <- list.files(
  path = indir,
  pattern = "^[0-9]*_[0-9]*.rds",
)

for (fit in fitfiles) {
    message("working on ", fit)
    fitobj <- readr::read_rds(here::here(indir, fit))
    list_of_draws <- rstan::extract(fitobj)
    nsamples <- length(list_of_draws[["gamma"]])
    selected <- sample(seq_len(nsamples), nsim)
    ## we want to sample all parameters jointly
    ## so save the indices to be used later.
    readr::write_rds(x = selected,
                     path = here::here(indir,
                                       paste0("idx_",
                                              fit)))


    gamma_samples <- list_of_draws[["gamma"]][selected]
    pstay_samples <- list_of_draws[["pstay"]][selected]

    l <- list(x = gamma_samples,
              y = pstay_samples,
              z = selected)
    purrr::pwalk(l,
                 function(x, y, z) {
                     df <- flow_mat(gamma = x,
                                    pstay = y)
                     readr::write_rds(x = df,
                                      path = here::here(indir,
                                                        paste("flow_matrices/flow",
                                                               z,
                                                              fit,
                                                              sep = "_")))
                   })

}

## Input parameters
## incidfile <- "data/processed/20122018_promed_loglinear_weekly.csv"
## incidfile <- "data/processed/20122018_promed_loglinear_wide.csv"
incidfile <- "data/processed/01032019_who_bycountry.csv"
metadatafile <- "data/processed/all_african_centroids.csv"
places <- c("LBR", "GIN", "SLE")
day0 <- readr::read_csv(incidfile,
                        n_max = 1) %>%
    pull(date)
twindows <- c(14, 28, 42)
n.dates.sim <- 28

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
  ##                 day0 = day0,
  ##                 indir = indir)
  ##   )

  rmarkdown::render(
    "analysis/projection_using_fitted.Rmd",
    params = list(tproj = tproj,
                  twindow = twindow,
                  n.dates.sim = n.dates.sim,
                  day0 = day0,
                  indir = indir,
                  outdir = outdir)
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
                                   place = place,
                                   outdir = outdir,
                                   indir = outdir)
                     )

  } ## end of for
}




## Consolidates metrics for all countries.
for (tw in twindows) {
    tproj <- seq(from = tw + 7,
              to = 427 - tw,
              by = 7)

    for (p in places) {
        message("working on ", tw, " and ", p)
        rmarkdown::render("analysis/forecasts_metrics_consolidate.Rmd",
                          params = list(twindow = tw,
                                        tproj = tproj,
                                        incid = incidfile,
                                        n.dates.sim = 28,
                                        place = p,
                                        indir = "data/who_output/metrics/weekly",
                                        outdir = "data/who_output/metrics/weekly"))
    }
}

## Projections for each country for non-overlapping periods.
incidfile <- "data/processed/01032019_who_bycountry_weekly.csv"
places <- c("LBR", "SLE", "GIN")
indir <- "data/who_stanfits"
outdir <- "data/who_output"
twindows <- c(14, 28, 42)
pars <- data.frame()
for (tw in twindows) {
    for (i in 1:4) {
        s1 <- seq(from = tw + (7 * i),
                  to = 427 - tw,
                  by = 28)
        maxtproj <- max(s1)
        pars <- rbind(pars, expand.grid(maxtproj = maxtproj,
                                        twindow = tw,
                                        tproj = list(s1)))
    }

}


for (row in seq_len(nrow(pars))) {
    rmarkdown::render("analysis/projections_viz_fixed_country.Rmd",
                       params = list(
                           tproj = pars$tproj[[row]],
                           twindow = pars$twindow[row],
                           incid = incidfile,
                           n.dates.sim = 28,
                           places = places,
                           maxtproj = pars$maxtproj[[row]],
                           indir = indir,
                           outdir = outdir

                       ))
}




## R quantiles
fitfiles <- list.files(
  path = "./data/who_stanfits/",
  pattern = "*_rquantiles_[0-9]*_[0-9]*.rds",
)

names(fitfiles) <- stringr::str_replace_all(fitfiles,
                                            ".rds",
                                            "")

rquantiles <- purrr::map_dfr(fitfiles,
                             function(x) {
                                 x <- here::here("data/who_stanfits",
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
                path = "data/processed/who_rquantiles_projection.csv")



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







rcategory <- readr::read_csv("data/processed/rquantiles_projection.csv")
rcategory <- filter(rcategory,
                    country %in% c("LBR", "SLE", "GIN"))

propinci <- list(`SLE` = "data/output/metrics/daily/SLE_prop_in_ci.csv",
                 `LBR` = "data/output/metrics/daily/LBR_prop_in_ci.csv",
                 `GIN` = "data/output/metrics/daily/GIN_prop_in_ci.csv") %>%
    map_dfr(~ readr::read_csv(.x, col_names = FALSE),
            .id = "country")

colnames(propinci) <- c("country",
                        "tproj",
                        "twindow",
                        "prop_in_ci")

propinci <- left_join(propinci,
                      rcategory,
                      by = c("tproj",
                             "twindow",
                             "country"))

propinci$ci <- factor(propinci$ci)
propinci$twindow <- factor(propinci$twindow)
propinci$country <- factor(propinci$country)

## Rename levels
propinci$country <- forcats::fct_recode(propinci$country,
                                        `Sierra Leone` = "SLE",
                                        Liberia = "LBR",
                                        Guinea = "GIN")

propinci$ci <- forcats::fct_recode(propinci$ci,
                                   Neither = "ci_includes_1",
                                   Growing = "ll_greater_than_1",
                                   Declining = "ul_less_than_1")
library(ggbeeswarm)
p <- filter(propinci, propinci$twindow == 14) %>%
    ggplot(
        aes(ci,
            prop_in_ci)) +
    geom_beeswarm() +
    theme_classic() +
    facet_wrap(~country, nrow = 3) +
    xlab("") +
    ylab("Proportion in 50% CrI")

ggsave("data/output/figures/promed_prop_in_ci_14.png", p)



## Model Selection
library(rstan)
fit_98_14 <- readr::read_rds("data/stanfits/98_14.rds")
fit_98_28 <- readr::read_rds("data/stanfits/98_28.rds")
## https://github.com/quentingronau/bridgesampling/issues/7
bridge.14 <- bridgesampling::bridge_sampler(fit_98_14)


## Phase of epidemic
rcategory <- readr::read_csv("data/processed/who_rquantiles_projection.csv")
rcategory <- filter(rcategory,
                    country %in% c("SLE",
                                   "GIN",
                                   "LBR"))
rcategory$country <- factor(rcategory$country)
rcategory$country <- forcats::fct_recode(rcategory$country,
                                         `Sierra Leone` = "SLE",
                                         Guinea = "GIN",
                                         Liberia = "LBR")

## Total trajectories in 95% CrI
prop_in_ci <- purrr::map_dfr(list(`Sierra Leone` = "SLE",
                                  Guinea = "GIN",
                                  Liberia = "LBR"),
                             function(x) {
                                 infile <- paste0("data/who_output/metrics/daily/",
                                                  x,
                                                  "_prop_in_ci.csv")
                                 df <- readr::read_csv(infile,
                                                       col_names = FALSE)
                                 colnames(df) <- c("tproj",
                                                   "twindow",
                                                   "prop")
                                 df

                             },
                             .id = "country")

prop_in_ci <- left_join(prop_in_ci,
                        rcategory)

df <- group_by(prop_in_ci,
               twindow,
               country) %>%
    summarise(`Proportion in 95% CrI` = mean(prop, na.rm = TRUE))

df <- tidyr::spread(df,
                    key = twindow,
                    value = `Proportion in 95% CrI`)

knitr::kable(df)

## By phase

df <- group_by(prop_in_ci,
               twindow,
               country,
               ci) %>%
    summarise(`Proportion in 95% CrI` = mean(prop, na.rm = TRUE))


df28 <- filter(df, twindow == 28) %>%
    ungroup() %>%
    select(-twindow)

df28 <- tidyr::spread(df28,
                      key = ci,
                      value = `Proportion in 95% CrI`)

df28 <- select(df28,
               country,
               "Neither" = ci_includes_1,
               "Growing" = ll_greater_than_1,
               "Declining" = ul_less_than_1)



######
library(bridgesampling)
is_zero <- function(col) {
    return(all(col == 0))
}


window <- c(14, 28, 42)
pars <- data.frame(tproj = c(),
                   twindow = c())
for (tw in window) {
  tproj = seq(from = tw + 7,
              to = 656 - tw,
              by = 7)
  pars <- rbind(pars, expand.grid(tproj = tproj,
                                  twindow = tw))

}


distances <- readRDS("data/processed/allafrica_distances.rds")
centroids <- read.csv(metadatafile)
lklhoods <- purrr::pmap(pars,
                        function(tproj, twindow) {
                            suffix <- paste(tproj, twindow, sep = "_")
                            suffix <- paste0(suffix,
                                             ".rds")
                            message("read ", suffix)
                            fit <- readr::read_rds(here::here("data/stanfits",
                                                              suffix))

                           ## collect data for stan
                           I <- readr::read_rds(here::here("data/stanfits",
                                                           paste0("incid_",
                                                                  suffix)))
                           zero_idx <- which(apply(I, 2, is_zero))
                           zero_incid <- length(zero_idx)

                           SI <- readr::read_rds(here::here("data/stanfits",
                                                            paste0("si_",
                                                                   suffix)))
                           rindex <- readr::read_rds(here::here("data/stanfits",
                                                                paste0("rindex_",
                                                                       suffix)))

                           standata <- list(T = tproj,
                                            N = 55,
                                            I = I,
                                            SI = SI,
                                            rindex = rindex,
                                            num_Rjt = max(rindex),
                                            population = centroids$pop,
                                            dist_mat = distances,
                                            alpha = 1,
                                            beta = 1,
                                            K = 1,
                                            prior_mean = 1,
                                            prior_std = 0.5,
                                            zero_incid = zero_incid,
                                            zero_idx = zero_idx
                                            )


                           new_mod <- stan(file = here::here("analysis/full_spatial_model_optimised.stan"),
                                           data = standata,
                                           chains = 0)
                           bridgesampling::bridge_sampler(fit, new_mod)
})
