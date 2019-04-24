library(dplyr)
library(ggmcmc)
library(rstan)
fitfiles <- list.files(
  path = ".", ,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

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
source(here::here("analysis/parameters.R"))
day0 <- readr::read_csv(here::here(all_files[[datasource]]$incidfile),
  n_max = 1
) %>%
  pull(date)

fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
)


## Use these to construct the set of parameters.
## tidyr will throw warnings as fitfiles has other stuff.
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
## Flow matrices
source("analysis/flow_matrix_estim.R")
for (fit in fitfiles) {
  message("working on ", fit)
  fitobj <-
    readr::read_rds(here::here(
      all_files[[datasource]]$stanfits_dir,
      fit
      ))
  if (! inherits(fitobj, "stanfit")) {
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

fitfiles <- stringr::str_remove(fitfiles, ".rds")


library(tictoc)
library(furrr)
plan(multiprocess)
tic()
furrr::future_map(
  fitfiles,
  function(fit) {
    message("###############################")
    message("Working on ", fit)
    tproj <- strsplit(fit, split = "_")[[1]][1] %>%
      as.numeric()
    twindow <- strsplit(fit, split = "_")[[1]][2] %>%
      as.numeric()
    message("working on ", tproj, "_", twindow)

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
        output_file = paste0(
          "projection_using_fitted_",
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

toc()


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
          incid = incidfile,
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
purrr::map(
  twindows,
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
        incid = weekly_incidfile,
        n.dates.sim = n.dates.sim,
        places = places,
        maxtproj = max(tproj),
        indir =
          all_files[[datasource]]$stanfits_dir,
        outdir =
          all_files[[datasource]]$outdir,
        datasource = "WHO"
      )
    )
  }
)




## R quantiles
fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "*_rquantiles_[0-9]*_[0-9]*.rds",
)

names(fitfiles) <- stringr::str_replace_all(
  fitfiles,
  ".rds",
  ""
)

rquantiles <- purrr::map_dfr(fitfiles,
  function(x) {
    x <- here::here(
      "data/who_stanfits",
      x
    )
    out <- readr::read_rds(x)
    out <- slice(out, n())
    out
  },
  .id = "params"
)


rquantiles <- tidyr::separate(rquantiles,
  params,
  into = c(
    "country",
    "what",
    "tproj",
    "twindow"
  ),
  sep = "_"
)

rquantiles <- select(
  rquantiles,
  -what,
  -var
)


## Affix a column indicating if the ll of the 95%
## CI is greater than 1, the ul is less than 1 or
## if the CI straddles 1.
rquantiles <- mutate(rquantiles,
  ci = case_when(
    `2.5%` > 1 ~ "ll_greater_than_1",
    `97.5%` < 1 ~ "ul_less_than_1",
    TRUE ~ "ci_includes_1"
  )
)


readr::write_csv(
  x = rquantiles,
  path = "data/processed/who_rquantiles_projection.csv"
)


rcategory <- readr::read_csv("data/processed/rquantiles_projection.csv")
rcategory <- filter(
  rcategory,
  country %in% c("LBR", "SLE", "GIN")
)

propinci <- list(
  `SLE` = "data/output/metrics/daily/SLE_prop_in_ci.csv",
  `LBR` = "data/output/metrics/daily/LBR_prop_in_ci.csv",
  `GIN` = "data/output/metrics/daily/GIN_prop_in_ci.csv"
) %>%
  map_dfr(~ readr::read_csv(.x, col_names = FALSE),
    .id = "country"
  )

colnames(propinci) <- c(
  "country",
  "tproj",
  "twindow",
  "prop_in_ci"
)

propinci <- left_join(propinci,
  rcategory,
  by = c(
    "tproj",
    "twindow",
    "country"
  )
)

propinci$ci <- factor(propinci$ci)
propinci$twindow <- factor(propinci$twindow)
propinci$country <- factor(propinci$country)

## Rename levels
propinci$country <- forcats::fct_recode(propinci$country,
  `Sierra Leone` = "SLE",
  Liberia = "LBR",
  Guinea = "GIN"
)

propinci$ci <- forcats::fct_recode(propinci$ci,
  Neither = "ci_includes_1",
  Growing = "ll_greater_than_1",
  Declining = "ul_less_than_1"
)
library(ggbeeswarm)
p <- filter(propinci, propinci$twindow == 14) %>%
  ggplot(
    aes(
      ci,
      prop_in_ci
    )
  ) +
  geom_beeswarm() +
  theme_classic() +
  facet_wrap(~country, nrow = 3) +
  xlab("") +
  ylab("Proportion in 50% CrI")

ggsave("data/output/figures/promed_prop_in_ci_14.png", p)





## Phase of epidemic ProMED
rcategory <- readr::read_csv("data/processed/rquantiles_projection.csv")
rcategory <- filter(
  rcategory,
  country %in% c(
    "SLE",
    "GIN",
    "LBR"
  )
)
rcategory$country <- factor(rcategory$country)
rcategory$country <- forcats::fct_recode(rcategory$country,
  `Sierra Leone` = "SLE",
  Guinea = "GIN",
  Liberia = "LBR"
)

## Total trajectories in 95% CrI
prop_in_ci <- purrr::map_dfr(list(
  `Sierra Leone` = "SLE",
  Guinea = "GIN",
  Liberia = "LBR"
),
function(x) {
  infile <- paste0(
    "data/output/metrics/daily/",
    x,
    "_prop_in_ci.csv"
  )
  df <- readr::read_csv(infile,
    col_names = FALSE
  )
  colnames(df) <- c(
    "tproj",
    "twindow",
    "prop"
  )
  df
},
.id = "country"
)

prop_in_ci <- left_join(
  prop_in_ci,
  rcategory
)

df <- group_by(
  prop_in_ci,
  twindow,
  country
) %>%
  summarise(`Proportion in 95% CrI` = mean(prop, na.rm = TRUE))

df <- tidyr::spread(df,
  key = twindow,
  value = `Proportion in 95% CrI`
)

knitr::kable(df)

## By phase

df <- group_by(
  prop_in_ci,
  twindow,
  country,
  ci
) %>%
  summarise(
    `Proportion in 95% CrI` =
      mean(prop, na.rm = TRUE)
  )


df28 <- filter(
  df,
  twindow == 28
) %>%
  ungroup() %>%
  select(-twindow)

df28 <- tidyr::spread(df28,
  key = ci,
  value = `Proportion in 95% CrI`
)

df28 <- select(df28,
  country,
  "Neither" = ci_includes_1,
  "Growing" = ll_greater_than_1,
  "Declining" = ul_less_than_1
)

## Save it so that we can render it directly in results.
readr::write_csv(
  x = df28,
  path = "data/processed/promed_propinci_byphase.csv"
)


######


#### For a given time window, number of weeks with predicted
#### non-zero cases. Similarly, number of weeks with non-zero observed
#### cases.

forecasts_files <- list.files(
  path = all_files[[datasource]]$outdir,
  pattern = paste0(
    "^forecasts_[0-9]*_",
    twindow,
    "_",
    n.dates.sim,
    ".csv"
  )
)
names(forecasts_files) <- stringr::str_replace(
  forecasts_files,
  "forecasts_",
  ""
) %>%
  stringr::str_replace(".csv", "")

all_forecasts <- purrr::map_dfr(forecasts_files,
  ~ readr::read_csv(here::here(
    all_files[[datasource]]$outdir,
    .x
  )),
  .id = "parameters"
)

all_forecasts <- tidyr::separate(all_forecasts,
  col = parameters,
  into = c(
    "tproj",
    "time_window",
    "n.dates.sim"
  ),
  convert = TRUE
)



all_forecasts <- arrange(all_forecasts, tproj)
all_forecasts$week_of_year <-
    paste(lubridate::year(all_forecasts$date),
          "Week",
          lubridate::week(all_forecasts$date))

## Save this for mapping later.
readr::write_csv(x = all_forecasts,
                 path = here::here(all_files[[datasource]]$outdir,
                                   paste0("all_forecasts_",
                                          twindow,
                                          "_",
                                          n.dates.sim,
                                          ".csv")))

pm_predicted_nonzero <-
  group_by(all_forecasts, country) %>%
  summarise(
    non_zero_y = sum(y > 0),
    non_zero_ymin = sum(ymin > 0),
    non_zero_ymax = sum(ymax > 0)
  )


readr::write_csv(
  x = pm_predicted_nonzero,
  path = here::here(
    all_files[[datasource]]$outdir,
    paste0("weeks_with_nonzero_predicted_",
           twindow,
           "_",
           n.dates.sim,
           ".csv")
  )
)
