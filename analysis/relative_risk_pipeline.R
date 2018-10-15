## ----setup

library(dplyr)
library(purrr)
library(mRIIDS)
sources <- c("GIN", "LBR", "SLE")
tproj <- 245
twindow <- 42
prefix <- paste0(tproj, "_", twindow, ".rds")
## Read in the parameter values from the fitted model
fit <- readr::read_rds(here::here("data/stanfits/",
                                  prefix))
fit_summary <- rstan::summary(fit)
gamma <- fit_summary$summary["gamma", "mean"]

## Getting R values is a little more involved
## because of the mapping to index etc.
## Read in the quantiles and then get median values.
rfiles <- paste0(sources,
                 "_rquantiles_",
                 prefix)
rfiles <- here::here("data/stanfits", rfiles)
names(rfiles) <- sources
restimates <- map(rfiles,
                  ~ readr::read_rds(.x) %>%
                    select(`50%`) %>%
                    tail(1))

## Read in the serial interval and incidence
## used to fit the models.
si <- readr::read_rds(here::here("data/stanfits",
                                 paste0("si_", prefix)))
incid <- readr::read_rds(here::here("data/stanfits",
                                    paste0("incid_", prefix)))


## for each country estimate the flow matrix using the
## median gamma value.
params <-
  list(centroids = "data/processed/all_african_centroids.csv",
       model = "gravity",
       modelpars = list(K = 1,
                        pow_N_from = 1,
                        pow_N_to = 1,
                        pow_dist = gamma))


## ------------------------------------------------------------------------
centroids <- here::here(params$centroids) %>%
  readr::read_csv()

## ----flowmat, eval = TRUE------------------------------------------------


distances <- geosphere::distm(cbind(centroids$lon,
                                    centroids$lat))
distances <- as.matrix(distances)
distvec <- distances[lower.tri(distances)]

## ----pops----------------------------------------------------------------
idx <- mRIIDS:::lower_tri_idx(nrow(distances))
n_from <- centroids$pop[idx[, 1]]
n_to <- centroids$pop[idx[, 2]]


## ------------------------------------------------------------------------
flow_from_to <- flow_matrix(
  distvec,
  n_from,
  n_to,
  place_names = centroids$ISO3,
  model = params$model,
  params = params$modelpars
)


## Normalising Relative Flow
for (i in seq_len(nrow(flow_from_to))) {
  flow_from_to[i, ] <- flow_from_to[i, ] /
    (sum(flow_from_to[i, ],
         na.rm = TRUE))
}


## Now estimate the relative flow from each source.
relative_risk <- flow_from_to
rel_risk_df <- tibble::rownames_to_column(data.frame(relative_risk),
                                          var = "flow_from")

outfile_suffix <- paste(sapply(params$modelpars, paste, collapse=""),
                        collapse = "_")
for (s in sources) {
  from_source <- filter(rel_risk_df,
                        flow_from == s) %>%
      tidyr::gather(flow_to, relative_flow, -flow_from)
  outfile_prefix <- paste0("flow_from_", s, "_")

    here::here("data/output",
              paste0(outfile_prefix, outfile_suffix, ".csv")) %>%
      readr::write_csv(x = from_source,
                       path = .)
}

## Now estimate relative risk of spread.

## ----Importation Risk--------------------------------------------------
riskfiles <- here::here("data/output",
                        paste0("flow_from_",
                               sources,
                               "_",
                               outfile_suffix,
                               ".csv"))
names(riskfiles) <- sources
params <-
  list(risk = riskfiles,
       R = restimates,
       outfile = "data/output/weighted_flow.csv")

incid <- select(incid, sources)
weights <-  map2_dbl(incid,
                     restimates,
                     ~mRIIDS:::infectivity_at_source(.x,
                                                     si,
                                                     .y$`50%`) %>%
                         tail(1))


## ------------------------------------------------------------------------

normalised <- weights / sum(weights)

## ----relrisk, eval = TRUE------------------------------------------------

relrisk_df <- purrr::map(params$risk,
                         ~ readr::read_csv(.x))


## ----wtdrelrisk, eval = TRUE---------------------------------------------
## signf <- 7
## normalised <- signif(normalised, 7)


## wtd_rel_risk <- (normalised[1]  * rel_risk[[1]]$relative_flow) +
##     (normalised[2] * rel_risk[[2]]$relative_flow)

rel_risk <- map_dfc(relrisk_df, ~ .x$relative_flow)
wtd_rel_risk <- map2_dfc(rel_risk,
                         normalised,
                         ~ .x * .y) %>%
  rowSums(na.rm = TRUE)

wtd_risk_profile <- data.frame(ISO3c = relrisk_df[[1]]$flow_to,
                               wtd_rel_risk = wtd_rel_risk)

wtd_risk_profile$country <-
    countrycode::countrycode(wtd_risk_profile$ISO3c,
                             "iso3c",
                             "country.name")
wtd_risk_profile <- select(wtd_risk_profile,
                           country,
                           ISO3c,
                           wtd_rel_risk)
## ----wtdrelriskout, eval = TRUE------------------------------------------
readr::write_csv(x = wtd_risk_profile,
                 path = here::here(params$outfile))
