## Weights
r_samples <- function(infiles, idx) {
  message("Reading r samples")
  rt_vec <- purrr::map(infiles, readRDS)
  ## Need to add 3 to the column index to account for date, country
  ## and var columns
  rt_vec <- purrr::map_dfr(rt_vec, function(x) x[nrow(x), idx + 3])
  rt_vec
}

si_distr <- function(suffix) {
    si_suffix <- glue::glue("si_{suffix}")
    si <- readRDS(here::here(indir, si_suffix))
  ## length of si is 1 more than T because si starts at 0.
  ## see how data are prepared to be fed into stan for clarity
    si <- si[-1]
    si <- matrix(si, nrow = 1)
    si
}

infectivity <- function(R, lambda) {
  out <- R * lambda
  out
}

indir <- all_files[[datasource]]$stanfits_dir
suffix <- glue::glue("{tproj}_{twindow}.rds")
selected <- readRDS(
  here::here(
    all_files[[datasource]]$stanfits_dir,
    glue::glue("idx_{suffix}")
  )
 )
message("Read selected indices")
## Incidence
incidence <- readRDS(
    here::here(
      all_files[[datasource]]$stanfits_dir,
      glue::glue("incid_{suffix}")
    )
)

## SI
si <- si_distr(suffix)

## R samples
countries <- colnames(incidence)
infiles <- glue::glue("{countries}_rsamples_{suffix}")
infiles <- here::here(indir, infiles)
names(infiles) <- countries
message("Reading r samples")
rt <- r_samples(infiles, selected)

I <- as.matrix(incidence, nrow = nrow(incidence))
lambda <- rev(si) %*% I

inftvty <- apply(
    X = rt,
    MARGIN = 2,
    FUN = function(x) infectivity(R = x, lambda = lambda)
)
## At this point, inftvty is a 55 * 1000 matrix
## where m[i, j] is the jth sample of infectivity at i.

## We will now obtain relative flows where
## M[i, j] is the relative flow from i into j.

## Flows
relative_flow <- function(inf) {
  ## Reverse the multiplication by 1 - pstay.
  flow_mat <- readRDS(inf)


  pstay <- diag(flow_mat)[1]
  diag(flow_mat) <- 0
  flow_mat <- flow_mat / (1 - pstay)
  flow_mat
}

indir <- glue::glue(
  "{all_files[[datasource]]$stanfits_dir}/flow_matrices"
)
infiles <- glue::glue("flow_{selected}_{suffix}")
infiles <- here::here(indir, infiles)
rel_flow <- purrr::map(infiles, relative_flow)
message("Calculated relative flow")

## We have 1000 flow matrices, and 1000 samples of
## infectivity
inftvty_df <- data.frame(inftvty)
rel_risk <- purrr::map2(
    inftvty_df,
    rel_flow,
    function(x, y) x * y
  )

## out has 1000 matrices that are relative flows weighted by
## infectivity at source. Normalise at the point of plotting.



## Output
reshape_matrix <- function(out) {
  colnames(out) <- rownames(out)
  out <- data.frame(out)
  out <- tibble::rownames_to_column(out, var = "risk_from")
  out <- tidyr::gather(out, key = "risk_to", value = "risk", -risk_from)
  out
}

rel_risk <- purrr::map(rel_risk, reshape_matrix)

outfile <- glue::glue("{datasource}_infectivity_{suffix}")
outfile <- here::here(all_files[[datasource]]$outdir, outfile)
message("Writing to ", outfile)
readr::write_rds(x = inftvty, path = outfile)

rel_risk <- do.call(rbind, rel_risk)
outfile <- glue::glue("{datasource}_importation_risk_{tproj}_{twindow}.csv")
outfile <- here::here(all_files[[datasource]]$outdir, outfile)
message("Writing to ", outfile)
readr::write_csv(x = rel_risk, path = outfile)

out <- dplyr::group_by(
  rel_risk,
  risk_from,
  risk_to
) %>%
  dplyr::summarise(
    low = quantile(risk, 0.025),
    med = quantile(risk, 0.5),
    high = quantile(risk, 0.975)
  )
outfile <- glue::glue(
    "{datasource}_importation_risk_quantiles_{tproj}_{twindow}.csv"
)
outfile <- here::here(all_files[[datasource]]$outdir, outfile)
message("Writing to ", outfile)
readr::write_csv(x = out, path = outfile)
