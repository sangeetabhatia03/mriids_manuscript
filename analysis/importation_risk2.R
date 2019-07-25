## Approach 2
infectivity <- function(tproj, tw, idx) {
    suffix <- glue::glue("{tproj}_{tw}.rds")
    indir <- all_files[[datasource]]$stanfits_dir

    si_suffix <- glue::glue("si_{tproj}_{tw}.rds")
    si <- readr::read_rds(here::here(indir, si_suffix))

    ## length of si is 1 more than T because si starts at 0.
    ## see how data are prepared to be fed into stan for clarity
    si <- si[-1]

    incidence <- readr::read_rds(
        here::here(
            all_files[[datasource]]$stanfits_dir,
            glue::glue("incid_{tproj}_{tw}.rds")
         )
    )
    countries <- colnames(incidence)
    infiles <- glue::glue("{countries}_rsamples_{suffix}")
    infiles <- here::here(indir, infiles)
    names(infiles) <- countries


    rt_vec <- purrr::map(infiles, readr::read_rds)
    ## Need to add 3 to the column index to account for date, country
    ## and var columns
    rt_vec <-  purrr::map(rt_vec, function(x) x[nrow(x), idx + 3])

    si <- matrix(si, nrow = 1)
    I <- as.matrix(incidence, nrow =  nrow(incidence))
    infectivity <- rev(si) %*%  I
    infectivity
}

reshape_matrix <- function(out) {
    colnames(out) <- rownames(out)
    out <- data.frame(out)
    out <- tibble::rownames_to_column(out, var = "risk_from")
    out <- tidyr::gather(out, key = "risk_to", value = "risk", -risk_from)
    out

}

relative_flow <- function(inf) {
    ## Reverse the multiplication by 1 - pstay.
    flow_mat <- readr::read_rds(inf)
    pstay <- diag(flow_mat)[1]
    diag(flow_mat) <- 0
    flow_mat <- flow_mat / (1 - pstay)
    flow_mat
}

normalise_cols <- function(flow) {
    for (col in 1:ncol(flow)) {
        if (sum(flow[,col]) > 0) flow[, col] <- flow[, col] / sum(flow[,col])
    }
    flow
}

##importation_risk <- function(tproj, tw) {
selected <-read_rds(
    path = here(
        all_files[[datasource]]$stanfits_dir,
        glue::glue("idx_{tproj}_{twindow}.rds")
    )
)
indir <- glue::glue(
   "{all_files[[datasource]]$stanfits_dir}/flow_matrices"
)
infiles <- glue::glue("flow_{selected}_{tproj}_{twindow}.rds")
infiles <- here::here(indir, infiles)
rel_flow <- purrr::map(infiles, relative_flow)
inftvty <- purrr::map(selected, ~ infectivity(tproj, twindow, .x))
rel_risk <- purrr::map2(
    rel_flow,
    inftvty,
    function(relf, inf) {
        out <- relf %*% diag(inf[1, ])
        out
      }
    )
rel_risk <- purrr::map(rel_risk, normalise_cols)
rel_risk <- purrr::map(rel_risk, reshape_matrix)



inftvty <- do.call(rbind, inftvty)
outfile <- glue::glue("{datasource}_infectivity_{tproj}_{twindow}.csv")
outfile <- here::here(all_files[[datasource]]$outdir, outfile)
readr::write_csv(x = rel_risk, path = outfile)

rel_risk <- do.call(rbind, rel_risk)
outfile <- glue::glue("{datasource}_importation_risk_{tproj}_{twindow}.csv")
outfile <- here::here(all_files[[datasource]]$outdir, outfile)
readr::write_csv(x = rel_risk, path = outfile)

out <- dplyr::group_by(
    rel_risk,
    risk_from,
    risk_to) %>%
    dplyr::summarise(
        low = quantile(risk, 0.025),
        med = quantile(risk, 0.5),
        high = quantile(risk, 0.975)
        )
outfile <- glue::glue("{datasource}_importation_risk_quantiles_{tproj}_{twindow}.csv")
outfile <- here::here(all_files[[datasource]]$outdir, outfile)
message("Writing to ", outfile)
readr::write_csv(x = out, path = outfile)
