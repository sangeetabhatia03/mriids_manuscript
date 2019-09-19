## Retrieve the set of parameters that maximises lp__
max_lp_params <- function(fit) {
    draws <- rstan::extract(fit)
    idx <- which.max(draws[["lp__"]])
    draws[["R"]] <- matrix(draws[["R"]][idx, ], nrow = 1)
    draws[["gamma"]] <- draws[["gamma"]][idx]
    draws[["pstay"]] <- draws[["pstay"]][idx]
    draws[["lp__"]] <- draws[["lp__"]][idx]
    draws[["max_idx"]] <- idx
    draws

}

## Infectivity at each location at given tproj.
infectivity <- function(tproj, tw, idx) {
    suffix <- glue::glue("{tproj}_{tw}.rds")
    indir <- all_files[[datasource]]$stanfits_dir

    si_suffix <- glue::glue("si_{tproj}_{tw}.rds")
    si <- readr::read_rds(here::here(indir, si_suffix))

    ## length of si is 1 more than T because si starts at 0.
    ## see how data are prepared to be fed into stan for clarity
    si <- si[-1]

    incidence <- readr::read_csv(all_files[[datasource]]$incidfile,
                                 n_max = tproj)
    countries <- colnames(incidence)[-1]
    infiles <- glue::glue("{countries}_rsamples_{suffix}")
    infiles <- here::here(indir, infiles)
    names(infiles) <- countries


    rt_vec <- purrr::map(infiles, readRDS)
    ## Need to add 3 to the column index to account for date, country
    ## and var columns
    rt_vec <-  purrr::map(rt_vec, function(x) x[nrow(x), idx + 3])

    si <- matrix(si, nrow = 1)
    I <- as.matrix(incidence[ , -1], nrow =  nrow(incidence))
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

indir <- all_files[[datasource]]$stanfits_dir
outdir <- all_files[[datasource]]$outdir
for (tproj in tprojs) {
    for (tw in twindows) {
        suffix <- glue::glue("{tproj}_{tw}.rds")
        message("Reading ", suffix)
        fit <- readRDS(here::here(indir, suffix))
        draws <- max_lp_params(fit)
        message("Extracted max lp parameters")
        ## This will give pstay * flow(i, j)/ sum_x(flow(i, x))
        ## df[i, j] is the  flow into  j from i relative to the overall flow
        ## i.
        rel_flow <- flow_mat2(
            gamma = draws[["gamma"]],
            pstay = draws[["pstay"]],
            metadatafile = all_files[[datasource]]$metadatafile,
            dist_obj = all_files[[datasource]]$dist_obj,
            countries_col = "ISO3",
            pop_col = "pop"
        )
        diag(rel_flow) <- 0
        inftvty <- infectivity(tproj, tw, draws[["max_idx"]])
        out <- rel_flow %*% diag(inftvty[1, ])
        ## Normalise columns for ease of interpretaibility.
        for (col in 1:ncol(out)) {
            if (sum(out[,col]) > 0) out[, col] <- out[, col] / sum(out[,col])
        }
        out <- reshape_matrix(out)
        outfile <- glue::glue("{datasource}_importation_risk_{tproj}_{tw}.csv")
        outfile <- here::here(outdir, outfile)
        message("Writing to ", outfile)
        readr::write_csv(x = out, path = outfile)
    }
}



