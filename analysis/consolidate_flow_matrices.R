## Collect all flow matrices for a given
## tproj and given twindow

## flow_files - list of names of files.
consolidate_flow_matrices <- function(flow_files) {
  flow_matrices <- purrr::map(
    flow_files,
    ~ readr::read_rds(.x)
  )
  flow_matrices <- purrr::map(
    flow_matrices,
    ~ tibble::rownames_to_column(data.frame(.x),
      var = "from"
    )
  )

  flow_matrices <- do.call(rbind, flow_matrices)
  flow_tall <- tidyr::gather(flow_matrices,
    key = "to",
    value = "relative_flow",
    -from
  )

  flow_tall
}

library(dplyr)
nsim <- 900
twindow <- c(14, 42)
for (tw in twindow) {
  tproj <- seq(
    from = tw + 7,
    to = 656 - tw,
    by = 7
  )
  for (tp in tproj) {
    message("working on ", tp, "_", tw)
    flow_files <- paste0(
      "data/stanfits/flow_matrices/",
      tp,
      "_",
      tw,
      "_flow_",
      seq_len(nsim),
      ".rds"
    )
    flow_files <- here::here(flow_files)
    flow <- consolidate_flow_matrices(flow_files) %>%
      group_by(
        from,
        to
      ) %>%
      summarise(
        lower = quantile(relative_flow, 0.025),
        median = quantile(relative_flow, 0.5),
        upper = quantile(relative_flow, 0.975)
      )


    message("dumping output at ", here::here(
        "data/output",
        paste0(
          "flow_",
          tp,
          "_",
          tw,
          ".csv"
        )
      ))
    readr::write_csv(
      x = flow,
      path = here::here(
        "data/output",
        paste0(
          "flow_",
          tp,
          "_",
          tw,
          ".csv"
        )
      )
    )
  }
}

