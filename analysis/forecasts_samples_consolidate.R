infiles <- list.files(
  path = all_files[[datasource]]$outdir,
  pattern = "^forecasts_samples_[0-9]*_[0-9]*_[0-9]*.Rds",
)

purrr::walk(
    infiles,
    function(x) {
        message("Working on ", x)
        df <- readr::read_rds(here::here(all_files[[datasource]]$outdir, x))
        df <- purrr::map(df, ~ .[, all_files[[datasource]]$places])
        df <- dplyr::bind_cols(df)
        readr::write_rds(
            x = df,
            path = here::here(
               all_files[[datasource]]$outdir,
               paste0("consolidated_", x)
            )
        )
    }
)



