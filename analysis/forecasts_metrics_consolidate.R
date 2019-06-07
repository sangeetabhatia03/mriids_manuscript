source(here::here("analysis/parameters.R"))

consolidate_metrics <- function(infiles, indir) {
    metrics <- purrr::map_dfr(
        infiles,
        ~ readr::read_csv(here::here(indir, .x)),
        .id = "params"
        )
    metrics <- tidyr::separate(
        metrics,
        col = params,
        into = c("country", "tproj", "twindow", "n.dates.sim"),
        convert = TRUE
        )
    metrics
}

pattern <- "*daily_metrics*csv"
infiles <- list.files(
  path = paste0(all_files[[datasource]]$outdir, "/metrics/daily"),
  pattern = glob2rx(pattern),
)

names(infiles) <- stringr::str_remove_all(
    infiles,
    "daily_metrics_"
    )

indir <- paste0(all_files[[datasource]]$outdir, "/metrics/daily")
daily_metrics <- consolidate_metrics(infiles, indir)


## Weekly Metrics
infiles <- stringr::str_replace_all(infiles,
                                    pattern = "daily",
                                    replacement = "weekly")

names(infiles) <- stringr::str_remove_all(
    infiles,
    "weekly_metrics_"
    )

indir <- stringr::str_replace_all(indir,
                                    pattern = "daily",
                                    replacement = "weekly")

weekly_metrics <- consolidate_metrics(infiles, indir)
weekly_metrics <- dplyr::group_by(weekly_metrics, country, tproj, twindow) %>%
    arrange(date) %>%
    mutate(week_of_projection = seq_len(n()))



readr::write_csv(
  x = daily_metrics,
  path = here::here(
    all_files[[datasource]]$outdir,
    "daily_metrics.csv"
  )
 )


readr::write_csv(
  x = weekly_metrics,
  path = here::here(
    all_files[[datasource]]$outdir,
    "weekly_metrics.csv"
  )
 )







