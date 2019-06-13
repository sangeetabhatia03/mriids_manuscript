
pattern <- paste0(
    "^forecasts_samples_[0-9]*_",
    twindow,
    "_",
    n.dates.sim,
    ".Rds"
)

infiles <- list.files(
    path = all_files[[datasource]]$outdir,
    pattern = pattern
 )


forecasts <- purrr::map(
    infiles,
    ~ readr::read_rds(here::here(all_files[[datasource]]$outdir, .x))
    )

## Each element of forecasts is a list of 1000 data.frames where each
## data.frame is a single simulation.

sum_all_regions <- purrr::map(
    forecasts,
    function(forecast) {
        out <- purrr::map(
         forecast,
         function(sim) {
             df <- data.frame(date = sim$date)
             df$total <- rowSums(sim[ , -56])
             df
         }
         )
        out
    }
  )

## sum_all_region has the same structure as forecasts.
## i.e., list of lists. sum_all_regions[[i]] has nsims elements.
risk_distribution <- purrr::map2(
    forecasts,
    sum_all_regions,
    function(forecast, overall) {
        out <- purrr::map2(
          forecast,
          overall,
          function(x, y) {
              risk <- as.matrix(x[ , -56]) / y$total
              out2 <- data.frame(
                  date = x$date,
                  risk
              )
              out2
          }
        )
       out
    }
    )

## risk_distribution has the same format i.e., list of lists.
## re-organise so that we have a list of data-frames where
## each data frame captures the risk distribution for one set of
## parameters.

risk_distribution <- purrr::map(
    risk_distribution,
    bind_rows
)

## Dumping it for now.
names(risk_distribution) <- stringr::str_replace_all(infiles, "forecasts", "risk")
names(risk_distribution) <- stringr::str_replace_all(names(risk_distribution), "Rds", "fst")

outdir <- "~/GitWorkArea/mriids_manuscript/data/output/"

purrr::iwalk(
   risk_distribution,
   function(df, path) fst::write.fst(df, paste0(outdir, path))
 )


## How shall we present these results?
infiles <- stringr::str_replace_all(infiles, "forecasts", "risk")
infiles <- stringr::str_replace_all(infiles, "Rds", "fst")

x <- fst::read.fst(
   here::here(all_files[["ProMED"]]$outdir, infiles[1])
   )

median_risk <-
    group_by(x, data) %>%
    summarise_all(quantile, 0.5)

median_risk <- tidyr::gather(
   median_risk,
   country,
   risk,
   -data
 )

group_by(median_risk, data) %>%
    summarise(max(risk))
