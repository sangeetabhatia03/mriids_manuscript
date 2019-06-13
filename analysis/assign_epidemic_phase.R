library(dplyr)
## To analyse the performance of the model by
## phase, we classify each window as either
## declining, growing or neither.



fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

fitfiles <- paste(
  "_rquantiles",
  fitfiles,
  sep = "_"
)

rquantile_files <- expand.grid(
  places,
  fitfiles,
  stringsAsFactors = FALSE
)

rquantile_files <- paste0(
    rquantile_files$Var1,
    rquantile_files$Var2
)

names(rquantile_files) <- stringr::str_replace_all(
  rquantile_files,
  ".rds",
  ""
)

## First extract the R value used to project forward.
rquantiles <- purrr::map_dfr(rquantile_files,
  function(x) {
    x <- here::here(
      all_files[[datasource]]$stanfits_dir,
      x
    )
    out <- readr::read_rds(x)
    out <- slice(out, n())
    out
  },
  .id = "params"
)

## Keep the columns we want.
rquantiles <- tidyr::separate(rquantiles,
  params,
  into = c(
    "country",
    "what",
    "tproj",
    "twindow"
  ),
  sep = "_",
  convert = TRUE
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
  path = here::here(
    all_files[[datasource]]$outdir,
    paste0(
      datasource,
      "_rquantiles_projection.csv"
    )
  )
)


## Retrospective and real-time estimates of R.
## Repeat each row over the projection horizon.

rused <- rquantiles[rep(seq_len(nrow(rquantiles)), each = n.dates.sim), ]

## Fix the dates.
rused <- dplyr::group_by(
    rused,
    tproj,
    twindow,
    country
    ) %>%
    dplyr::mutate(
    date = seq(from = min(date), length.out = n.dates.sim, by = "1 day")
)

readr::write_csv(
  x = rused,
  path = here::here(
    all_files[[datasource]]$outdir,
    paste0(
      datasource,
      "_rquantiles_real_time.csv"
    )
  )
 )


