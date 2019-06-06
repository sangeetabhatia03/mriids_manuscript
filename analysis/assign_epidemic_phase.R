library(dplyr)
## To analyse the performance of the model by
## phase, we classify each window as either
## declining, growing or neither.

source(here::here("analysis/parameters.R"))

fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

rquantile_files <- paste(
  places,
  "rquantiles",
  fitfiles,
  sep = "_"
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
  path = here::here(
    "data/processed",
    paste0(
      datasource,
      "_rquantiles_projection.csv"
    )
  )
)
