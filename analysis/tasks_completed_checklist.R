library(dplyr)
source(here::here("analysis/parameters.R"))
error_log <- glue::glue("{datasource}_missing_files.txt")

missing <- function(files) {
    exist <- file.exists(files)
    files[!exist]
}

fitfiles <- list.files(
  path = all_files[[datasource]]$stanfits_dir,
  pattern = "^[0-9]*_[0-9]*.rds",
  full.names = FALSE
)

## Expect indices
selected <- glue::glue("idx_{fitfiles}")

out <- missing(
    here::here(all_files[[datasource]]$stanfits_dir, selected)
)

message("Missing index files ")
message(out)
cat(out, sep = "\n", file = error_log, append = TRUE)

## Check that draws have been extracted from the stanfits
pstay_files <- glue::glue("pstay_samples_{fitfiles}")
out <- missing(
    here::here(all_files[[datasource]]$stanfits_dir, pstay_files)
)
message("Missing pstay files ")
message(out)
cat(out, sep = "\n", file = error_log, append = TRUE)

gamma_files <- glue::glue("gamma_samples_{fitfiles}")
out <- missing(
    here::here(all_files[[datasource]]$stanfits_dir, gamma_files)
)
message("Missing gamma files ")
message(out)
cat(out, sep = "\n", file = error_log, append = TRUE)

## Forward simulation
ndates <- c(28, 42, 56)
infiles <- stringr::str_remove_all(fitfiles, ".rds")
infiles <- purrr::cross2(infiles, ndates)
infiles <- unlist(purrr::map(infiles, purrr::lift(paste, sep = "_")))
infiles <- glue::glue("forecasts_samples_{infiles}.Rds")
out <- missing(
    here::here(all_files[[datasource]]$outdir, infiles)
)
message("Missing forecast samples ")
message(out)
cat(out, sep = "\n", file = error_log, append = TRUE)

## Consolodated samples
infiles <- glue::glue("consolidated_{infiles}")
out <- missing(
    here::here(all_files[[datasource]]$outdir, infiles)
)
message("Missing consolidates samples files ")
message(out)
cat(out, sep = "\n", file = error_log, append = TRUE)

## Consolodated weekly samples
infiles <- stringr::str_replace_all(infiles, "consolidated", "weekly")
out <- missing(
    here::here(all_files[[datasource]]$outdir, infiles)
)
message("Missing weekly samples files ")
message(out)
cat(out, sep = "\n", file = error_log, append = TRUE)

infiles <- stringr::str_replace_all(infiles, "forecasts_samples", "metrics")
infiles <- stringr::str_replace_all(infiles, ".Rds", ".csv")

gin_infiles <- glue::glue("metrics/weekly/GIN_{infiles}")
out <- missing(
    here::here(all_files[[datasource]]$outdir, gin_infiles)
)

message("Missing weekly metrics files ")
message(out)

cat(out, sep = "\n", file = error_log, append = TRUE)

lbr_infiles <- glue::glue("metrics/weekly/LBR_{infiles}")
out <- missing(
    here::here(all_files[[datasource]]$outdir, lbr_infiles)
)

message("Missing weekly metrics files ")
message(out)

cat(out, sep = "\n", file = error_log, append = TRUE)

sle_infiles <- glue::glue("metrics/weekly/SLE_{infiles}")
out <- missing(
    here::here(all_files[[datasource]]$outdir, sle_infiles)
)

message("Missing weekly metrics files ")
message(out)

cat(out, sep = "\n", file = error_log, append = TRUE)
