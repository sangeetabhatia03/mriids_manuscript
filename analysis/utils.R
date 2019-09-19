## Return week of year as {year} Week {week}
week_of_year <- function(date) {
    paste(
        lubridate::year(date),
        "Week",
        lubridate::week(date)
    )

}


sensitivity <- function(obs, pred) {
  sum(pred > 0 & obs > 0, na.rm = TRUE) / sum(obs > 0, na.rm = TRUE)
}

specificity <- function(obs, pred) {
  sum(pred == 0 & obs == 0, na.rm = TRUE) / sum(obs == 0, na.rm = TRUE)
}

false_negative <- function(obs, pred) {
  sum(pred == 0 & obs > 0, na.rm = TRUE) / sum(obs > 0, na.rm = TRUE)
}

metrics <- function(obs, pred) {
  data.frame(
    true_positive = sensitivity(obs = obs, pred = pred),
    true_negative = specificity(obs = obs, pred = pred),
    missed_alerts = false_negative(obs = obs, pred = pred)
  )
}

alert_type <- function(obs, pred) {
 out <- dplyr::case_when(
     obs == 0 & pred > 0 ~ "False Alert",
     obs > 0 & pred > 0 ~ "True Alert",
     obs > 0 & pred == 0 ~ "Missed Alert",
     obs == 0 & pred == 0 ~ "No Alert"
 )
 out
}


## Read in daily forecast samples and convert to weekly,
## for all places.
weekly_forecasts <- function(infile) {
    df <- readr::read_rds(infile)
    nweeks <- nrow(df) / 7
    group <- rep(1:nweeks, each = 7)
    out <- rowsum(df, group = group)
    out

}

## Determine quantiles; rows are dates;
## columns are simulations.
## output - rows are dates; columns are quantiles.
weekly_quantiles <- function(weekly,
                             probs = seq(0, 1, 0.025)) {
    out <- apply(weekly, 1, quantile, probs)
    out <- t(out)
    out
}

normalise <- function(vec) {
    vec / sum(vec)
}

country_name <- function(x) {
    countrycode::countrycode(
        x, origin = "iso3c", destination = "country.name"
    )
}

make_pretty <- function(file) {

    namer::unname_all_chunks(file)
    namer::name_chunks(file)
    styler::style_file(file)

}

extract_r_samples <- function(rindex, rsamples, dates) {
    rindex_df <- as.data.frame(rindex)
    rindex_df <- cbind(trng_date, rindex_df)
    colnames(rindex_df) <- c("date", colnames(incid))
    rindex_df <- tidyr::gather(rindex_df, country, var, -date)
    samples <- purrr::pmap(rindex_df, function(date, country, var) r_samples[ , var])
    samples <- do.call(rbind, samples)
    rsamples <- cbind(rindex_df, samples)
    rsamples
}
