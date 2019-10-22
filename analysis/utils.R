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

    if (length(obs) != length(pred)) {
        stop("Length of obs vector different from pred vector")
    }
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


daily_to_weekly <- function (daily) {

    extra <- nrow(daily)%%7
    if (extra != 0) {
        warning("Number of rows is not a multiple of 7.")
        warning(paste("Ignoring last", extra, "days."))
        daily <- utils::head(daily, -extra)
    }
    weeks <- cut(daily$date, breaks = "7 days")
    weekly <- split(daily, weeks)
    weekly <- plyr::ldply(weekly, function(d) colSums(d[, !names(d) %in%
        "date"]))
    weekly <- dplyr::rename(weekly, date = .id)
    weekly
}

#####################################################################
#####################################################################
###################### FIGURES ######################################
#####################################################################
#####################################################################
tpr_by_threshold <- function(df) {

    df$week_of_projection <- factor(df$week_of_projection)
    p <- ggplot(df) +
        geom_point(aes(threshold, tpr, col = week_of_projection)) +
    geom_point(aes(threshold, fpr, col = week_of_projection),
               shape = 17)
    p <- p + scale_color_manual(
                 values = mriids_plot_theme$week_color_scale
             )
    p <- p + mriids_plot_theme$theme +
        mriids_plot_theme$legend

    p <- p + ylab("True/False Alert Rate") + ylim(0, 1)

    p <- p + geom_hline(yintercept = 0.5, alpha = 0.1) +
        geom_vline(xintercept = 50, alpha = 0.1)

    p

}


## ROC curve and alerts for the week.
roc <- function(df, threshold = "50%", plot_overall = TRUE) {

    df$week_of_projection = factor(df$week_of_projection)
    used <- df[df$threshold %in% threshold, ]

    roc_p <- ggplot(df) +
        geom_line(aes(x = fpr, y = tpr, col = week_of_projection)) +
        geom_point(data = used,
                   aes(x = fpr, y = tpr, col = week_of_projection),
                   shape = 19)
    if (plot_overall) {
        overall <- dplyr::group_by(
        df,
        threshold
        ) %>% dplyr::summarise(tpr = mean(tpr), fpr = mean(fpr))
        used_overall <- overall[overall$threshold %in% threshold, ]
        roc_p <- roc_p +
            geom_line(data = overall, aes(x = fpr, y = tpr), col = "black") + ## overall
            geom_point(data = used_overall, aes(x = fpr, y = tpr),
                       col = "black",
                       shape = 19)

    }
    roc_p <- roc_p + xlim(0, 1) + ylim(0, 1)
    roc_p <- roc_p + xlab("False Alert Rate") + ylab("True Alert Rate")
    roc_p <- roc_p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
    roc_p <- roc_p +
        scale_color_manual(values = mriids_plot_theme$week_color_scale)
    roc_p <- roc_p + mriids_plot_theme$theme
    roc_p <- roc_p + mriids_plot_theme$legend

    roc_p

}
