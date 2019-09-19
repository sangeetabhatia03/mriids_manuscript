weekly_alerts <- data.table::fread(
  file = here::here(
    all_files[[datasource]]$outdir,
    "weekly_alerts.csv"
   )
 )

weekly_alerts <- weekly_alerts[weekly_alerts$n.dates.sim != 14, ]
## Plot TPR vs FPR
## TPR = # of TRUE Alerts/ (# of True Alerts + # of Missed Alerts)
## FPR = # of FALSE Alerts/ (# of No Alerts + # of False Alerts)

res <- dplyr::group_by(
    weekly_alerts,
    time_window,
    n.dates.sim,
    threshold,
    alert_type
) %>%
  dplyr::summarise(n = dplyr::n())

res <- tidyr::spread(
    res,
    key = alert_type,
    value = n)

res$tpr <- res$`True Alert` / (res$`True Alert` + res$`Missed Alert`)
res$fpr <- res$`False Alert` / (res$`No Alert` + res$`False Alert`)


outfile <- here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_trp_fpr.csv")
  )
readr::write_csv(x = res, path = outfile)

## Fix column classes for correct plotting
res$time_window <- factor(res$time_window)
## res$n.dates.sim <- factor(res$n.dates.sim)
library(ggplot2)
library(ggthemes)

plot_roc <- function(df) {
    p <- ggplot(df, aes(x = fpr, y = tpr)) + geom_line()
    p <- p + xlim(0, 1) + ylim(0, 1)
    p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
    p <- p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
    p
}

by_params <- split(res, list(res$time_window, res$n.dates.sim))
## by parameters
plotlist <- purrr::iwalk(by_params, function(df, params) {
    p <- plot_roc(df)
    p <- p + mriids_plot_theme$onecol_theme
    p <- p + mriids_plot_theme$legend
    outfile <- glue::glue("{datasource}_roc_{params}.pdf")
    outfile <- here::here(all_files[[datasource]]$outdir, outfile)
    ggplot2::ggsave(
        filename = outfile,
        plot = p,
        units = mriids_plot_theme$units,
        width = mriids_plot_theme$single_col_width,
        height = mriids_plot_theme$single_col_height
      )
  }
)

by_ndates <- split(res, res$n.dates.sim)

plotlist <- purrr::iwalk(by_ndates, function(df, params) {
    p <- ggplot(df, aes(x = fpr, y = tpr)) + geom_line(aes(col = time_window))
    p <- p + xlim(0, 1) + ylim(0, 1)
    p <- p + xlab("False Positive Rate") + ylab("True Positive Rate")
    p <- p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
    p <- p + mriids_plot_theme$onecol_theme
    p <- p + mriids_plot_theme$legend
    outfile <- paste0(datasource, "_roc_ndates_", params, ".pdf")
    outfile <- here::here(all_files[[datasource]]$outdir, outfile)
    ggsave(
        filename = outfile,
        plot = p,
        units = mriids_plot_theme$units,
        width = mriids_plot_theme$single_col_width,
        height = mriids_plot_theme$single_col_height
       )
  }
 )


by_tw <- split(res, res$time_window)

palette <- c(
    "28" = "#af5d00",
    "42" = "#6234b8",
    "56" = "#8dd6a1"
)

plotlist <- purrr::iwalk(by_tw, function(df, params) {

    df$n.dates.sim <- factor(df$n.dates.sim)
    p <- ggplot(df, aes(x = fpr, y = tpr)) +
        geom_line(aes(col = n.dates.sim))

    p <- p + xlim(0, 1) + ylim(0, 1)
    p <- p + xlab("False Alert Rate") + ylab("True Alert Rate")
    p <- p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
    p <- p +  scale_color_manual(values = palette)

    p <- p + mriids_plot_theme$onecol_theme
    p <- p + mriids_plot_theme$legend
    outfile <- glue::glue("{datasource}_roc_tw_{params}.pdf")
    outfile <- here::here(all_files[[datasource]]$outdir, outfile)
    ggplot2::ggsave(
        filename = outfile,
        plot = p,
        units = mriids_plot_theme$units,
        width = mriids_plot_theme$single_col_width,
        height = mriids_plot_theme$single_col_height
     )
  }
)
