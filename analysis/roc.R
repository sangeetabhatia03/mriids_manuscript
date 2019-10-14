weekly_alerts <- data.table::fread(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_weekly_alerts.csv")
   )
 )

weekly_alerts <- weekly_alerts[weekly_alerts$n.dates.sim != 14, ]
## weekly_alerts[!complete.cases(weekly_alerts), ]
## Plot TPR vs FPR
## TPR = # of TRUE Alerts/ (# of True Alerts + # of Missed Alerts)
## FPR = # of FALSE Alerts/ (# of No Alerts + # of False Alerts)

res <- dplyr::group_by(
    weekly_alerts,
    time_window,
    n.dates.sim,
    week_of_projection, ## Added on 0710 as per Anne's suggestion to plot sensitivity etc by week of projection.
    threshold,
    alert_type
) %>%
  dplyr::summarise(n = dplyr::n())

res <- ungroup(res)

res <- tidyr::spread(
    res,
    key = alert_type,
    value = n,
    fill = 0
    )
## res[!complete.cases(res), ]
## total_alerts <- res$`False Alert` +
## res$`Missed Alert` +
## res$`No Alert` +
## res$`True Alert`
## ## counted another way
## total_alerts2 <- dplyr::group_by(
##    weekly_alerts, time_window, n.dates.sim, threshold) %>%
##   summarise(n = n())
## all(total_alerts2$n == total_alerts)

res$tpr <- res$`True Alert` / (res$`True Alert` + res$`Missed Alert`)
res$fpr <- res$`False Alert` / (res$`No Alert` + res$`False Alert`)


outfile <- here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_trp_fpr_by_week_projection.csv")
  )
readr::write_csv(x = res, path = outfile)

##########################
## Added on 09-10-2019
## TPR/FPR this over time.
##########################

res2 <- dplyr::group_by(
    weekly_alerts,
    time_window,
    n.dates.sim,
    date_pred,
    week_of_projection, ## Added on 0710 as per Anne's suggestion to plot sensitivity etc by week of projection.
    threshold,
    alert_type
) %>%
  dplyr::summarise(n = dplyr::n())

res2 <- ungroup(res2)

res2 <- tidyr::spread(
    res2,
    key = alert_type,
    value = n,
    fill = 0
    )
## res[!complete.cases(res), ]
## TPR and FPR are now the rates *so far*, not instantaneous rates.
byparams <- split(res2, list(res2$time_window, res2$n.dates.sim))
##byparams <- purrr::map(byparams, ~ arrange(.x, date_pred))
##alldates_pred <-purrr::map(byparams, ~ unique(.x$date_pred))

perf_sofar <- purrr::map_dfr(
    byparams,
    ## df - data.frame for a fixed time_window and n.dates.sim
    function(df) {
        alldates <- unique(df$date_pred)
        names(alldates) <- alldates
        ## out will have - date_pred, time_window, n.dates.sim,
        ## week_of_projection, threshold, False Alert, Missed Alert,
        ## True Alert, No Alert, tpr, fpr
        out <- purrr::map_dfr(
            alldates,
            function(curr_date) {
                before_curr_date <- dplyr::filter(df, date_pred <= curr_date)
                before_curr_date <- dplyr::group_by(
                    before_curr_date,
                    time_window,
                    n.dates.sim,
                    week_of_projection,
                    threshold
                ) %>% dplyr::summarize_at(
                          c("False Alert",
                            "Missed Alert",
                            "True Alert",
                            "No Alert"),
                          sum
                          )
                before_curr_date$tpr <- before_curr_date$`True Alert` /
                    (before_curr_date$`True Alert` + before_curr_date$`Missed Alert`)
                before_curr_date$fpr <- before_curr_date$`False Alert` /
                    (before_curr_date$`No Alert` + before_curr_date$`False Alert`)

                before_curr_date
            },
            .id = "date_pred"
         )
        out
    }

)


outfile <- here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_trp_fpr_by_date_obs.csv")
  )
readr::write_csv(x = perf_sofar, path = outfile)


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
