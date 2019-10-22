library(patchwork)
weekly_incid <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_processed_weekly_incidence.csv")
  )
)
weekly_incid <- weekly_incid[weekly_incid$n.dates.sim != 14, ]
incid_byparams <- split(
    weekly_incid,
    list(weekly_incid$time_window, weekly_incid$n.dates.sim),
    sep = "_"
)

## weekly_incid <- dplyr::filter(weekly_incid, country %in% c("GIN", "SLE", "LBR"))

infile <- here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_trp_fpr_by_date_obs.csv")
  )

roc <- readr::read_csv(infile)

roc$time_window <- as.integer(roc$time_window)
roc$n.dates.sim <- as.integer(roc$n.dates.sim)
roc$date_pred <- lubridate::ymd(roc$date_pred)
roc$week_of_projection <- factor(roc$week_of_projection)

threshold <- "50%"
roc <- roc[roc$threshold == threshold, ]
roc <- roc[roc$n.dates.sim != 14, ]

byparams <- split(roc, list(roc$time_window, roc$n.dates.sim), sep = "_")
params <- names(byparams)
plots <- purrr::map(
    params,
    function(param) {
        wincid <- incid_byparams[[param]]
        wincid <- wincid[wincid$country %in% c("GIN", "LBR", "SLE"), ]
        df <- byparams[[param]]

        incidplot <- ggplot(
            wincid,
            aes(date, incid)) + geom_col(aes(fill = country))

        incidplot <- incidplot + mriids_plot_theme$theme
        incidplot <- incidplot + mriids_plot_theme$legend
        incidplot <- incidplot + xlab("") + ylab("Weekly Incidence")

        overall <- dplyr::group_by(df, date_pred, threshold) %>%
            dplyr::summarise_at(c("tpr", "fpr"), mean)

        p <- ggplot(df, aes(date_pred, tpr, col = week_of_projection)) +
            geom_line()
        p <- p + geom_line(data = df,
                           aes(date_pred, fpr, col = week_of_projection),
                           linetype = "dashed")
        p <- p + geom_line(data = overall, aes(date_pred, tpr), col = "black")
        p <- p + geom_line(data = overall,
                           aes(date_pred, fpr),
                           col = "black",
                           linetype = "dashed")
        p <- p + ylim(0, 1)
        p <- p + ylab("True/False Alert Rate") + xlab("")
        p <- p + geom_hline(yintercept = 0.5, alpha = 0.1)
        p <- p + scale_color_manual(values = mriids_plot_theme$week_color_scale)
        p <- p + mriids_plot_theme$theme
        p <- p + mriids_plot_theme$legend
        p <- p + incidplot + plot_layout(nrow = 2)
        outfile <- here::here(
            all_files[[datasource]]$outdir,
            glue::glue("{Sys.Date()}_{datasource}_tpr_fpr_by_date_obs_{param}.pdf")
            )
        ggplot2::ggsave(
            filename = outfile,
            plot = p,
            width = mriids_plot_theme$double_col_width,
            height = mriids_plot_theme$double_col_width / 2,
            units = mriids_plot_theme$units
        )
        p

    }
)


