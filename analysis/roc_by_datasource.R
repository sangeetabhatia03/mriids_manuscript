datasources <- list("HealthMap", "ProMED", "WHO")
names(datasources) <- datasources

infiles <- purrr::map(
    datasources,
    ~ here::here(all_files[[.x]]$outdir,
         glue::glue("{Sys.Date()}_{.x}_trp_fpr_by_week_projection.csv")
  )
)

roc <- purrr::map_dfr(infiles, readr::read_csv, .id = "datasource")
roc <- roc[roc$n.dates.sim != 14, ]

overall <- dplyr::group_by(
    roc,
    datasource,
    time_window,
    n.dates.sim,
    threshold
) %>% dplyr::summarise_at(c("tpr", "fpr"), mean)


overall$n.dates.sim <- as.integer(overall$n.dates.sim)
overall$time_window <- as.integer(overall$time_window)

byparams <- split(
    overall, list(overall$time_window, overall$n.dates.sim), sep = "_"
)

purrr::iwalk(
    byparams,
    function(df, params) {
        used <- df[df$threshold == "50%", ]
        overall_p <- ggplot(df, aes(x = fpr, y = tpr)) +
            geom_line(aes(col = datasource)) +
        geom_point(data = used,
                   aes(x = fpr, y = tpr, col = datasource),
                   shape = 19)
        overall_p <- overall_p + xlim(0, 1) + ylim(0, 1)
        overall_p <- overall_p + xlab("False Alert Rate") + ylab("True Alert Rate")
        overall_p <- overall_p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
        overall_p <- overall_p + scale_color_manual(values = mriids_plot_theme$color_scale)
        overall_p <- overall_p + mriids_plot_theme$theme
        overall_p <- overall_p + mriids_plot_theme$legend

        outfile <- glue::glue("{Sys.Date()}_roc_by_ds_{params}.pdf")
        outfile <- here::here("ms-figures/si-figures/other", outfile)
        message("Saving to ", outfile)
        ggplot2::ggsave(
            filename = outfile,
            plot = overall_p,
            width = mriids_plot_theme$single_col_width,
            height = mriids_plot_theme$single_col_height,
            units = mriids_plot_theme$width
            )

    }
)


overall <- dplyr::group_by(
    roc,
    datasource,
    time_window,
    n.dates.sim,
    week_of_projection,
    threshold
) %>% dplyr::summarise_at(c("tpr", "fpr"), mean)


overall$n.dates.sim <- as.integer(overall$n.dates.sim)
overall$time_window <- as.integer(overall$time_window)

byparams <- split(
    overall,
    list(overall$time_window, overall$n.dates.sim), sep = "_"
)

purrr::iwalk(
    byparams,
    function(df, params) {
        used <- df[df$threshold == "50%", ]
        overall_p <- ggplot(df, aes(x = fpr, y = tpr)) +
            geom_line(aes(col = datasource)) +
        geom_point(data = used,
                   aes(x = fpr, y = tpr, col = datasource),
                   shape = 19)
        overall_p <- overall_p + xlim(0, 1) + ylim(0, 1)
        overall_p <- overall_p + xlab("False Alert Rate") + ylab("True Alert Rate")
        overall_p <- overall_p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
        overall_p <- overall_p + scale_color_manual(values = mriids_plot_theme$color_scale)
        overall_p <- overall_p + mriids_plot_theme$theme
        overall_p <- overall_p + mriids_plot_theme$legend
        ## By data source and week of projection.
        overall_p <- overall_p + facet_wrap(~week_of_projection)
        outfile <- glue::glue("{Sys.Date()}_roc_by_ds_week_{params}.pdf")
        outfile <- here::here("ms-figures/si-figures/other", outfile)
        ggplot2::ggsave(
            filename = outfile,
            plot = overall_p,
            width = mriids_plot_theme$single_col_width,
            height = mriids_plot_theme$single_col_height,
            units = mriids_plot_theme$width
            )

    }
)


