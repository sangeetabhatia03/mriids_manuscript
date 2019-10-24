## y level for each country and week
## x-axis is date, y-axis is country. So we need to artifically
## assign a y-level to each country so that successive weeks
## can be plotted at different heights for each country.
yaxis_levels <- function(countries, weeks, by = 0.05) {

    country_week <- purrr::cross2(
        as.character(countries),
        as.character(weeks)
    )

    forced_y_level <- data.frame(
        country = purrr::map_chr(country_week, ~ .x[[1]]),
        week_of_projection = purrr::map_chr(country_week, ~ .x[[2]]),
        forced_y = 0,
        stringsAsFactors = FALSE
    )
    forced_y_level$label <- paste(
        "Week",
        forced_y_level$week_of_projection
    )
    ## Make sure the weeks are in order before assigning y level.

    forced_y_level <- dplyr::arrange(
        forced_y_level,
        country,
        week_of_projection
    )

    forced_y_level$forced_y <- seq(
        from = 0,
        by = by,
        length.out = nrow(forced_y_level)
    )

    forced_y_level
}
## All alerts for all weeks for all countries
all_alerts <- function(alerts, by = 0.05) {
    countries <- unique(alerts$country)
    weeks <- unique(alerts$week_of_projection)
    forced_y_level <- yaxis_levels(countries, weeks, by)
    forced_y_level$week_of_projection <- factor(
        forced_y_level$week_of_projection
    )
    alerts$week_of_projection <- factor(alerts$week_of_projection)
    alerts <- dplyr::left_join(
        alerts,
        forced_y_level
        )
    ## Place the label in the middle of the 4 rows.
    label_levels <- dplyr::group_by(
        forced_y_level,
        country
    ) %>%
        summarise(maxy = max(forced_y), miny = min(forced_y))

    label_levels$y <- label_levels$miny +
        ((label_levels$maxy - label_levels$miny)/2)

    p <- ggplot(
        alerts,
        aes(date_obs, forced_y,
            size = first_alert,
            col = interaction(alert_type, interpolated),
            shape = new_obs
            )
    ) + geom_point() +
    scale_shape_manual(values = c(YES = 17, NO = 15)) +
    scale_size_manual(values = c(YES = 1, NO = 0.5)) +
    scale_colour_manual(values = values) +
    scale_y_continuous(
      breaks = label_levels$y,
      labels = label_levels$country
    )

    p <- p + mriids_plot_theme$theme +
        mriids_plot_theme$legend +
        theme(
            axis.text.y = element_text(
                angle = 0,
                hjust = 0.5,
                size = 4
            )
        )  +
        scale_x_date(labels = mriids_plot_theme$dateformat) +
        mriids_plot_theme$xticklabels +
        xlab("") + ylab("")

    lines <- forced_y_level[forced_y_level$week_of_projection == 1, ]
    lines <- lines[-1, ]
    p2 <- p +
        geom_hline(
            data = lines,
            aes(yintercept = forced_y - by),
            alpha = "0.05"
        )
    p2

}

alerts_by_week <- function(alerts, by = 0.05) {

    countries <- unique(alerts$country)
    weeks <- "1"
    forced_y_level <- yaxis_levels(countries, weeks, by)
    forced_y_level <- dplyr::select(
        forced_y_level,
        country,
        forced_y
    )

    alerts <- dplyr::left_join(
        alerts,
        forced_y_level,
        by = "country"
    )

    pfacet <- ggplot(
        alerts,
        aes(date_obs, forced_y,
            size = first_alert,
            col = interaction(alert_type, interpolated),
            shape = new_obs
        )
    ) +
        geom_point() +
        scale_shape_manual(values = c(YES = 17, NO = 15)) +
        scale_size_manual(values = c(YES = 0.75, NO = 0.5)) +
        scale_colour_manual(
            values = values
        ) +
        scale_y_continuous(
            breaks = forced_y_level$forced_y,
            labels = forced_y_level$country
        )

    pfacet <- pfacet + mriids_plot_theme$theme +
        mriids_plot_theme$legend +
        scale_x_date(labels = mriids_plot_theme$dateformat) +
        mriids_plot_theme$xticklabels +
        xlab("") + ylab("")
    pfacet <- pfacet + facet_wrap(~week_of_projection)

    ## Faint horizontal lines
    lines <- forced_y_level
    lines$y <- lines$forced_y + 0.01
    pfacet2 <- pfacet +
        geom_hline(
            data = lines,
            aes(yintercept = y),
            alpha = "0.05",
            size = 0.3 ## default is 0.5. Looks quite thick in this plot.
        )
    pfacet2

}

source(here::here("analysis/common_plot_properties.R"))

message("Reading from and writing to ", all_files[[datasource]]$outdir)

incid_pred <- vroom::vroom(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_weekly_alerts.csv")
    )
  )
incid_pred <- incid_pred[incid_pred$n.dates.sim != 14, ]
nonna_alerts <- dplyr::filter(incid_pred, alert_type != "No Alert")
nonna_alerts <- dplyr::filter(nonna_alerts, threshold == "50%")

## First Alert
first_alerts <- dplyr::group_by(
    nonna_alerts, country, time_window, n.dates.sim) %>%
    dplyr::summarise(date_of_projection = min(date_of_projection))

first_alerts <- dplyr::ungroup(first_alerts)
first_alerts$first_alert <- "YES"

nonna_alerts <- dplyr::left_join(
    nonna_alerts,
    first_alerts
)
nonna_alerts$first_alert[is.na(nonna_alerts$first_alert)] <- "NO"

## New weekly observations
new_weekly_obs <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_new_weekly_incidence.csv")
  )
)
new_weekly_obs <- new_weekly_obs[new_weekly_obs$n.dates.sim != 14, ]
new_weekly_obs$new_obs <- "YES"

nonna_alerts <- dplyr::left_join(
    nonna_alerts,
    new_weekly_obs,
    by = c("date_obs" = "date", "country", "time_window", "n.dates.sim")
)

nonna_alerts$new_obs[is.na(nonna_alerts$new_obs)] <- "NO"
nonna_alerts$stroke <- 0
nonna_alerts$stroke[nonna_alerts$new_obs == "YES"] <- 0.5



alerts_byparams <- split(
    nonna_alerts,
    list(nonna_alerts$time_window, nonna_alerts$n.dates.sim),
    sep = "_"
)
## Better to extract by name so that we get ROC and alerts for the
## same combination fo parameters.
params <- names(alerts_byparams)

values <- c(`Missed Alert.FALSE` = "#ff0000",
            `Missed Alert.TRUE` = "#ff7f7f",
            `True Alert.FALSE` = "#009E73",
            `True Alert.TRUE` = "#7fceb9",
            `False Alert.FALSE` = "#cc8400",
            `False Alert.TRUE` = "#e5c17f"
            )


purrr::walk(
    params,
    function(param) {
        alerts <- alerts_byparams[[param]]
        ## Plot 1. All countries, all weeks
        p <- all_alerts(alerts)
        ggplot2::ggsave(
            filename = here::here(
                all_files[[datasource]]$outdir,
                glue::glue("{Sys.Date()}_alerts_per_week_{datasource}",
                           "_{param}.pdf")),
            plot = p,
            units = mriids_plot_theme$units,
            width = mriids_plot_theme$single_col_width,
            height = mriids_plot_theme$single_col_height
        )

    }
)


purrr::walk(
    params,
    function(param) {
        alerts <- alerts_byparams[[param]]
        ## Plot 1. All countries, all weeks
        p <- alerts_by_week(alerts)
        p <- p +  theme(
            axis.text.y = element_text(
                angle = 0,
                hjust = 0.5,
                size = 2
            ),
            axis.ticks.y = element_line(size = 0.3),
            axis.text.x = element_text(
                size = 2
            )
        )

        ggplot2::ggsave(
            filename = here::here(
                all_files[[datasource]]$outdir,
                glue::glue("{Sys.Date()}_alerts_by_week_{datasource}",
                           "_{param}.pdf")),
            plot = p,
            units = mriids_plot_theme$units,
            width = mriids_plot_theme$single_col_width,
            height = mriids_plot_theme$single_col_height
        )

    }
  )


purrr::walk(
    params,
    function(param) {
        alerts <- alerts_byparams[[param]]
        byweek <- split(alerts, alerts$week_of_projection)
        purrr::iwalk(
            byweek,
            function(df, week) {
                p <- alerts_by_week(df)
                ggplot2::ggsave(
                  filename = here::here(
                     all_files[[datasource]]$outdir,
                     glue::glue("{Sys.Date()}_alerts_in_{week}_{datasource}",
                                "_{param}.pdf")),
                  plot = p,
                  units = mriids_plot_theme$units,
                  width = mriids_plot_theme$single_col_width,
                  height = mriids_plot_theme$single_col_height
                  )

            }
        )
    }
)


infile <- here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date()}_{datasource}_trp_fpr_by_week_projection.csv")
  )
roc_all <- readr::read_csv(infile)
roc_all <- roc_all[roc_all$n.dates.sim != 14, ]
roc_byparams <- split(
    roc_all,
    list(roc_all$time_window, roc_all$n.dates.sim),
    sep = "_"
)

purrr::walk(
    names(roc_byparams),
    function(param) {
        p <-  roc(roc_byparams[[param]])

        ggplot2::ggsave(
            filename = here::here(
                all_files[[datasource]]$outdir,
                glue::glue("{Sys.Date()}_roc_{datasource}",
                           "_{param}.pdf")),
            plot = p,
            units = mriids_plot_theme$units,
            width = mriids_plot_theme$single_col_width,
            height = mriids_plot_theme$single_col_height
        )

    }
)


## ROC and alerts by week.

purrr::walk(
    params,
    function(param) {
        message("Working on ", param)
        alerts <- alerts_byparams[[param]]
        rates <- roc_byparams[[param]]
        byweek <- split(alerts, alerts$week_of_projection)

       ## ROC curve for all weeks plotted along with
       ## alerts for the first week.

        p1 <- alerts_by_week(byweek[["1"]])
        p1 <- p1 + theme(
                  strip.background = element_blank(),
                  strip.text.x = element_blank()
                  )
        p1 <- p1 +  theme(
            axis.text.y = element_text(
                angle = 0,
                hjust = 0.5,
                size = 4
            ),
            axis.ticks.y = element_line(size = 0.3),
            axis.text.x = element_text(
                size = 4
            )
        )

        p2 <- roc(rates)
        plot <- cowplot::plot_grid(
            p2,
            p1,
            align = "hv",
            axis = "rlbt",
            nrow = 1,
            ncol = 2,
            ##rel_widths = c(1, 1.5),
            labels = "AUTO",
            label_size = 8
        )
        ggplot2::ggsave(
            filename = here::here(
                all_files[[datasource]]$outdir,
                glue::glue("{Sys.Date()}_alerts_in_1_all_roc",
                           "_{datasource}_{param}.pdf")),
            plot = plot,
            units = mriids_plot_theme$units,
            width = mriids_plot_theme$double_col_width,
            height = mriids_plot_theme$double_col_width / 2,
            )

        readr::write_rds(
            x = plot,
            path = here::here(
                all_files[[datasource]]$outdir,
                glue::glue("{Sys.Date()}_alerts_in_1_all_roc",
                           "_{datasource}_{param}.rds"))
        )
        roc_byweek <- split(rates, rates$week_of_projection)
        purrr::walk(
            names(byweek),
            function(week) {
                p1 <- alerts_by_week(byweek[[week]])
                p1 <- p1 + theme(
                               strip.background = element_blank(),
                               strip.text.x = element_blank()
                           )
                p2 <- roc(roc_byweek[[week]], plot_overall = FALSE)
                plot <- cowplot::plot_grid(
                    p2,
                    p1,
                    align = "hv",
                    axis = "rlbt",
                    nrow = 1,
                    ncol = 2,
                    ##rel_widths = c(1, 1.5),
                    labels = "AUTO",
                    label_size = 8
                )
                ggplot2::ggsave(
                  filename = here::here(
                     all_files[[datasource]]$outdir,
                     glue::glue("{Sys.Date()}_alerts_roc_in_{week}",
                                "_{datasource}_{param}.pdf")),
                  plot = plot,
                  units = mriids_plot_theme$units,
                  width = mriids_plot_theme$double_col_width,
                  height = mriids_plot_theme$double_col_width / 2,
                  )

            }
        )
    }
)
