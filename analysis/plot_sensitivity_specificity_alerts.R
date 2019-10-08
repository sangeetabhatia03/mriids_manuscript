source(here::here("analysis/common_plot_properties.R"))

message("Reading from and writing to ", all_files[[datasource]]$outdir)

incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "weekly_alerts.csv"
    )
  )


incid_pred <- incid_pred[incid_pred$time_window == twindow &
                           incid_pred$n.dates.sim == n.dates.sim, ]


## incid_pred <- dplyr::filter(incid_pred, !is.na(incid))
##nonna_alerts <- dplyr::filter(incid_pred, incid != 0 | ymin != 0)

## Only retain countries where we didn't always observe and predict 0.
nonna_alerts <- dplyr::filter(incid_pred, alert_type != "No Alert")
nonna_alerts <- dplyr::filter(nonna_alerts, threshold == "50%")

## Make 1 plot for each country.
bycountry <- split(nonna_alerts, nonna_alerts$country)
values <- c(`Missed Alert.FALSE` = "red",
            `Missed Alert.TRUE` = "#ffb2b2",
            `True Alert.FALSE` = "#009E73",
            `True Alert.TRUE` = "#99d8c7",
            `False Alert.FALSE` = "#cc8400",
            `False Alert.TRUE` = "#f4e6cb")

purrr::iwalk(
    bycountry,
    function(df, country) {
        p <- ggplot(df, aes(date_obs, date_pred, col = interaction(alert_type, interpolated))) +
            geom_point()
        p <- p + theme_base()
        p <- p + scale_colour_manual(values = values)
        p <- p + theme(legend.position = "none")
        outfile <- here::here(
            all_files[[datasource]]$outdir,
            glue::glue("{country}_alerts_{twindow}_{n.dates.sim}.pdf")
        )
        ggplot2::ggsave(filename = outfile, plot = p)
       }
)
## Since we want to plot dates on the x-axis, coutnries on y
## And alerts on the plot, we will have to force assign a y level
## to each alert.
## create a data frame with artificial y-levels.
countries <- unique(nonna_alerts$country)
weeks <- unique(nonna_alerts$week_of_projection)

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
    by = 0.05,
    length.out = nrow(forced_y_level)
)



## Join the data frame to the data frame that contains alerts.
forced_y_level$week_of_projection <- factor(forced_y_level$week_of_projection)
nonna_alerts$week_of_projection <- factor(nonna_alerts$week_of_projection)

nonna_alerts <- dplyr::left_join(
  nonna_alerts,
  forced_y_level
)

yaxis_levels <- dplyr::group_by(
  forced_y_level,
  country
) %>%
  summarise(maxy = max(forced_y), miny = min(forced_y))

yaxis_levels$y <- yaxis_levels$miny + ((yaxis_levels$maxy - yaxis_levels$miny)/2)




## For each country, tag the first alert raised.
## so that we can make it slightly bigger.
first_alerts <- dplyr::group_by(nonna_alerts, country) %>%
    dplyr::summarise(date_of_projection = min(date_of_projection))

first_alerts$first_alert <- "YES"

nonna_alerts <- dplyr::left_join(
    nonna_alerts,
    first_alerts
)

nonna_alerts$first_alert[is.na(nonna_alerts$first_alert)] <- "NO"


## For eacg country, highight instances where there were no cases
## in the previous week and cases observed in the present week.

new_weekly_obs <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "2019-10-07_new_weekly_incidence.csv"
  )
)

new_weekly_obs$new_obs <- "YES"

nonna_alerts <- dplyr::left_join(
    nonna_alerts,
    new_weekly_obs,
    by = c("date_obs" = "date", "country")
)

nonna_alerts$new_obs[is.na(nonna_alerts$new_obs)] <- "NO"
nonna_alerts$stroke <- 0
nonna_alerts$stroke[nonna_alerts$new_obs == "YES"] <- 0.5


## Define color scale.


p <- ggplot(
  nonna_alerts,
  aes(
      date_obs,
      forced_y,
      size = first_alert,
      col = interaction(alert_type, interpolated),
      shape = new_obs
  )
) +
    geom_point() +
    scale_shape_manual(values = c(YES = 24, NO = 15)) +
    scale_size_manual(values = c(YES = 1, NO = 0.5)) +
    scale_colour_manual(
        values = values
    ) +
    ## scale_fill_manual(
    ##     values = c(
    ##         `NO.Missed Alert` = "red",
    ##         `NO.True Alert` = "#009E73",
    ##         `NO.False Alert` = "#E69F00",
    ##         `YES.Missed Alert` = "black",
    ##         `YES.True Alert` = "black",
    ##         `YES.False Alert` = "black"
    ##     )
    ## ) +
    scale_y_continuous(
      breaks = yaxis_levels$y,
      labels = yaxis_levels$country
    )

p <- p + mriids_plot_theme$theme +
    mriids_plot_theme$legend +
    theme(
        axis.text.y = element_text(
            angle = 90,
            hjust = 0.5,
            size = 4
        )
    )  +
    scale_x_date(labels = mriids_plot_theme$dateformat) +
    mriids_plot_theme$xticklabels +
    xlab("") + ylab("")

## Week labels on the right
forced_y_level$date <- max(nonna_alerts$date_of_projection) + 2
## p <- p +
##   geom_text(
##     data = forced_y_level,
##     aes(
##       x = date,
##       y = forced_y,
##       label = label
##     ),
##     size = 1,
##     inherit.aes = FALSE
##   ) +
##   coord_cartesian(
##     xlim = range(nonna_alerts$date_of_projection),
##     clip = "off"
##   )

## Faint horizontal lines
lines <- forced_y_level[forced_y_level$week_of_projection == 1, ]
lines <- lines[-1, ]
p2 <- p +
    geom_hline(
        data = lines,
        aes(yintercept = forced_y - 0.05),
        alpha = "0.05"
    )


ggsave(
    filename = here::here(
        all_files[[datasource]]$outdir,
        glue::glue(
            "alerts_per_week_high_{datasource}",
            "_{twindow}_{n.dates.sim}.pdf"
            )
        ),
    plot = p2,
    units = mriids_plot_theme$units,
    width = mriids_plot_theme$single_col_width,
    height = mriids_plot_theme$single_col_height
)


### ROC

infile <- here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_trp_fpr_by_week_projection.csv")
  )
roc <- readr::read_csv(infile)
df <- dplyr::filter(roc, time_window == twindow & n.dates.sim == 28)
df$n.dates.sim <- factor(df$n.dates.sim)
df$week_of_projection <- factor(df$week_of_projection)
## f77be3
## palette <- c(
##     "28" = "#0078fb", ## blue
##     "42" = "#f77be3", ## dark pink
##     "56" = "#ff8b8c" ## light pink
## )

palette <- c(
    "4" = "#99c9fd", ## light blue
    "3" = "#0078fb", ## blue
    "2" = "#f77be3", ## dark pink
    "1" = "#ff8b8c" ## light pink
)

roc_p <- ggplot(df, aes(x = fpr, y = tpr)) +
        geom_line(aes(col = week_of_projection))
roc_p <- roc_p + xlim(0, 1) + ylim(0, 1)
roc_p <- roc_p + xlab("False Alert Rate") + ylab("True Alert Rate")
roc_p <- roc_p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
roc_p <- roc_p + scale_color_manual(values = palette)
roc_p <- roc_p + mriids_plot_theme$theme
roc_p <- roc_p + mriids_plot_theme$legend

plot <- cowplot::plot_grid(
    roc_p,
    p2,
    align = "hv",
    nrow = 1,
    ncol = 2,
    rel_widths = c(1, 1.5),
    labels = "AUTO",
    label_size = 8
    )

filename <- glue::glue("{datasource}_roc_alerts_{twindow}.pdf")
filename <- here::here(all_files[[datasource]]$outdir, filename)

cowplot::save_plot(
    filename = filename,
    plot,
    base_height = mriids_plot_theme$one_n_half_col_height / 2.5,
    base_width = mriids_plot_theme$double_col_width / 2.5
)


## Which threshold gives max sensitivity
roc$mean_tpr_frp <- (roc$tpr + (1 - roc$fpr)) / 2
out <- split(roc, list(roc$time_window, roc$n.dates.sim))
for (o in out) {
    idx <- which.max(o$tpr)
    message(o$time_window[1], " ", o$n.dates.sim[1])
    message(o$threshold[idx], " ", o$tpr[idx], " ", o$fpr[idx])
}


## Which threshold gave max mean(tpr + fpr)
for (o in out) {
    idx <- which.max(o$mean_tpr_frp)
    message(o$time_window[1], " ", o$n.dates.sim[1])
    message(o$threshold[idx], " ", o$tpr[idx], " ", o$fpr[idx], " ", o$mean_tpr_frp[idx])
}
