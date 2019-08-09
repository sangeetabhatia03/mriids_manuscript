source(here::here("analysis/common_plot_properties.R"))

message("Reading from and writing to ", all_files[[datasource]]$outdir)

incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "alerts_per_week.csv"
    )
  )


incid_pred <- incid_pred[incid_pred$time_window == twindow &
                           incid_pred$n.dates.sim == n.dates.sim, ]

## Only retain countries where we didn't always observe and predict 0.
incid_pred <- dplyr::filter(incid_pred, !is.na(incid))
nonna_alerts <- dplyr::filter(incid_pred, incid != 0 | ymin != 0)



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
    by = 0.09,
    length.out = nrow(forced_y_level)
)



## Join the data frame to the data frame that contains alerts.
forced_y_level$week_of_projection <- factor(forced_y_level$week_of_projection)
nonna_alerts$week_of_projection <- factor(nonna_alerts$week_of_projection)

nonna_alerts <- dplyr::left_join(
  nonna_alerts,
  forced_y_level
)



## This is not really important but retaining only columns we need
## for clarity.

df_to_plot <- dplyr::select(
  nonna_alerts,
  date_of_projection,
  country,
  week_of_projection,
  forced_y,
  alert_type_high
)


yaxis_levels <- dplyr::group_by(
  forced_y_level,
  country
) %>%
  summarise(maxy = max(forced_y), miny = min(forced_y))

yaxis_levels$y <- yaxis_levels$miny + ((yaxis_levels$maxy - yaxis_levels$miny)/2)


p <- ggplot(
  df_to_plot,
  aes(date_of_projection, forced_y, col = alert_type_high)
) +
  geom_point(size = 0.5, shape = 15) +
    scale_colour_manual(
        values = c(
            `Missed Alert` = "red",
            `True Alert` = "#009E73",
            `False Alert` = "#E69F00"
        )
    ) +
    scale_y_continuous(
      breaks = yaxis_levels$y,
      labels = yaxis_levels$country
    )

p <- p + mriids_plot_theme$onecol_theme +
    mriids_plot_theme$legend +
    theme(
        axis.text.y = element_text(
            angle = 90,
            hjust = 0.5,
            size = 4
        ),
        axis.text.x = element_text(
            size = 4,
            angle = 0
        )
    ) +
  scale_x_date(date_breaks = "3 months") +
  xlab("") + ylab("")

## Week labels on the right
forced_y_level$date <- max(df_to_plot$date_of_projection) + 21
p <- p +
  geom_text(
    data = forced_y_level,
    aes(
      x = date,
      y = forced_y,
      label = label
    ),
    size = 1,
    inherit.aes = FALSE
  ) +
  coord_cartesian(
    xlim = range(df_to_plot$date_of_projection),
    clip = "off"
  )

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
    glue::glue("{datasource}_trp_fpr.csv")
  )
roc <- readr::read_csv(infile)
df <- dplyr::filter(roc, time_window == twindow)
df$n.dates.sim <- factor(df$n.dates.sim)

palette <- c(
    "28" = "#0078fb",
    "42" = "#e2b200",
    "56" = "#ff8b8c"
)

roc_p <- ggplot(df, aes(x = fpr, y = tpr)) +
        geom_line(aes(col = n.dates.sim))

roc_p <- roc_p + xlim(0, 1) + ylim(0, 1)
roc_p <- roc_p + xlab("False Positive Rate") + ylab("True Positive Rate")
roc_p <- roc_p + geom_abline(slope = 1, intercept = 0, alpha = 0.3)
roc_p <- roc_p + scale_color_manual(values = palette)
roc_p <- roc_p + mriids_plot_theme$onecol_theme
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
    base_height = 3.5,
    base_width = 7
)


## Which threshold gives max sensitivity
out <- split(roc, list(roc$time_window, roc$n.dates.sim))
for (o in out) {
    idx <- which.max(o$tpr)
    message(o$time_window[1], " ", o$n.dates.sim[1])
    message(o$threshold[idx], " ", o$tpr[idx], " ", o$fpr[idx])
}
