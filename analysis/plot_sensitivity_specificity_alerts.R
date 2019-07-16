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
incid_pred <- filter(incid_pred, !is.na(incid))
nonna_alerts <- filter(incid_pred, incid != 0 | ymin != 0)



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

df_to_plot <- select(
  nonna_alerts,
  date_of_projection,
  country,
  week_of_projection,
  forced_y,
  alert_type_high
)


yaxis_levels <- group_by(
  forced_y_level,
  country
) %>%
  summarise(maxy = max(forced_y), miny = min(forced_y))

yaxis_levels$y <- yaxis_levels$miny + ((yaxis_levels$maxy - yaxis_levels$miny)/2)


p <- ggplot(
  df_to_plot,
  aes(date_of_projection, forced_y, col = alert_type_high)
) +
  geom_point(size = 1.1, shape = 15) +
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

p <- p + mriids_plot_theme$theme +
    mriids_plot_theme$legend +
    theme(
        axis.text.y = element_text(
            angle = 90,
            hjust = 0.5,
            size = 10
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
    size = 2,
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
        paste0(
            "alerts_per_week_high_",
            datasource,
            "_",
            twindow,
            "_",
            n.dates.sim,
            ".pdf"
        )
        ),
    plot = p2
)
