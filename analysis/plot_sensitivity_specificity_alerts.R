incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    paste0(
      "alerts_per_week_",
      twindow,
      "_",
      n.dates.sim,
      ".csv"
    )
  )
)


## Only retain countries where we didn't always observe and predict 0.
incid_pred <- filter(incid_pred, !is.na(incid))
nonna_alerts <- filter(incid_pred, incid != 0 | ymin != 0)

## Assign a y level.
nonna_alerts$country_week <- paste(
  nonna_alerts$country,
  "Week",
  nonna_alerts$week_of_projection
)
nonna_alerts$country_week <- factor(nonna_alerts$country_week)

nonna_alerts$country <- factor(nonna_alerts$country)
nonna_alerts$week_of_projection <- factor(nonna_alerts$week_of_projection)

yinit <- 0
forced_y_level <- data.frame(
  country_week = levels(nonna_alerts$country_week),
  forced_y = yinit
)
y <- 1

for (cntry in levels(nonna_alerts$country)) {
  for (week in levels(nonna_alerts$week_of_projection)) {
    cw <- paste(cntry, "Week", week)
    idx <- which(forced_y_level$country_week == cw)
    if (length(idx) == 0) next
    if (forced_y_level$forced_y[idx] == yinit) {
      forced_y_level$forced_y[idx] <- y
    }
    y <- y + 0.3
  }
  y <- ceiling(y)
}

nonna_alerts <- dplyr::left_join(
  nonna_alerts,
  forced_y_level,
  by = "country_week"
)

## nonna_alerts$forced_y <- factor(nonna_alerts$forced_y)
source(here::here("analysis/common_plot_properties.R"))

####

df_to_plot <- select(
  nonna_alerts,
  date_of_projection,
  country,
  week_of_projection,
  forced_y,
  alert_type_high
)


yaxis_levels <- group_by(
  df_to_plot,
  country
) %>%
  summarise(y = mean(forced_y))

yaxis2_levels <- select(
  df_to_plot,
  week_of_projection,
  forced_y
)

yaxis2_levels$week_of_projection <- paste(
  "Week",
  yaxis2_levels$week_of_projection
)
yaxis2_levels$date <- max(df_to_plot$date_of_projection) + 14

ggplot(
  df_to_plot,
  aes(date_of_projection, forced_y, col = alert_type_high)
) +
  geom_point(size = 1.2) +
  scale_colour_manual(values = c(
    `Missed Alert` = "red",
    `True Alert` = "#009E73",
    `False Alert` = "#E69F00"
  )) +
  scale_y_continuous(
    breaks = yaxis_levels$y,
    labels = yaxis_levels$country
  ) +
  mriids_plot_theme$theme +
  mriids_plot_theme$legend +
  theme(axis.text.y = element_text(
    angle = 90,
    hjust = 0.5,
    size = 15
  )) +
  scale_x_date(date_breaks = "3 months") +
  xlab("") + ylab("") +
  geom_text(
    data = yaxis2_levels,
    aes(
      x = date,
      y = forced_y,
      label = week_of_projection
    ),
    size = 3,
    inherit.aes = FALSE
  ) +
  coord_cartesian(
    xlim = range(df_to_plot$date_of_projection),
    clip = "off"
  )

ggsave(
    here::here(
        all_files[[datasource]]$outdir,
        paste0(
            "alerts_per_week_high_",
            twindow,
            "_",
            n.dates.sim,
            ".png"
        )
   )
)
