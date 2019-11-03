#####################################################################
## Maps.
#####################################################################
library(ggplot2)
library(gganimate)

incid_pred <- vroom::vroom(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{Sys.Date() - 1}_weekly_alerts.csv")
    )
  )


nonna_alerts <- dplyr::filter(incid_pred, threshold == "50%")
nonna_alerts <- dplyr::filter(nonna_alerts, time_window == 14)
nonna_alerts <- dplyr::filter(nonna_alerts, n.dates.sim == 28)



fname <- here::here("data/Africa_SHP")
africa <- sf::st_read(fname)
africa$ISO3c <- countrycode::countrycode(
    africa$COUNTRY,
    destination = "iso3c",
    origin = "country.name"
)


joined <- dplyr::left_join(
    africa,
    nonna_alerts,
    by = c("ISO3c" = "country")
)




joined$date_obs <- lubridate::ymd(joined$date_obs)
joined$week_of_year <- week_of_year(joined$date_obs)
joined$week_of_year <- factor(joined$week_of_year)


joined1 <- joined[joined$week_of_projection == 1, ]


p <- ggplot(data = joined1)
p <- p + xlab("") + ylab("")
p <- p + theme(
             panel.ontop = FALSE,
             panel.grid = element_blank(),
             line = element_blank(),
             rect = element_blank())

p <- p + coord_sf(datum = NA)
p <- p + scale_fill_manual(values = values <- c(
    `Missed Alert` = "#ff0000",
    `True Alert` = "#009E73",
    `False Alert` = "#cc8400",
    `No Alert` = "#ffffff"
    )
  )

p <- p + theme(legend.title = element_blank(),
               legend.position = "bottom")

p <- p + geom_sf(aes(fill = alert_type),
                 lwd = 0.1)

p <- p + transition_states(
    states = date_obs,
    transition_length = 2,
    state_length = 5
  ) + view_static()




gganimate::anim_save("whole_africa_21.gif", p)


## Restrict to few countries
countries <- c("Sierra Leone",
               "Guinea",
               "Liberia",
               "Cote d`Ivoire",
               "Mali",
               "Senegal",
               "Gambia",
               "Guinea-Bissau",
               "Burkina Faso",
               "Ghana",
               "Mauritania")


wafrica <- filter(joined,
                  COUNTRY %in% countries)


p <- ggplot()
p <- p + scale_fill_viridis_c(breaks = c(1, 3000, 4700),
                              alpha = 0.8,
                              na.value = "#fcf9d3")
p <- p + xlab("") + ylab("")
p <- p + theme(
             panel.ontop = FALSE,
             panel.grid = element_blank(),
             line = element_blank(),
             rect = element_blank())


p <- p + geom_sf(data = wafrica,
                 aes(fill = incid),
                 lwd = 0.1)

p <- p + coord_sf(datum = NA)
p <- p + facet_grid(week_of_year ~ source)
p <- p + theme(legend.title = element_blank(),
               legend.position = "bottom")

ggsave("wafrica.png", p)
