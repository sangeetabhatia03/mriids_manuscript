#####################################################################
## Maps.
#####################################################################
library(ggplot2)
library(gganimate)

incid_pred <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    "incidence_forecasts.csv"
  )
)

incid_pred <- incid_pred[incid_pred$time_window == twindow &
                         incid_pred$n.dates.sim == n.dates.sim, ]

incid_pred <- select(incid_pred, date, country, y, incid)
fname <- here::here("data/Africa_SHP")
africa <- sf::st_read(fname)
africa$ISO3c <- countrycode::countrycode(africa$COUNTRY,
                                         destination = "iso3c",
                                         origin = "country.name")


joined <- left_join(
    africa,
    incid_pred,
    by = c("ISO3c" = "country")
)

## Reshape joined so that we can facet on observed and
## predicted.
joined <- tidyr::gather(
   joined,
   key = "predicted/observed",
   value = "y",
   y, incid
   )

joined <- dplyr::mutate_at(
    joined,
    "predicted/observed",
    funs(ifelse(. == "y", "predicted", "observed"))
    )

joined$`predicted/observed` <- factor(
    joined$`predicted/observed`,
    levels = c("observed", "predicted")
)

## NAs for Western Sahara.
## joined <- na.omit(joined)

## set 0s to NA so that we can color it differently
joined <- dplyr::mutate_at(
   joined,
   vars(y),
   list(y = ~ ifelse(. > 0, ., NA)
  )
 )


## Labels as week of year

week_labeller <- function(var) {

    label <- paste(lubridate::year(var),
                   ": Week ",
                   lubridate::week(var))
    label
}
joined$date <- lubridate::ymd(joined$date)
joined$week_of_year <- week_labeller(joined$date)
joined$week_of_year <- factor(joined$week_of_year)






library(viridis)
p <- ggplot(data = joined)
p <- p + scale_fill_viridis_c(breaks = c(1000, 2000, 3000, 4000),
                              alpha = 0.8,
                              na.value = "#fefdf6")
p <- p + xlab("") + ylab("")
p <- p + theme(
             panel.ontop = FALSE,
             panel.grid = element_blank(),
             line = element_blank(),
             rect = element_blank())

p <- p + facet_wrap(~ `predicted/observed`, nrow = 1)

p <- p + coord_sf(datum = NA)

p <- p + theme(legend.title = element_blank(),
               legend.position = "bottom")

p <- p + geom_sf(aes(fill = y),
                 lwd = 0.1)

p <- p + transition_time(
    date,
    transition_length = 2,
    state_length = 2
  ) + view_static()




ggsave("whole_africa_21.png", p)


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
