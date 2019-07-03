## provide a named list of files
compile_forecasts <- function(infiles, twindow, country, n.dates.sim) {
    message("Reading ", infiles)
    df <- purrr::map_dfr(
        infiles,
        ~ readr::read_csv(.x),
        .id = "datasource"
        )
    df <- df[df$country == country &
             df$time_window == twindow &
             df$n.dates.sim == n.dates.sim, ]
    df
}

## named list of weekly incidence files
compile_incidence <- function(infiles, country) {
   message("Reading ", infiles)
   df <- purrr::map_dfr(
        infiles,
        ~ readr::read_csv(.x),
        .id = "datasource"
        )
    df <- df[df$country == country, ]
    df
}

library(dplyr)
library(ggplot2)
library(scales)

source(here::here("analysis/common_plot_properties.R"))

indirs <- purrr::map(
    datasources,
    ~ all_files[[.x]]$outdir
)
infiles <- purrr::map(
    indirs,
    ~ here::here(.x, "all_forecasts_consolidated.csv")
)
names(infiles) <- datasources

forecasts <- compile_forecasts(infiles, tw, place, ndates)
forecasts$tproj <- as.integer(forecasts$tproj)
forecasts$time_window <- as.integer(forecasts$time_window)
forecasts$country <- forcats::fct_recode(forecasts$country,
  Guinea = "GIN",
  Liberia = "LBR",
  `Sierra Leone` = "SLE"
)
forecasts <- dplyr::mutate_at(
  forecasts,
  c("y", "ymin", "ymax"),
  ~ .x + 1
)

non_overlapping <- seq(
  from = min(forecasts$tproj),
  to = max(forecasts$tproj),
  by = forecasts$n.dates.sim[1]
)

forecasts <- forecasts[forecasts$tproj %in% non_overlapping, ]
forecasts$datasource <- factor(forecasts$datasource)





## Read in the weekly incidence in tall form.
infiles <- purrr::map(
    datasources,
    ~ here::here(all_files[[.x]]$weekly_incidfile)
)
names(infiles) <- datasources
incid <- compile_incidence(infiles, place)

incid$country <- forcats::fct_recode(incid$country,
  Guinea = "GIN",
  Liberia = "LBR",
  `Sierra Leone` = "SLE"
)
incid$incid <- incid$incid + 1

incid_ref <- incid[incid$datasource == "WHO", ]



p <- ggplot()
p <- p + geom_ribbon(
  data = forecasts,
  aes(
    x = date,
    ymin = ymin,
    ymax = ymax,
    group = interaction(
        tproj,
        datasource),
    fill = datasource
  ),
  ##fill = "gray",
  alpha = 0.3
)

p <- p + geom_line(
  data = forecasts,
  aes(
    x = date,
    y = y,
    group = interaction(
        tproj,
        datasource),
    col = datasource
  )
)

p <- p +
    scale_y_log10(
        breaks = scales::trans_breaks("log10", function(x) 10^x),
        labels = scales::trans_format("log10", scales::math_format(10^.x))
    )


p <- p +
  xlab("") +
  ylab("log(weekly incidence)")

p <- p +
  scale_x_date(
    date_breaks = "3 month",
    labels = scales::date_format("%b %Y")
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0))



p <- p +
    scale_colour_manual(values = mriids_plot_theme$color_scale) +
    scale_fill_manual(values = mriids_plot_theme$color_scale) +
  mriids_plot_theme$theme +
  mriids_plot_theme$legend

p1 <- p + geom_point(
  data = incid_ref,
  aes(date,
    incid
    ),
  col = "black"
)


p1

## Option 2; facetted. This will allow us to see the datasource
## specific incidence as well.

p2 <- p + geom_point(
  data = incid,
  aes(date,
      incid,
      col = interaction(
          datasource,
          interpolated)
    )
)

p2 <- p2 + facet_wrap(~datasource, nrow = 3)

outfile <- paste(place, tw, ndates, "non-facetted.pdf", sep = "_")
message(outfile)
ggsave(
    filename = paste0("ms-figures/", outfile),
    plot = p1
)

outfile <- paste(place, tw, ndates, "facetted.pdf", sep = "_")
message(outfile)
ggsave(
    filename = paste0("ms-figures/", outfile),
    plot = p2
)