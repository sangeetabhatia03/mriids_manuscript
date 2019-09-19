library(ggplot2)
library(sf)

importation_risk_map <- function(df, canvas) {

    p2 <- canvas +
        geom_sf(data = df,
                aes(fill = normalised_risk),
                lwd = 0.1,
                alpha = 0.5
                )
    p2 <- p2 + facet_wrap( ~risk_to_on, labeller = label_wrap_gen(width = 10))

    p2 <- p2 + coord_sf(datum = NA)
    p2 <- p2 + mriids_plot_theme$theme
    p2 <- p2 + mriids_plot_theme$legend
    p2 <- p2 + theme(strip.background = element_blank())
    p2

}

country_risk <- function(country, time_window, tproj, date_of_first_alert) {
    infile <- glue::glue(
        "{datasource}_importation_risk_quantiles_{tproj}_{time_window}.csv"
        )
    message("Reading ", infile)
    risk <- readr::read_csv(
        here::here(
            all_files[[datasource]]$outdir,
            infile
        )
        )
    message("Risk to ", country)
    risk <- dplyr::filter(risk, risk_to == country)
    risk$normalised_risk <- normalise(risk$med)
    risk$date_of_first_alert <- date_of_first_alert
    risk
}


first_weekly_alert <- readr::read_csv(
  file = here::here(
    all_files[[datasource]]$outdir,
    glue::glue("{datasource}_date_of_first_weekly_alert.csv")
  )
)


out <- split(
    first_weekly_alert,
    list(
        first_weekly_alert$time_window,
        first_weekly_alert$n.dates.sim,
        first_weekly_alert$alert_type
    ),
    sep = "_"
)

fname <- here::here("data/Africa_SHP")
africa <- sf::st_read(fname)
africa$ISO3c <- countrycode::countrycode(
    africa$COUNTRY,
    destination = "iso3c",
    origin = "country.name"
)

importation_risk <- purrr::map(
    out,
    function(df) {
        ## Each row of df is a country, and for each country,
        ## the date of first alert is different.
        df <- dplyr::select(df, country, time_window, tproj, date_of_first_alert)
        risk <- purrr::pmap_dfr(df, country_risk)
        ## Organise label for facets.
        risk$risk_to_on <- paste(
            country_name(risk$risk_to),
            risk$date_of_first_alert
        )
        risk$risk_to_on <- stringr::str_wrap(risk$risk_to_on, width = 20)
        risk
    }
)

names(importation_risk) <- names(out)

canvas <- ggplot()
canvas <- canvas + xlab("") + ylab("")
canvas <- canvas + theme(
               panel.ontop = FALSE,
               panel.grid = element_blank(),
               line = element_blank(),
             rect = element_blank())

canvas <- canvas + scale_fill_gradient(
             low = "#ffcccc",
             high = "#ff0000",
             name = "Relative Risk",
             na.value = "white"
         )

purrr::iwalk(importation_risk, function(x, y) {
    sources <- dplyr::filter(africa, ISO3c %in% x$risk_to)
    sources <- dplyr::left_join(
        sources,
        x,
        by = c("ISO3c" = "risk_to")
    )
    canvas2 <- canvas +
        geom_sf(data = sources,
                aes(geometry = geometry),
                fill = "#323232",
                lwd = 0.1
                )
    x$normalised_risk[x$normalised_risk == 0] <- NA
    joined <- dplyr::right_join(
           africa,
           x,
           by = c("ISO3c" = "risk_from")
           )

    p <- importation_risk_map(joined, canvas2)
    outfile <- here::here(all_files[[datasource]]$outdir, glue::glue("{y}.pdf"))
    message("Saving to ", outfile)
    ggplot2::ggsave(
        filename = outfile,
        plot = p,
        width = mriids_plot_theme$single_col_width,
        height = mriids_plot_theme$single_col_height,
        units = mriids_plot_theme$units
   )
 }
)
