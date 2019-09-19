library(ggplot2)
library(sf)
library(gridExtra)

importation_risk_map <- function(df, canvas) {

    p2 <- canvas +
        geom_sf(data = df,
                aes(fill = normalised_risk),
                lwd = 0.1,
                alpha = 0.5
                )
    ##p2 <- p2 + facet_wrap( ~risk_to_on, labeller = label_wrap_gen(width = 10))
    p2 <- p2 + coord_sf(datum = NA)
    p2 <- p2 + mriids_plot_theme$legend
    p2 <- p2 + theme(strip.background = element_blank())
    p2

}

country_risk <- function(country, time_window, tproj) {
    infile <- glue::glue(
        "{datasource}_importation_risk_quantiles_{tproj}_{time_window}.csv"
        )
    infile <- here::here(
            all_files[[datasource]]$outdir,
            infile
        )
    if (!file.exists(infile)) NULL
    message("Reading ", infile)
    risk <- readr::read_csv(infile)
    message("Risk to ", country)
    risk <- dplyr::filter(risk, risk_to == country)
    risk$normalised_risk <- normalise(risk$med)
    risk
}


## Importation risk before the first observed case in a country.
first_case <- readr::read_csv(
    file = here::here(
        all_files[[datasource]]$outdir,
        glue::glue("{datasource}_first_observed_case.csv")
    )
  )

first_case <- dplyr::arrange(first_case, twindow)

importation_risk <- purrr::pmap(
    first_case,
    function(country, date_first_obs, tproj, twindow, date) {
        risk <- country_risk(country, twindow, tproj)
        ## Organise label for facets.

        risk$risk_on <- date

        risk
    }
)

names(importation_risk) <- first_case$twindow

importation_risk <- dplyr::bind_rows(importation_risk, .id = "twindow")
importation_risk <- dplyr::arrange(importation_risk, risk_on, risk_to)

atrisk <- countrycode::countrycode(importation_risk$risk_to, "iso3c", "country.name")
importation_risk$risk_to_on <- glue::glue(
    "{atrisk}\n{importation_risk$risk_on}"
    )

importation_risk$risk_to_on <- factor(
    importation_risk$risk_to_on,
    levels = unique(importation_risk$risk_to_on),
    ordered = TRUE
)

##risk$risk_to_on <- stringr::str_wrap(risk$risk_to_on, width = 20)
importation_risk$twindow <- as.integer(importation_risk$twindow)
out <- split(importation_risk, importation_risk$twindow)

fname <- here::here("data/Africa_SHP")
africa <- sf::st_read(fname)
africa$ISO3c <- countrycode::countrycode(
    africa$COUNTRY,
    destination = "iso3c",
    origin = "country.name"
)

## Only plot W African countries
wafrica <- countrycode::countrycode(
    c("Mauritania",
      "Senegal",
      "Mali",
      "Gambia",
      "Guinea-Bissau",
      "Guinea",
      "Sierra Leone",
      "Liberia",
      "Côte d'Ivoire",
      "Ghana",
      "Burkina Faso",
      "Nigeria",
      ##"Niger",
      "Cameroon",
      "Togo",
      "Benin"
    ),
    destination = "iso3c",
    origin = "country.name"
)

africa <- dplyr::filter(africa, ISO3c %in% wafrica)
canvas <- ggplot()
canvas <- canvas + xlab("") + ylab("")
canvas <- canvas + theme(
               panel.ontop = FALSE,
               panel.grid = element_blank(),
               line = element_blank(),
             rect = element_blank())

## canvas <- canvas + scale_fill_gradient(
##              low = "#ffcccc",
##              high = "#ff0000",
##              name = "Relative Risk",
##              na.value = "white"
##          )

canvas <- canvas +
    scale_fill_gradient(
        low = "blue",
        high = "red",
        name = NULL,
        breaks = c(0, 0.99),
        labels = c("low", "high")
       )


    ## scale_fill_viridis_c(
    ##     alpha = 1,
    ##     begin = 0,
    ##     end = 1,
    ##     direction = 1,
    ##     option = "D",
    ##     aesthetics = "fill",
    ##     name = NULL,
    ##     breaks = c(0, 0.99),
    ##     labels = c("low", "high")
    ## )

## Keep putattive sources ready
importation_sources <- read.csv(
    here::here("data/putative_importation_source.csv")
)


gin <- readr::read_rds(here::here("data/gadm36_GIN_0_sf.rds"))
gin_centroid <- sf::st_centroid(gin$geometry)
gin$x <- sf::st_coordinates(gin_centroid)[, 1]
gin$y <- sf::st_coordinates(gin_centroid)[, 2]

lbr <- readr::read_rds(here::here("data/gadm36_LBR_0_sf.rds"))
lbr_centroid <- sf::st_centroid(lbr$geometry)
lbr$x <- sf::st_coordinates(lbr_centroid)[, 1]
lbr$y <- sf::st_coordinates(lbr_centroid)[, 2]

sources <- list(GIN = gin, LBR = lbr)

purrr::iwalk(out, function(x, y) {

    x$risk_to_on <- factor(x$risk_to_on, ordered = TRUE)
    x$normalised_risk <- round(x$normalised_risk, digits = 3)
    x2 <- split(x, x$risk_to)
    plots <- purrr::map(
        x2,
        function(df) {
            df$normalised_risk[df$risk_from == df$risk_to] <- NA
            joined <- dplyr::right_join(
                africa,
                df,
                by = c("ISO3c" = "risk_from")
            )
            p <- importation_risk_map(joined, canvas)
            ## plot putatative sources
            from <-
                importation_sources[importation_sources$importation_into == df$risk_to[1], "importation_from"]
            p <- p +
                annotate(
                    "point",
                    x = sources[[from]]$x,
                    y = sources[[from]]$y,
                    colour = "black",
                    size = 1
            )
            zero_risk_cntrs <- df$risk_from[which(df$normalised_risk == 0)]
            na_cntrs <- unique(df$risk_from[is.na(df$normalised_risk)])
            zero_risk_cntrs <- zero_risk_cntrs[! zero_risk_cntrs %in% na_cntrs]
            zero_risk <- dplyr::filter(africa, ISO3c %in% zero_risk_cntrs)
            zero_risk <- dplyr::left_join(
                zero_risk,
                df,
                by = c("ISO3c" = "risk_from")
                )
            p2 <- p +
                geom_sf(
                    data = zero_risk,
                    aes(geometry = geometry),
                    fill = "white",
                    lwd = 0.1
                )
            p2 <- p2 + ggtitle(label = df$risk_to_on[1])
            p2 <- p2 + theme_void()
            p2 <- p2 + theme(
                           plot.title = element_text(size = 6),
                           legend.text = element_text(size = 6),
                           legend.margin = margin(0, 0, 0, 0),
                           legend.text.align = c(0, 1)
                         )
            p2

        }
        )

  legend <- cowplot::get_legend(plots[[1]] +
    theme(
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.text.align = 0.2,
        legend.text = element_text(size = 4)
    )
    )

    lbr <- plots[["LBR"]] + theme(legend.position = "none")
    sle <- plots[["SLE"]] + theme(legend.position = "none")
    nga <- plots[["NGA"]] + theme(legend.position = "none")
    sen <- plots[["SEN"]] + theme(legend.position = "none")
    mli <- plots[["MLI"]] + theme(legend.position = "none")

    layout_matrix <- rbind(
        c(1, 1, 2, 2, 3, 3),
        c(1, 1, 2, 2, 3, 3),
        c(4, 4, 5, 5, NA, 6),
        c(4, 4, 5, 5, NA, 6)
    )
    compiled <- arrangeGrob(lbr, sle, nga, sen, mli, legend, layout_matrix = layout_matrix)

    outfile <- glue::glue("importation_risk_first_obs_{y}.pdf")
    ggsave(
        filename = here::here(all_files[[datasource]]$outdir, outfile),
        plot = compiled,
        width = mriids_plot_theme$single_col_width,
        height = mriids_plot_theme$single_col_height,
        units = mriids_plot_theme$units
    )
 }
)

