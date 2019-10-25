datasources <- c("ProMED", "HealthMap", "WHO")
names(datasources) <- datasources
weekly_incid <- purrr::map_dfr(
    datasources,
    function(datasource) {
        infile <- here::here(
            all_files[[datasource]]$outdir,
            glue::glue(
                "{Sys.Date()}_{datasource}_",
                "processed_weekly_incidence.csv"
            )
          )
        out <- readr::read_csv(infile)
        out
    },
    .id = "datasource"
)
weekly_incid <- weekly_incid[weekly_incid$n.dates.sim != 14, ]
weekly_incid <- weekly_incid[weekly_incid$country %in%
                             c("GIN", "LBR", "SLE"), ]


byparams <- split(
    weekly_incid,
    list(weekly_incid$time_window, weekly_incid$n.dates.sim),
    sep = "_"
)

country_name <- function(x) {
    label <- countrycode::countrycode(
        x,
        origin = "iso3c",
        destination = "country.name"
        )
    label
}

plots <- purrr::map(
    byparams,
    function(df, param) {

        p <- ggplot(df,
                    aes(date,
                        incid,
                        col = interaction(datasource, interpolated))) +
            geom_point(shape = 20, size = 0.5) +
            scale_color_manual(values = mriids_plot_theme$color_scale)
        p <- p + facet_wrap(
                     ~country,
                     ncol = 1,
                     scales = "free_y",
                     labeller = as_labeller(country_name))
        p <- p + mriids_plot_theme$theme
        p <- p + xlab("") + ylab("Weekly incidence")
        p <- p + mriids_plot_theme$legend
        p


    }
)

purrr::iwalk(
    plots,
    function(p, filename) {
        filename <- glue::glue("{Sys.Date()}_weekly_incid_{filename}.pdf")
        indir <- glue::glue("ms-figures/si-figures/other/{Sys.Date()}")
        filename <- here::here(indir, filename)
        ggplot2::ggsave(
            filename = filename,
            plot = p,
            width = mriids_plot_theme$double_col_width,
            height = mriids_plot_theme$double_col_width /2,
            units = mriids_plot_theme$units
        )
    }
  )


## Correlations
wide_weekly <- purrr::map(
    byparams,
    function(df) {
        ## Correlation for dates where we have data from
        ## all three sources.
        start <- dplyr::group_by(df, datasource) %>%
            dplyr::summarise(start = min(date)) %>%
            dplyr::ungroup()
        start <- max(start$start)

        end <- dplyr::group_by(df, datasource) %>%
            dplyr::summarise(end = max(date)) %>%
            dplyr::ungroup()
        end <- min(end$end)
        df <- dplyr::filter(df, date > start, date < end)

        out <- tidyr::spread(
            data = dplyr::select(
                df, datasource, week_of_year, country, incid),
            key = datasource,
            value = incid
            )

        out

    }
  )

coeffs_overall <- purrr::map_dfr(
    wide_weekly,
    function(out) {
        mat <- as.matrix(out[ ,c("HealthMap", "ProMED", "WHO")])
        broom::tidy(Hmisc::rcorr(mat))

    },
    .id = "param"

)

## coeffs_overall[["14_28"]]
##           HealthMap ProMED  WHO
## HealthMap      1.00   0.41 0.74
## ProMED         0.41   1.00 0.44
## WHO            0.74   0.44 1.00

## n= 228


## P
##           HealthMap ProMED WHO
## HealthMap            0      0
## ProMED     0                0
## WHO        0         0


coeffs_bycountry <- purrr::map_dfr(
    wide_weekly,
    function(out) {
        bycountry <- split(out, out$country)
        coeffs <- purrr::map_dfr(
            bycountry, function(df) {
                mat <- as.matrix(df[ ,c("HealthMap", "ProMED", "WHO")])
                broom::tidy(Hmisc::rcorr(mat))
            },
            .id = "country"
        )
        coeffs

    },
    .id = "param"

)

## > coeffs_bycountry[["14_28"]]
## $GIN
##           HealthMap ProMED  WHO
## HealthMap      1.00   0.74 0.86
## ProMED         0.74   1.00 0.73
## WHO            0.86   0.73 1.00

## n= 76


## P
##           HealthMap ProMED WHO
## HealthMap            0      0
## ProMED     0                0
## WHO        0         0

## $LBR
##           HealthMap ProMED  WHO
## HealthMap      1.00   0.38 0.63
## ProMED         0.38   1.00 0.21
## WHO            0.63   0.21 1.00

## n= 76


## P
##           HealthMap ProMED WHO
## HealthMap           0.0008 0.0000
## ProMED    0.0008           0.0661
## WHO       0.0000    0.0661

## $SLE
##           HealthMap ProMED  WHO
## HealthMap      1.00   0.35 0.79
## ProMED         0.35   1.00 0.50
## WHO            0.79   0.50 1.00

## n= 76


## P
##           HealthMap ProMED WHO
## HealthMap           0.0018 0.0000
## ProMED    0.0018           0.0000
## WHO       0.0000    0.0000

outdir <- glue::glue("ms-figures/si-figures/other/{Sys.Date()}")
outfile <- glue::glue("{Sys.Date()}_weekly_incid_correlation_overall.csv")
outfile <- here::here(outdir, outfile)
readr::write_csv(x = coeffs_overall, path = outfile)

outfile <- glue::glue("{Sys.Date()}_weekly_incid_correlation_bycountry.csv")
outfile <- here::here(outdir, outfile)
readr::write_csv(x = coeffs_bycountry, path = outfile)
