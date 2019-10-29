datasources <- c("ProMED", "HealthMap", "WHO")
names(datasources) <- datasources
all_files[["ProMED"]]$outdir <- "data/promed_stanfits_gamma_ul_10_output"
all_files[["HealthMap"]]$outdir <- "data/healthmap_gamma_ul_10_output"
all_files[["WHO"]]$outdir <- "data/who_stanfits_gamma_ul_10_output"


all_quantiles <- purrr::map_dfr(
    datasources,
    function(ds) {
        qntls <- readr::read_csv(
            here::here(
                all_files[[ds]]$outdir,
                "gravity_model_parameters_quantiles.csv"
               )
            )
        qntls
    },
    .id = "Source"
)

all_quantiles$Source <- factor(all_quantiles$Source)
all_qntls_by_tw <- split(all_quantiles, all_quantiles$twindow)

library(ggplot2)
purrr::iwalk(
    all_qntls_by_tw,
    function(df, y) {
        outfile <- glue::glue(
            "{Sys.Date()}_gravity_model_pars_{y}.pdf"
        )
        outfile <- here::here("ms-figures", outfile)
        p <- ggplot(df) +
            geom_line(aes(
                date,
                `50%`,
                col = Source
            ))
        p <- p + geom_ribbon(aes(
                     x = date,
                     ymin = `2.5%`,
                     ymax = `97.5%`,
                     fill = Source
                 ),
                 alpha = 0.3)

        p <- p + facet_wrap(~param,
                            ncol = 2,
                            scales = "free_y")
        p <- p + xlab("") + ylab("")
        p <- p + scale_x_date(labels = mriids_plot_theme$dateformat)
        p <- p + mriids_plot_theme$xticklabels
        p <- p +
            expand_limits(y = 0) +
            mriids_plot_theme$onecol_theme +
            mriids_plot_theme$legend +
            scale_color_manual(values = mriids_plot_theme$color_scale) +
            scale_fill_manual(values = mriids_plot_theme$color_scale)
        ggsave(
            filename = outfile,
            plot = p,
            units = mriids_plot_theme$units,
            width = mriids_plot_theme$single_col_width,
            height = mriids_plot_theme$single_col_height/2
        )
    }
 )




