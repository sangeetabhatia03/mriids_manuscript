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

all_quantiles <- all_quantiles[all_quantiles$twindow == twindow, ]
all_quantiles$Source <- factor(all_quantiles$Source)

p <- ggplot(all_quantiles) +
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
                    ncol = 1,
                    scales = "free_y")
p <- p + theme(axis.text.x = element_text(
                   angle = 90,
                   hjust = 0
               ))
p <- p + xlab("") + ylab("")
p <- p + scale_x_date(date_labels = "%b-%Y")
p <- p +
    expand_limits(y = 0) +
    mriids_plot_theme$theme +
    mriids_plot_theme$legend +
    scale_color_manual(values = mriids_plot_theme$color_scale) +
    scale_fill_manual(values = mriids_plot_theme$color_scale)

