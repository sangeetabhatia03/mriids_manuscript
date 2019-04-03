mriids_plot_theme <- function(p) {

    color_scale <- c(
        "HealthMap.FALSE" = "#000000",
        "HealthMap" = "#000000",
        "HealthMap.TRUE" = "#7A7A7A",
        "WHO.FALSE" = "#EDDF1E",
        "WHO" = "#ffd370",
        "WHO.TRUE" = "#F9F4AF",
        "ProMED.TRUE" = "#cfeaf9",
        "ProMED.FALSE" = "#229de2",
        "ProMED" = "#229de2"
    )
    p <- p +
        theme_minimal() +
        theme(legend.position = "none") +
        scale_color_manual(values = color_scale) +
        scale_fill_manual(values = color_scale) +
        xlab("")

    p

}
