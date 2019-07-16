mriids_plot_theme <- list(

    color_scale = c(
        "HealthMap.FALSE" = "#009e73",
        "HealthMap" = "#009e73",
        "HealthMap.TRUE" = "#b2e1d5",
        "WHO.FALSE" = "#ffd370",
        "WHO" = "#ffd370",
        "WHO.TRUE" = "#F9F4AF",
        "ProMED.TRUE" = "#a6d7f3",
        "ProMED.FALSE" = "#229de2",
        "ProMED" = "#229de2"
    ),
    phase_color_scale = c(
        "Growing" = "#cb5683",
        "Declining" = "#71a659",
        "Neither" = "#8975ca",
        "Overall" = "#000000"
    ),

    theme = ggplot2::theme_classic(),
    legend = ggplot2::theme(legend.position = "none")
)
