mriids_plot_theme <- list(

    color_scale = c(
        "HealthMap.FALSE" = "#000000",
        "HealthMap" = "#000000",
        "HealthMap.TRUE" = "#7A7A7A",
        "WHO.FALSE" = "#ffd370",
        "WHO" = "#ffd370",
        "WHO.TRUE" = "#F9F4AF",
        "ProMED.TRUE" = "#cfeaf9",
        "ProMED.FALSE" = "#229de2",
        "ProMED" = "#229de2"
    ),
    phase_color_scale = c(
        "Growing" = "#cb5683",
        "Declining" = "#71a659",
        "Neither" = "#8975ca",
        "Overall" = "#000000"
    ),

    theme = theme_classic(),
    legend = theme(legend.position = "none")
)
