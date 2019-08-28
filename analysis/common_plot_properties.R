mriids_plot_theme <- list(

    color_scale = c(
        "HealthMap.FALSE" = "#009e73",
        "HealthMap" = "#009e73", ## green
        "HealthMap.TRUE" = "#b2e1d5",
        "WHO.FALSE" = "#ffd370", ## orange
        "WHO" = "#ffd370",
        "WHO.TRUE" = "#F9F4AF",
        "ProMED.TRUE" = "#a6d7f3",
        "ProMED.FALSE" = "#229de2",
        "ProMED" = "#229de2" ## blue
    ),
    phase_color_scale = c(
        "Growing" = "#cb5683",
        "Declining" = "#71a659",
        "Neither" = "#8975ca",
        "Overall" = "#000000"
    ),

    theme = ggplot2::theme_classic(),
    onecol_theme = ggplot2::theme_classic(base_size = 6),
    legend = ggplot2::theme(legend.position = "none"),
    single_col_width = 8.7,
    single_col_height = 8.5,
    double_col_width = 17,
    double_col_height = 15,
    one_n_half_col_width = 11,
    one_n_half_col_height = 11,
    units = "cm"
)
