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
        "Stable" = "#8975ca",
        "Overall" = "#000000"
    ),
    dateformat = scales::date_format("%m-%Y"),
    theme = ggplot2::theme_classic(base_size = 10),
    xticklabels = ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0, hjust = 0.5, size = 6)),
    onecol_theme = ggplot2::theme_classic(base_size = 6),
    legend = ggplot2::theme(legend.position = "none"),
    single_col_width = 8.7,
    single_col_height = 8.5,
    double_col_width = 17,
    double_col_height = 15,
    one_n_half_col_width = 11,
    one_n_half_col_height = 10,
    units = "cm"
)
