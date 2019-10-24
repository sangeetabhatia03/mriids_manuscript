params <-list(
    n.dates.sim = 28L,
    twindow = 14L,
    places = c("SLE", "LBR", "GIN"),
    indirs = c("data/output", "data/promed_stanfits_gamma_ul_10_output"),
    datasource = "ProMED"
)

## ----setup, eval = TRUE--------------------------------------------------
library(dplyr)
library(purrr)
library(ggplot2)
source(here::here("analysis/common_plot_properties.R"))





## ----forecasts-assess-by-time-window-2-----------------------------------

infiles <- here::here(
    params$indirs,
    glue::glue("{params$datasource}_weekly_metrics.csv")
  )


names(infiles) <- c("2", "10")

weekly_metrics <- purrr::map_dfr(infiles, readr::read_csv, .id = "gamma_upper_limit")

weekly_metrics <- weekly_metrics[
    weekly_metrics$n.dates.sim == params$n.dates.sim &
    weekly_metrics$twindow == params$twindow,
]
weekly_metrics <-  dplyr::rename(weekly_metrics, relsharpness = "rel_sness")



weekly_metrics <- dplyr::mutate_if(
  weekly_metrics,
  is.character,
  factor
)
weekly_metrics$week_of_projection <- factor(weekly_metrics$week_of_projection)
weekly_metrics$tproj <- as.integer(weekly_metrics$tproj)
weekly_metrics$twindow <- factor(weekly_metrics$twindow)


## ----forecasts-assess-by-time-window-9-----------------------------------
pbias <- ggplot(
  weekly_metrics,
  aes(week_of_projection,
    bias,
    col = gamma_upper_limit
  )
) +
  geom_boxplot(position = "dodge", outlier.size = 1, outlier.stroke = 0.1)
pbias <- pbias +
  xlab("") +
  ylab("Bias") +
  mriids_plot_theme$onecol_theme +
  NULL

pbias


## ----forecasts-assess-by-time-window-10----------------------------------

psharp <- ggplot(
  weekly_metrics,
  aes(week_of_projection,
    relsharpness,
    col = gamma_upper_limit
  )
) +
  geom_boxplot(outlier.size = 1, outlier.stroke = 0.1)

psharp <- psharp +
  xlab("") +
  ylab("sharpness") +
  mriids_plot_theme$onecol_theme

psharp


## ----forecasts-assess-by-time-window-11----------------------------------

propinci_perc <- group_by(
    weekly_metrics,
    week_of_projection,
    gamma_upper_limit
) %>% summarise(nweeks = 100 * sum(propinci) / n())

propinci_perc <- dplyr::ungroup(propinci_perc)

prel <- ggplot(
  propinci_perc,
  aes(week_of_projection,
    nweeks,
    fill = gamma_upper_limit
  )
) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5)
## geom_point(aes(fill = ci),
##            alpha = 0.7,
##            position = position_jitterdodge())
prel <- prel +
  xlab("") +
    ylab(
        stringr::str_wrap("% weeks in 95% forecast interval", width = 15)
    ) +
  mriids_plot_theme$onecol_theme +
  ylim(0, 100)


prel


## ----forecasts-assess-by-time-window-12----------------------------------
prelerr <- ggplot(
  weekly_metrics,
  aes(week_of_projection,
    rmae,
    col = gamma_upper_limit
  )
) +
geom_boxplot(outlier.size = 1, outlier.stroke = 0.1)

prelerr <- prelerr +
  mriids_plot_theme$onecol_theme +

  NULL


prelerr <- prelerr +
  xlab("") +
  ylab("relative mean absolute error")

prelerr


## ----forecasts-assess-by-time-window-13----------------------------------

## Affix legend manually otherwise I can't get rid of the
## annoying legend title.
legend <- cowplot::get_legend(pbias +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
  )

prow <- cowplot::plot_grid(
  prel + theme(legend.position = "none"),
  prelerr + theme(legend.position = "none"),
  pbias + theme(legend.position = "none"),
  psharp + theme(legend.position = "none"),
  align = "vh",
  labels = c("A", "B", "C", "D"),
  label_size = 8,
  hjust = c(-1, 1, -3, 1),
  vjust = c(3, 3, -2.5, 0),
  nrow = 2
)


# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- cowplot::plot_grid(prow,
  legend,
  ncol = 1,
  rel_heights = c(1, .1)
)

filename <- glue::glue(
    "{Sys.Date()}_{params$datasource}_forecasts_assess_by_gamma_ul_",
    "{params$twindow}_{params$n.dates.sim}.pdf"
)
filename <- here::here(
    glue::glue("ms-figures/si-figures/other/{Sys.Date()}"),
    filename
)
cowplot::save_plot(
    filename = filename,
    plot = p,
    base_height = mriids_plot_theme$single_col_height / 2.5,
    base_width = mriids_plot_theme$single_col_width / 2.5
)



prelerr_log <- prelerr +
      scale_y_log10(
    breaks = scales::trans_breaks(trans = "log10", inv = function(x) 10^x),
    labels = scales::trans_format("log10", scales::math_format(.x))) +
     ylab("log(rmae)") +   theme(
        axis.title = ggplot2::element_text(margin = margin(t = 0, r = 0, b = 0, l = 0), size = 6)
        )

prow <- cowplot::plot_grid(
  prel + theme(legend.position = "none"),
  prelerr_log + theme(legend.position = "none"),
  pbias + theme(legend.position = "none"),
  psharp + theme(legend.position = "none"),
  align = "vh",
  labels = c("A", "B", "C", "D"),
  label_size = 8,
  hjust = c(-1, 1, -3, 1),
  vjust = c(3, 3, -2.5, 0),
  nrow = 2
)


# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p <- cowplot::plot_grid(prow,
  legend,
  ncol = 1,
  rel_heights = c(1, .1)
)

filename <- glue::glue(
    "{Sys.Date()}_{params$datasource}_forecasts_assess_by_gamma_ul_",
    "log_scale_{params$twindow}_{params$n.dates.sim}.pdf"
)
filename <- here::here(
    glue::glue("ms-figures/si-figures/other/{Sys.Date()}"),
    filename
)
cowplot::save_plot(
    filename = filename,
    plot = p,
    base_height = mriids_plot_theme$single_col_height / 2.5,
    base_width = mriids_plot_theme$single_col_width / 2.5
)
