library(ggplot2)
library(dplyr)
library(ggthemes)
library(purrr)
library(patchwork)

source(here::here("analysis/common_plot_properties.R"))
gravity_model_pars_quantiles <- function(infiles, dates) {
    par_samples <- purrr::map_dfr(
                              infiles,
                              ~ readr::read_rds(here::here(.x))
                          )
    prob <- c(0.025, 0.5, 0.975)
    par_quantiles <- purrr::map_df(par_samples,
                                   ~ quantile(
    .x,
    prob
))

    par_quantiles$prob <- prob
    par_quantiles <- tidyr::gather(par_quantiles,
                                   key = "params",
                                   value = "val",
                                   -prob,
                                   factor_key = FALSE
                                   )
    ## This messes with the order of params so that
    ## they are no longer in the correct order.
    par_quantiles <- tidyr::spread(par_quantiles, prob, val)
    ## To fix this, we separate and then sort.
    par_quantiles <- tidyr::separate(par_quantiles,
                                     col = params,
                                     into = c("tproj", "twindow"),
                                     sep = "_"
                                     )
    par_quantiles$tproj <- as.integer(par_quantiles$tproj)
    par_quantiles <- arrange(par_quantiles, tproj)
    ## attach to pstay_quantiles
    par_quantiles$date <- lubridate::ymd(dates)
    par_quantiles
}

plot_gravity_model_pars <- function(par_quantiles) {

    p <- ggplot(par_quantiles) +
        geom_line(aes(
            date,
            `0.5`,
            col = Source
        ))
    p <- p + geom_ribbon(aes(
                 x = date,
                 ymin = `0.025`,
                 ymax = `0.975`,
                 fill = Source
             ),
             alpha = 0.3)

    p <- p + facet_wrap(~parameter,
                        ncol = 1,
                        scales = "free_y")
    p <- p + theme(axis.text.x = element_text(
                       angle = 90,
                       hjust = 0
                   ))
    p <- p + xlab("") + ylab("")
    p <- p + scale_x_date(date_labels = "%b-%Y")
    p
}

indir <- list(ProMED = "data/stanfits/",
              WHO = "data/who_stanfits/")
incidfiles <- list(ProMED = "data/processed/20122018_promed_loglinear_wide.csv",
                   WHO = "data/processed/01032019_who_bycountry.csv")

parameters <- list(gamma = "gamma_samples_",
                   pstay = "pstay_samples_")
param_quantiles <- purrr::map2_dfr(indir,
                                  incidfiles,
                                  function(x, y) {
                                      ## get dates
                                      incid <- readr::read_csv(here::here(y))
                                      out <-
                                      map_dfr(parameters,
                                          function(par) {
                                              param_files <-list.files(path = x,
                                                                       pattern = paste0(par,
                                                                                        "[0-9]*_14.rds"),
                                                                       full.names = FALSE)
                                              param_names <- stringr::str_replace(param_files,
                                                                                  par,
                                                                                  "")
                                              param_names <- stringr::str_replace(param_names,
                                                                                  ".rds",
                                                                                  "")
                                              param_files <- paste0(x,
                                                                    param_files)
                                              names(param_files) <-
                                                  param_names
                                              tproj <- stringr::str_split(param_names,
                                                                  "_") %>%
                                                  map(`[`(1)) %>%
                                                  unlist
                                              tproj <- as.integer(tproj) %>%
                                                  sort
                                              dates <- incid$date[tproj]
                                              quantiles  <-
                                                  gravity_model_pars_quantiles(param_files,
                                                                               dates)

                                              quantiles
                                          },
                                          .id = "parameter")
                                      out},
                                  .id = "Source")


p1 <- plot_gravity_model_pars(param_quantiles)
p1 <- p1 +
    mriids_plot_theme$theme +
    mriids_plot_theme$legend +
    scale_color_manual(values = mriids_plot_theme$color_scale) +
    scale_fill_manual(values = mriids_plot_theme$color_scale)


ggsave(here::here("who_gamma_pstay_over_tproj_14.png"),
       p1)


