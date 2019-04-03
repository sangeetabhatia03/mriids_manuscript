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
p1 <- mriids_plot_theme(p1)

ggsave(here::here("data/output/figures/who_gamma_pstay_over_tproj_14.png"),
       p1)

## Maps.
## Second throwaway function.



## three points separated by approximately 6 months
promed_tprojs <- c(##`35` = 35,
                  `196` = 196,
                  `231` = 231)
## WHO data starts 86 days before promed data
## The closest matching dates for WH
who_tprojs <- promed_tprojs + 84
names(who_tprojs) <- c("280", "315")

twindow <- 28


read_forecasts <- function(tproj, twindow = 28, indir) {
      df <- readr::read_csv(here::here(indir,
                                       paste0("forecasts_",
                                              tproj,
                                              "_",
                                              twindow,
                                              "_28.csv")))

      df

}

forecasts <-
purrr::map2_dfr(list(WHO = who_tprojs,
                     ProMED = promed_tprojs),
                list(`WHO Predicted`= "data/who_output",
                     `ProMED Predicted` = "data/output"),
               function(tprojs, indir) {
                   df <- purrr::map_dfr(tprojs,
                                        ~ read_forecasts(.x, 28, indir),
                                        .id = "tproj")
                   df
               }
             , .id = "source")



forecasts <- group_by(forecasts,
                      tproj,
                      country,
                      source) %>%
    filter(date == min(date))


forecasts <- select(ungroup(forecasts),
                    date,
                    source,
                    country,
                    incid = y)


incid <- readr::read_csv("data/processed/20122018_promed_loglinear_weekly.csv")
incid <- select(incid, date, country, incid)
incid <- tidyr::spread(incid, key = country, value = incid, fill = 0)
incid <- tidyr::gather(incid, country, incid, -date)
incid <- filter(incid,
                date %in% unique(forecasts$date))


incid$source <- "ProMED (Observed)"
incid <- select(incid,
                date,
                source,
                country,
                incid)

forecasts_observed <- rbind(forecasts,
                            incid)

fname <- here::here("data/Africa_SHP")
africa <- sf::st_read(fname)
africa$ISO3c <- countrycode::countrycode(africa$COUNTRY,
                                         destination = "iso3c",
                                         origin = "country.name")


joined <- left_join(africa,
                    forecasts_observed,
                    by = c("ISO3c" = "country"))

## NAs for Western Sahara.
joined <- na.omit(joined)

## set 0s to NA so that we can color it differently
joined <- mutate_at(joined,
                    vars(incid),
                    funs(ifelse(incid > 0,
                                .,
                                NA)))


## Labels as week of year

week_labeller <- function(var) {
    label <- paste(lubridate::year(var),
                   ": Week ",
                   lubridate::week(var))
    label
}
joined$date <- lubridate::ymd(joined$date)
joined$week_of_year <- week_labeller(joined$date)
joined$week_of_year <- factor(joined$week_of_year)



## Fix the order of facets
joined$source <- factor(joined$source,
                        levels = c("ProMED (Observed)",
                                   "ProMED",
                                   "WHO"))


## Set-up
joined <- filter(joined,
                  source
                  %in%
                  c("ProMED (Observed)" , "ProMED"))

joined$source <- forcats::fct_recode(joined$source,
                                      c(`ProMED (Predicted)` = "ProMED"))


joined$source <- factor(joined$source,
                         levels = c("ProMED (Observed)",
                                    "ProMED (Predicted)"),
                         ordered = TRUE)

##
library(viridis)
p <- ggplot()
p <- p + scale_fill_viridis_c(breaks = c(1, 300, 600),
                              alpha = 0.8,
                              na.value = "#fefdf6")
p <- p + xlab("") + ylab("")
p <- p + theme(
             panel.ontop = FALSE,
             panel.grid = element_blank(),
             line = element_blank(),
             rect = element_blank())


p <- p + geom_sf(data = joined,
                 aes(fill = incid),
                 lwd = 0.1)

p <- p + coord_sf(datum = NA)
p <- p + facet_grid(week_of_year ~ source)
p <- p + theme(legend.title = element_blank(),
               legend.position = "bottom")

ggsave("whole_africa_MALI_nonzero.png", p)


## Restrict to few countries
countries <- c("Sierra Leone",
               "Guinea",
               "Liberia",
               "Cote d`Ivoire",
               "Mali",
               "Senegal",
               "Gambia",
               "Guinea-Bissau",
               "Burkina Faso",
               "Ghana",
               "Mauritania")


wafrica <- filter(joined,
                  COUNTRY %in% countries)


p <- ggplot()
p <- p + scale_fill_viridis_c(breaks = c(1, 3000, 4700),
                              alpha = 0.8,
                              na.value = "#fcf9d3")
p <- p + xlab("") + ylab("")
p <- p + theme(
             panel.ontop = FALSE,
             panel.grid = element_blank(),
             line = element_blank(),
             rect = element_blank())


p <- p + geom_sf(data = wafrica,
                 aes(fill = incid),
                 lwd = 0.1)

p <- p + coord_sf(datum = NA)
p <- p + facet_grid(week_of_year ~ source)
p <- p + theme(legend.title = element_blank(),
               legend.position = "bottom")

ggsave("wafrica.png", p)


