library(dplyr)
library(ggplot2)
source(here::here("analysis/parameters.R"))
source(here::here("analysis/common_plot_properties.R"))
########### Visualisation Tasks.
## Projections for each country for non-overlapping periods.
## give weekly incid for visualisation
## Depends on: assign_epidemic_phase
## Outfiles:

pars <- expand.grid(
    tw = c(14, 28, 42),
    ndates = c(c(28, 42, 56)),
    ds = c("WHO", "ProMED", "HealthMap"),
    stringsAsFactors = FALSE
)

purrr::pwalk(
   pars,
   function(tw, ndates, ds) {
       rmarkdown::render(
           "analysis/forecasts_viz_fixed_country.Rmd",
           params = list(
               twindow = tw,
               incid = all_files[[ds]]$weekly_incidfile,
               n.dates.sim = ndates,
               places = all_files[[ds]]$places,
               outdir =
                   all_files[[ds]]$outdir,
               datasource = ds
           )
         )
   }
)


purrr::pwalk(
   pars,
   function(tw, ndates, ds) {
       message("Working on ", tw, " : ", ndates, " : ", ds)
       rmarkdown::render(
           "analysis/forecasts_assess_viz.Rmd",
           output_file = paste(
               ds,
               tw,
               ndates,
               sep = "_"
           ),
           output_dir = all_files[[ds]]$outdir,
           params = list(
               twindow = tw,
               n.dates.sim = ndates,
               places = all_files[[ds]]$places,
               indir  =
                   all_files[[ds]]$outdir,
               outdir =
                   paste0(all_files[[ds]]$outdir, "/"),
               datasource = ds
           )
       )
   }
)

source(
    here::here(
      "analysis/roc.R"
    )
  )


## Depends on combine_forecasts_incidence
for (idx in seq_len(nrow(pars))) {
  datasource <- pars$ds[idx]
  twindow <- pars$tw[idx]
  n.dates.sim <- pars$ndates[idx]
  message("Working on ", datasource, " : ", twindow, " : ", n.dates.sim)
  source(
    here::here(
      "analysis/plot_sensitivity_specificity_alerts.R"
    )
  )
}



## The data sources we want to combine.
datasources <- c("ProMED", "WHO", "HealthMap")
names(datasources) <- datasources
## Depends on gravity_model_pars_quantiles
## Infile gravity_model_parameters_quantiles.csv

source(
    here::here(
       "analysis/plot_gravity_model_pars_quantiles.R"
    )
)

datasources <- c("ProMED", "WHO", "HealthMap")
names(datasources) <- datasources
for (idx in seq_len(nrow(pars))) {
  tw <- pars$tw[idx]
  ndates <- pars$ndates[idx]
  source(
      here::here(
          "analysis/projections_viz_all_datasources.R"
      )
    )
}


datasources <- c("ProMED", "WHO", "HealthMap")
ndates <- c(28, 42, 56)
pars <- expand.grid(
    ds = datasources,
    ndate = ndates,
    stringsAsFactors = FALSE
)

purrr::pwalk(
    pars,
    function(ds, ndate) {
        rmarkdown::render(
             input = "analysis/forecasts_assess_by_time_window.Rmd",
             params = list(
                 n.dates.sim = ndate,
                 indir = all_files[[ds]]$outdir,
                 outdir = glue::glue(
                     "{all_files[[ds]]$outdir}/"
                 ),
                 datasource = ds
             )
          )
     }
)
