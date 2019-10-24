library(dplyr)
library(ggplot2)
source(here::here("analysis/parameters.R"))
source(here::here("analysis/common_plot_properties.R"))
########### Visualisation Tasks.
## Projections for each country for non-overlapping periods.
## give weekly incid for visualisation
## Depends on: assign_epidemic_phase
## Outfiles:

rmarkdown::render(
    "analysis/who_hm_promed_comparison.Rmd",
    params = list(
        ofinterest = c("GIN", "LBR", "SLE"),
        pmdaily = all_files[["ProMED"]]$incidtall,
        hmdaily = all_files[["HealthMap"]]$incidtall,
        whodaily = all_files[["WHO"]]$incidtall
    )
)

pars <- expand.grid(
    tw = c(14, 28, 42),
    ndates = c(c(28, 42, 56)),
    ds = c("WHO", "ProMED", "HealthMap"),
    stringsAsFactors = FALSE
)

purrr::pwalk(
   pars,
   function(tw, ndates, ds) {
       weekly_incidfile <- glue::glue(
           all_files[[ds]]$outdir,
           "/{Sys.Date()}_{ds}_processed_weekly_incidence.csv"
       )
       message("Weekly file is ", weekly_incidfile)
       rmarkdown::render(
           "analysis/forecasts_viz_fixed_country.Rmd",
           params = list(
               twindow = tw,
               incid = weekly_incidfile,
               n.dates.sim = ndates,
               places = all_files[[ds]]$places,
               outdir =
                   all_files[[ds]]$outdir,
               datasource = ds
           ),
           output_dir = glue::glue("{all_files[[ds]]$outdir}/{Sys.Date()}")
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
           output_dir = glue::glue("{all_files[[ds]]$outdir}/{Sys.Date()}"),
           params = list(
               twindow = tw,
               n.dates.sim = ndates,
               places = all_files[[ds]]$places,
               indir  =
                   all_files[[ds]]$outdir,
               outdir = all_files[[ds]]$outdir,
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
           "analysis/forecasts_assess_by_datasource.Rmd",
           output_file = glue::glue(
               "{tw}_{ndates}"
           ),
           params = list(
               twindow = tw,
               n.dates.sim = ndates
           ),
           output_dir = glue::glue(
               "ms-figures/si-figures/other/{Sys.Date()}"
           )
       )
   }
 )







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
