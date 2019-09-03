library(captioner)

si_fig_nums <- captioner(prefix = "SI Figure")
section_nums <- captioner(prefix = "SI Section")
si_tab_nums <- captioner(prefix = "SI Table")
#####################
## SI sections
#####################
dataclean_section <- section_nums(
    name = "datacleaning",
    display = "cite"
)

pm_section <- section_nums(
    name = "promed_section",
    display = "cite"
)
hm_section <- section_nums(
    name = "healthmap_section",
    display = "cite"
)

who_section <- section_nums(
    name = "who_section",
    display = "cite"
)

tw_perf_section <- section_nums(
    name = "tw_perf",
    display = "cite"
)

ds_perf_section <- section_nums(
    name = "ds_perf",
    display = "cite"
)

gamma_ul_section <- section_nums(
    name = "gamma_ul_section",
    display = "cite"
)

#####################
## SI tables
#####################
corr_coeffs <- si_tab_nums(
    name = "corr_coeffs",
    caption = "Correlation coefficients for R estimates. The correlation
coefficients for the estimates of the time-varying reproduction number
estimated using WHO, ProMED and HealthMap data for the three mainly affected
countries. For the purposes of comparison, the reproduction number was
estimated using R package EpiEstim [@cori2013new] on sliding windows of 28 days.")

#####################
## SI figures
#####################
pm_data_clean <- si_fig_nums(
    name = "pm_data_clean",
    caption = "Illustration of workflow for Processing ProMED feed.
Raw ProMED feed consisted of suspected and confirmed cases and suspected and confirmed
deaths. The top left figure (a) shows the suspected and confirmed cases in the raw feed.
We used the suspected and confirmed cases to derive cumulative
incidence data (b). Dupliacte alerts on a day were then removed (c).
If there were outliers in the data, we removed them in the next step (d).
We then made the cumulative case count monotonically increasing (e) by
removing inconsistent records. Finally, missing data was imputed using
log-linear interpolation (f). Interpolated points are show in light blue.
See Methods for details.")

## ProMED

pm_14_42_cap <- si_fig_nums(
    name = "pm_14_42",
    caption = "Observed and predicted
incidence, and reproduction number estimates from ProMED data.
The top panel shows the weekly incidence derived from ProMED
data and 6 weeks incidence forecast on log scale.
The solid dots represent the observed weekly incidence where
the light blue dots show
weeks for which at least one data point was obtained using
interpolation. The projections are made over 6 week windows, based on the
reproduction number estimated in the previous 2 weeks.
The middle figure in each panel shows the reproduction number
used to make projections over each 4 week forecast horizon.
The bottom figure shows the effective reproduction number estimated
retrospectively using the full dataset up to the end.
In each case, the solid gray line is the median
estimate and the shaded region represents the 95% Credible Interval.
The red horizontal dashed line indicates the $R_t = 1$ threshold.
Results are shown for the three mainly affected countries although the
analysis was done using data for all countries on African mainland.
All figures in Sections 2, 3 and 4 of the Supplementary material follow
the same layout and color convention. "
)

proj_fig_caption <- function(ds, tw, fw, name) {
    caption <- glue::glue(
        "Observed and predicted incidence, and reproduction number estimates from {ds} data.",
        "The calibration window is {tw} weeks and the forecast horizon is {fw} weeks."
        )

    out <- si_fig_nums(name = name, caption = caption)
    out
}

proj_fig_cite <- function(ds, tw, fw, name) {
    out <- si_fig_nums(name = name, display = "cite")
    out
}

pars <- expand.grid(
    fw = c(4, 6, 8),
    tw = c(2, 4, 6),
    ds = c("ProMED", "HealthMap", "WHO"),
    stringsAsFactors = FALSE
)
pars <- pars[-c(1, 2), ]

proj_figcaps <- purrr::pmap(pars, function(fw, tw, ds) {
    name <- glue::glue("{ds}_{tw}_{fw}")
    proj_fig_caption(ds, tw, fw, name = name)
  }
)

proj_figcite <- purrr::pmap(pars, function(fw, tw, ds) {
    name <- glue::glue("{ds}_{tw}_{fw}")
    proj_fig_cite(ds, tw, fw, name = name)
  }
)

pm_model_perf_all_windows <- si_fig_nums(
    name = "pm_model_perf_all_windows",
    caption = "Model performance metrics stratified by the time window used for
model calibration. The performance metrics (anti-clockwise from top left) are
bias, sharpness, relative mean absolute error (show on log scale) and
the percentage of weeks for which the 95% forecast interval contained
the observed incidence.")



model_perf_all_ds <- si_fig_nums(
    name = "model_perf_all_ds",
    caption = "Model performance metrics stratified by datasource
ProMED (blue), HealthMap (green), and WHO (yellow).
The performance metrics (anti-clockwise from top left) are
bias, sharpness, relative mean absolute error (show on log scale) and
the percentage of weeks for which the 95% forecast interval contained
the observed incidence.")




pm10_params <- si_fig_nums(
    name = "pm10_params",
    caption = "Estimates of mobility model parameters
during the epidemic. Population movement was modelled using a gravity model
where the flow between locations $i$ and $j$ is proportional to the product of their
populations and inversely population to the distance between them raised to an exponent
$gamma$. The parameter gamma thus modulates the influence of distance on the population flow.
Here $\\gamma$ is allowed to vary between 1 and 10. $p_{stay}$ represents the probability
of an individual to stay in a given location during their
infectious period. The solid lines represents the median estimates
obtained using ProMED data. The shaded regions represent the 95% CrI."
)

pars <- expand.grid(
    fw = c(4, 6, 8),
    tw = c(2, 4, 6),
    ds = "ProMED",
    stringsAsFactors = FALSE
)

pm_figcaps <- purrr::pmap(pars, function(fw, tw, ds) {
    name <- glue::glue("{ds}_{tw}_{fw}_10")
    proj_fig_caption(ds, tw, fw, name = name)
  }
)

model_perf_gamma <- si_fig_nums(
    name = "model_perf_gamma",
    caption = "Model performance metrics allowing $\\gamma$ to vary from
1 to 2 or 10. The performance metrics (anti-clockwise from top left) are
bias, sharpness, relative mean absolute error (show on log scale) and
the percentage of weeks for which the 95% forecast interval contained
the observed incidence.")
