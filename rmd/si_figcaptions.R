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
pm_28_28_cite <- si_fig_nums(
    name = "pm_28_28",
    display = "cite"
)

pm_28_28_cap <- si_fig_nums(
    name = "pm_28_28",
    caption = "Non-overlapping forecasts using ProMED data with 4 weeks
  time window for calibration and 4 weeks forecast horizon."
)

pm_42_28_cite <- si_fig_nums(
    name = "pm_42_28",
    display = "cite"
)

pm_42_28_cap <- si_fig_nums(
    name = "pm_42_28",
    caption = "Non-overlapping forecasts using ProMED data with 6 weeks
  time window for calibration and 4 weeks forecast horizon."
)



pm_model_perf_all_windows <- si_fig_nums(
    name = "pm_model_perf_all_windows",
    display = "cite"
)

pm_model_perf_all_windows_cap <- si_fig_nums(
    name = "pm_model_perf_all_windows",
    caption = "Model performance metrics for models using different
widows for calibration."
)

model_perf_all_ds <- si_fig_nums(
    name = "model_perf_all_ds",
    display = "cite"
)

model_perf_all_windows_cap <- si_fig_nums(
    name = "model_perf_all_ds",
    caption = "Model performance metrics for models using different
data sources."
)

## WHO
## ds - datasource, tw - time window, fw - forecast window
proj_fig_caption <- function(ds, tw, fw) {
    caption <- glue::glue(
        "Non-overlapping projections using {ds} data with {tw} weeks window for inference and {fw} forecast horizon."
        )
    name <- glue::glue("{ds}_{tw}_{fw}")
    out <- si_fig_nums(name = name, caption = caption)
    out
}




rcorrcite <- si_fig_nums(
    name = "rcorr",
    display = "cite"
)
rcorrcap <- si_fig_nums(
    name = "rcorr",
    caption = "R estimates from ProMED,
HealthMap and WHO data when using different time windows. (Top) Time
 window of 14 days. (Bottom) Time window of 42 days.
The orange line depicts the identity function.")

who28cite <- si_fig_nums(
    name = "who28",
    display = "cite"
)

who28cap <- si_fig_nums(
    name = "who28",
    caption = "Observed and predicted
incidence, and reproduction number estimates from the WHO data.
The top panel shows the weekly incidence derived from the WHO
data and the 4 weeks incidence forecast on log scale.
The solid dots represent the observed weekly incidence.
The projections are made over 4 week windows, based on the
reproduction number estimated in the previous 2 weeks.
The middle figure in each panel shows the reproduction number
used to
make projections over each 4 week projection window.
The bottom figure shows the effective reproduction number estimated
retrospectively using the full dataset up to the end.
In each figure, the solid gray line is the median
estimate and the shaded region represents the 95% CrI.")




rcorr <- si_tab_nums(
    name = "rcorr",
    caption = "Pearson's correlation coefficients for
correlation between effective time-varying reproduction number
estimates from ProMED, HealthMap and WHO daily incidence data.
The reproduction number was estimated on sliding windows of 28 days, using the  EpiEstim R
package with a time window of 28 days. Estimates shown at time $t$ are for
the 28-day window finishing on day $t$. All coefficients were statistically significant."
)
