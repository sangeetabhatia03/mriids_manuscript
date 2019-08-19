library(captioner)
fig_nums <- captioner(prefix = "Figure")
tab_nums <- captioner(prefix = "Table")
si_fig_nums <- captioner(prefix = "SI Figure")
section_nums <- captioner(prefix = "SI Section")

incidrcite <- fig_nums(
    name = "incidrcompare",
    display = "cite"
)
incidrcap <- fig_nums(
    name = "incidrcompare",
    caption = "Comparison of daily incidence
trends and R estimates from ProMED, HealthMap and WHO data (A) Daily
incidence series derived from ProMED (blue), HealthMap (green) and
WHO data (orange). The interpolated points for ProMED and HealthMap
are shown in lighter shade of blue and black respectively. WHO data
were aggergated to country level. (B) The mean
time-varying reproduction number estimated using the WHO (yellow), ProMED
(blue) and HealthMap (green) data. The shaded region depicts the 95%
credible interval (95% CrI) for the R estimates.
The effective reprodcution number was estimated using EpiEstim [@cori2013new]
with a sliding time window of 28 days. Estimates shown at time $t$ are
for the 28-day window finishing on day $t$. The y-axis differs for
each plot in panel A.")


pm28cite <- fig_nums(
    name = "promed28",
    display = "cite"
)

pm28cap <- fig_nums(
    name = "promed28",
    caption = "Observed and predicted
incidence, and reproduction number estimates from ProMED data.
The top panel shows the weekly incidence derived from ProMED
data and the 4 weeks incidence forecast on log scale.
The solid dots represent the observed weekly incidence where
the light blue dots show
weeks for which at least one data point was obtained using
interpolation. The projections are made over 4 week windows, based on the
reproduction number estimated in the previous 2 weeks.
The middle figure in each panel shows the reproduction number
used to make projections over each 4 week projection window.
The bottom figure shows the effective reproduction number estimated
retrospectively using the full dataset up to the end.
In each case, the solid gray line is the median
estimate and the shaded region represents the 95% CrI. The red
horizontal dashed line indicates the $R_t = 1$ threshold.")


all_ds_14_28 <- fig_nums(
    name = "all_ds_14_28",
    display = "cite"
)

all_ds_14_28 <- fig_nums(
    name = "all_ds_14_28",
    caption  = "Non-overlapping 4 weeks forecasts with a 2 week window
for calibration. Model performance is impacted by the data source used."
)

assesscite <- fig_nums(
    name = "assess",
    display = "cite"
)
assesscap <- fig_nums(
    name = "assess",
    caption = "Metrics for evaluating model
performance where the projection horizon was 4 weeks.
The y-axis shows (anti-clockwise from top left)
bias, log relative accuracy, relative mean
absolute error and sharpness scores respectively. In the forward
projections, we assumed transmissibility to be constant over the
projection window. be If the 97.5th percentile of the R estimate used
was less than 1, we defined the epidemic to be in the declining phase
during this period. Similarly, if the 2.5th percentile of R was
greater than 1, we defined the epidemic to be in a growing phase.")

imp_risk_cite <- fig_nums(
    "pm_imp_risk_28",
    display = "cite"
)

imp_risk_cap <- fig_nums(
    "pm_imp_risk_28",
    caption = "Importation risk."
)



#####################
## SI sections
#####################

hm_section <- section_nums(
    name = "healthmap_section",
    display = "cite"
)

who_section <- section_nums(
    name = "who_section",
    display = "cite"
)

gamma_ul_section <- section_nums(
    name = "gamma_ul_section",
    display = "cite"
)
#####################
## SI figures
#####################
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




## Need to define stuff before we can use it.

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

who28cite <- fig_nums(
    name = "who28",
    display = "cite"
)

who28cap <- fig_nums(
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

parscite <- fig_nums(
    name = "pstayandgamma",
    display = "cite")

parscap <- fig_nums(
    name = "pstayandgamma",
    caption = "Estimates of mobility model parameters
during the epidemic.
The solid yellow line represents the median obtained using WHO
data and the blue line represents the median using ProMED data.
The shaded regions represent the 95% CrI.")


proptabcite <- tab_nums(name = "propinci", display = "cite")

proptabcap <- tab_nums(
   name = "propinci",
   caption = "Proportion of observed incidence in
95% CrI of the projected incidence.")



alertscite <- fig_nums(
    name = "roc_alerts",
    display = "cite"
)

alertscap <- fig_nums(
    name = "roc_alerts",
    caption = "Predicted weekly presence of cases in each country.
Green, yellow and red dots show respectively the true, false, and
missed alerts, defined as follows. True alert: week where the 97.5%
percentile of the predicted incidence and the observed incidence for a
country were both > 0. False alert: week where the 97.5% percentile of
the predicted incidence for a country was >0 but the observed incidence
for that country was zero. Missed alert: week where the 97.5%
percentile of the predicted incidence for a country was zero but the
observed incidence for that country was > 0.
Weeks are shown on the x-axis and countries on the y-axis. The figure
only shows countries on the African continent for which either the
predicted incidence or the observed incidence was >0 at least once.
Country codes, shown on the y-axis, are as follows: BEN - Benin, BFA -
Burkina Faso, CIV - Côte d'Ivoire, CMR - Cameroon, COD - Democratic
Republic of Congo, GIN - Guinea, GNB - Guinea-Bissau, LBR - Liberia,
MLI - Mali, NER - Niger, NGA - Nigeria, SEN - Senegal, SLE - Sierra
Leone, TGO - Togo. For each country, four rows are shown,
corresponding to the 1, 2, 3 and 4 week ahead predictions respectively;
these are shown on the
right-hand side y axis. Predictions used here are using the ProMED
data, a 2-week window for inference and a 4 week forecast horizon."
)
