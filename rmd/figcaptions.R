library(captioner)
fig_nums <- captioner(prefix = "Figure")


incidrcap <- fig_nums(
    name = "incidrcompare",
    caption = "Comparison of national daily incidence trends and R
estimates from ProMED, HealthMap and WHO data for Guinea, Liberia and Sierra Leone.
(A) Daily incidence derived from ProMED (blue), HealthMap (green) and WHO data (orange).
Daily incidence that was not directly available from ProMED and HealthMap data and
which were therefore imputed (see Methods) are shown in lighter shade of blue and green respectively.
WHO data were aggregated to country level. The y-axis differs for each plot. (B)
The mean time-varying reproduction number
$R_t$ estimated using the WHO data (orange), ProMED (blue) and HealthMap (green) data.
The shaded regions depicts the 95% credible intervals (95% CrI) for the $R_t$ estimates.
The reproduction number was estimated on sliding windows of 28 days, using the  EpiEstim R
package with a time window of 28 days. Estimates shown at time $t$ are for
the 28-day window finishing on day t.")


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
used to make projections over each 4 week forecast horizon.
The bottom figure shows the effective reproduction number estimated
retrospectively using the full dataset up to the end.
In each case, the solid gray line is the median
estimate and the shaded region represents the 95% Credible Interval.
The red horizontal dashed line indicates the $R_t = 1$ threshold.
Results are shown for the three mainly affected countries although the
analysis was done using data for all countries on African mainland.")


all_ds_14_28_cite <- fig_nums(
    name = "all_ds_14_28",
    display = "cite"
)

all_ds_14_28 <- fig_nums(
    name = "all_ds_14_28",
    caption  = "Forcasts using ProMED, HealthMap and WHO data.
Non-overlapping weekly forecasts on log-scale over a 4 week horizon using ProMED (blue), HealthMap (green) and
WHO (orange) data with a 2 week window used for inference.
The black solid dots are the observed incidence derived from WHO data. The shaded regions represent the
95% forecast interval.")

assesscite <- fig_nums(
    name = "assess",
    display = "cite"
)
assesscap <- fig_nums(
    name = "assess",
    caption = "Model performance metrics stratified by epidemic phase.
The performance metrics (anti-clockwise from top left) are
bias, sharpness, relative mean absolute error (show on log scale) and
the percentage of weeks for which the 95% forecast interval contained
the observed incidence. In forecasting ahead, we assumed transmissibility
to be constant over the forecast horizon. If the 97.5th percentile of the R estimate used
for forecasting was less than 1, we defined the epidemic to be in the declining phase
during this period. Similarly, if the 2.5th percentile of R was
greater than 1, we defined the epidemic to be in a growing phase. The phase was
set to neither where the 95% Credible Interval of the R estimates contained 1.")

imp_risk_cite <- fig_nums(
    "pm_imp_risk_28",
    display = "cite"
)

imp_risk_cap <- fig_nums(
    "pm_imp_risk_28",
    caption = "Importation risk."
)


parscite <- fig_nums(
    name = "pstayandgamma",
    display = "cite")

parscap <- fig_nums(
    name = "pstayandgamma",
    caption = "Estimates of mobility model parameters
during the epidemic. Population movement was modelled using a gravity model
where the flow between a location $i$ and $j$ is proportional to the product of their
populations and inversely population to the distance between them raised to an exponent
$gamma$. The parameter gamma thus modulates the influence of distance between the population flow.
$p_{stay}$ represents the probability of an individual to stay in a given location during their
infectious period. The solid lines represents the median estimates obtained using WHO (yellow), ProMED (blue)
and HealthMap (green) data. The shaded regions represent the 95% CrI.")


alertscite <- fig_nums(
    name = "roc_alerts",
    display = "cite"
)

alertscap <- fig_nums(
    name = "roc_alerts",
    caption = "Predicted weekly presence of cases in each country.
The panel on the left shows the True and False alert rates using different
thresholds for classification. For a given threshold ($x^{th}$ percentile of the
forecast interval), we defined a True alert for a week where the $x^{th}$ percentile of the
the forecast interval and the observed incidence for a
country were both greater than 0; false alert for a week where the threshold
for a country was greater than 0 but the observed incidence
for that country was 0; and missed alert: week where the threshold for a country was 0 but the
observed incidence for that country was greater than 0. True alert rate is the fraction of correctly
classified true alerts (i.e., (true alerts)/(true alerts + missed alerts).
False alert rate is similarly the fraction of correctly classified false alerts.
The right panel shows
the True (green), False (yellow) and Missed (red) alerts using the 97.5th percentile of
the forecast interval as threshold. The figure only shows countries on the African continent for which either the
predicted incidence or the observed incidence was greater than 0 at least once.
For each country, four rows are shown,
corresponding to the 1, 2, 3 and 4 week ahead predictions respectively.
Country codes, shown on the y-axis, are as follows: BEN - Benin, BFA -
Burkina Faso, CIV - Côte d'Ivoire, CMR - Cameroon, COD - Democratic
Republic of Congo, GIN - Guinea, GNB - Guinea-Bissau, LBR - Liberia,
MLI - Mali, NER - Niger, NGA - Nigeria, SEN - Senegal, SLE - Sierra
Leone, TGO - Togo. Predictions used here are using the ProMED
data, a 2-week window for inference and a 4 week forecast horizon.")
