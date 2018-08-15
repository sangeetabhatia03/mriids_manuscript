twindow = seq(from = 7, to = 49, by = 7)
pars <- data.frame(t_proj = c(), time_window = c())
for (tw in twindow) {
    t_proj = seq(from = 2 * tw,
                 to = 567,
                 by = tw)
    pars <- rbind(pars, expand.grid(t_proj = t_proj,
                                    time_window = tw))
}

apply(pars, 1, function(row) {
  message("Working on row ", row)

  tproj <- row[1]
  twindow <- row[2]
  ## rmarkdown::render("analysis/extract_mcmc_draws.Rmd",
  ## 		     params = list(tproj = tproj,
  ##       	     	           twindow = twindow,
  ##                                  incid = "healthmap_wide_from_july.csv"))

  rmarkdown::render("analysis/projection_using_fitted.Rmd",
                    params = list(tproj = tproj,
                                  twindow = twindow,
                                  incid = "healthmap_wide_from_july.csv"))

})

## Now get the assessment metrics
places <- c("Liberia", "Guinea", "Sierra Leone")
twindow = seq(from = 7, to = 49, by = 7)
pars <- data.frame(t_proj = c(), time_window = c(), place = c())
for (tw in twindow) {
    t_proj = seq(from = 2 * tw,
                 to = 567,
                 by = tw)
    pars <- rbind(pars, expand.grid(t_proj = t_proj,
                                    time_window = tw,
                                    place = places))
}

for (i in 1:nrow(pars)) {
  row <- pars[i, ]
  message("Working on row ", row)

  tproj <- row$t_proj
  twindow <- row$time_window
  place <- as.character(row$place)
  rmarkdown::render("analysis/forecasts_assess.Rmd",
  		     params = list(tproj = tproj,
        	     	           twindow = twindow,
                                   n.dates.sim = 28,
                                   incid = "healthmap_wide_from_july.csv",
                                   place = place))
}


twindow = seq(from = 7, to = 49, by = 7)
for (tw in twindow) {
    tproj <- seq(from = 2 * tw,
                 to = 560,
                 by = tw)

    rmarkdown::render("analysis/forecasts_assess_viz.Rmd",
                      params = list(tproj = tproj,
                                    twindow = tw,
                                    n.dates.sim = 28,
                                    incid = "healthmap_wide_from_july.csv",
                                    place = "Sierra Leone"))
}
