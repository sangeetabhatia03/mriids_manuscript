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
  rmarkdown::render("analysis/extract_mcmc_draws.Rmd",
  		     params = list(tproj = tproj,
        	     	           twindow = twindow,
                                   incid = "healthmap_wide_from_july.csv"))

  rmarkdown::render("analysis/projection_using_fitted.Rmd",
                    params = list(tproj = tproj,
                                  twindow = twindow,
                                  incid = "healthmap_wide_from_july.csv"))

})

## For a given time window, at every pair of points at which
## we have done forecasts
twindow = seq(from = 7, to = 49, by = 7)
countries <- c("Guinea", "Liberia", "Sierra Leone")
for (tw in twindow) {
    t_proj = seq(from = 2 * tw,
                 to = 567,
                 by = tw)
    pairs <- combn(t_proj, 2)
    apply(pairs, 2, function(col) {
        rmarkdown::render("analysis/forecasts_visual_check.Rmd",
                          params = list(tproj = col,
                                        twindow = tw,
                                        place = "Guinea"))
        })


}



