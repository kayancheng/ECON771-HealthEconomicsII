#' Function to replicate figure 3 of Erison (2014) with different nbin
#' Ka Yan CHENG
#' 

erison_fig3 <- function(nb){
  
  rd.result_quartic <- rdplot(fig3_data$lnS, fig3_data$LISPremium, 
                              c = 0,
                              p = 4,
                              nbin = nb,
                              h = 10,
                              title = "",
                              x.label="Monthly Premium - LIS Subsidy, 2006", 
                              y.label="Log Enrollment Share, 2006")
  
  rd.result_linear <- rdplot(fig3_data$lnS, fig3_data$LISPremium, 
                             c = 0,
                             p = 1,
                             nbin = nb,
                             h = 4,
                             title = "",
                             x.label="Monthly Premium - LIS Subsidy, 2006", 
                             y.label="Log Enrollment Share, 2006")
  
  bin.avg_linear <- as_tibble(rd.result_linear$vars_bins)
  poly_linear <- as_tibble(rd.result_linear$vars_poly)
  
  bin.avg_quartic <- as_tibble(rd.result_quartic$vars_bins)
  poly_quartic <- as_tibble(rd.result_quartic$vars_poly)
  
  plot.q2 <- bin.avg_linear %>% ggplot(aes(x=rdplot_mean_x,y=rdplot_mean_y)) + 
    geom_point() + theme_bw() +
    poly_linear %>% geom_line(mapping = aes(x=rdplot_x,y=rdplot_y), size = 0.8, linetype = "dashed", colour = "gray69") + 
    poly_quartic %>% geom_line(mapping = aes(x=rdplot_x,y=rdplot_y), size = 0.5) +
    xlab("Monthly Premium - LIS Subsidy, 2006") + ylab("Log Enrollment Share, 2006")
  
}