
# sample from the 3 points distributions 
# c5model: 1=50%, 2=5%, 3=95%
# vals <- c(q05,q50,q95)
sample_lvl <- function(vals, n = 5000) {
  vals <- sort(vals)
  y <- runif(n)
  x <- y - 0.5
  x <- ifelse(x < 0,
              x * (vals[2]-vals[1]) / (0.5 - 0.05),
              x * (vals[2]-vals[3]) / (0.5 - 0.95))
  x <- x + vals[2]
}


plot_slr <- function(outfile) {
  
  loadd(impact_slr)
  
  impact_slr_dist <- melt(impact_slr, 
                          id.vars = c("model","cbudget","poltype","cluster","year"),
                          measure.vars = c("slr_p50","slr_p05","slr_p95"))
    
    
  impact_slr_samp <- impact_slr_dist[year >= 1980 & year %in% seq(1980,2200,by=10), 
                                     .(slr = quantile(sample_lvl(value))), 
                                     by = c("model","cbudget","poltype","cluster","poltype","year")]
  
  slr_stat <- impact_slr_samp[, .(slr_p05 = quantile(slr,0.05),
                                  slr_p95 = quantile(slr,0.95)) , 
                              by = c("cluster","poltype","year")]
  
  slr_stat2 <- impact_slr[year >= 1980 & year %in% seq(1980,2200,by=10),
                          .(slr_p50 = mean(slr_p50)),
                          by = c("cluster","poltype","year")]
  
  slr_stat <- merge(slr_stat,slr_stat2,by = c("cluster","poltype","year"))
  
  slr_stat[, cluster := factor(cluster, levels = clusters)]
  slr_stat[poltype == "full", poltype := "End of Century"]
  slr_stat[poltype == "peak", poltype := "Net Zero"]
  
  p <- ggplot(slr_stat[year >= 1980]) +
    geom_line(aes(x = year, y = slr_p50 * 100,
                  linetype = "50%",
                  color = poltype),size = 1) +
    geom_line(aes(x = year, y = slr_p95 * 100,
                  linetype = "95%",
                  color = poltype),size = 1) +
    geom_line(aes(x = year, y = slr_p05 * 100,
                  linetype = "5%",
                  color = poltype),size = 1) +
    scale_linetype_manual(values = c(2,1,3), name = "Temperature\npercentile") +
    scale_color_npg(name = "Policy design") +
    facet_wrap(~ cluster, nrow = 1) +
    theme_bw() +
    labs(x = "", y = "Global mean sea level rise [cm]")

  ggsave(outfile,plot = p, width = 10, height = 5)
}


plot_slr_overshoot <- function(outfile) {
  
  loadd(impact_slr)
  
  
  impact_slr_dist <- melt(impact_slr, 
                          id.vars = c("model","cbudget","poltype","cluster","year"),
                          measure.vars = c("slr_p50","slr_p05","slr_p95"))
  
  
  impact_slr_samp <- impact_slr_dist[year >= 1980 & year %in% seq(1980,2200,by=10), 
                                     .(slr = quantile(sample_lvl(value))), 
                                     by = c("model","cbudget","poltype","cluster","poltype","year")]
  
  slr_stat <- impact_slr_samp[, .(slr_p05 = quantile(slr,0.05),
                                  slr_p95 = quantile(slr,0.95)) , 
                              by = c("cluster","poltype","year")]

  slr_stat2 <- impact_slr[year >= 1980 & year %in% seq(1980,2200,by=10),
             .(slr_p50 = mean(slr_p50)),
             by = c("cluster","poltype","year")]
  
  slr_stat <- merge(slr_stat,slr_stat2,by = c("cluster","poltype","year"))
    
  slr_diff <- dcast(slr_stat, cluster + year  ~ poltype,
                    value.var = c("slr_p05","slr_p50","slr_p95"),
                    fun.aggregate = mean)
  
  slr_diff[, slr_p05_diff := slr_p05_full - slr_p05_peak]
  slr_diff[, slr_p50_diff := slr_p50_full - slr_p50_peak]
  slr_diff[, slr_p95_diff := slr_p95_full - slr_p95_peak]
  
  slr_diff[, cluster := factor(cluster, levels = clusters)]
  
  p <- ggplot(slr_diff[year >= 2010]) +
    geom_line(aes(x = year, y = slr_p50_diff * 100, linetype = "50%")) +
    geom_line(aes(x = year, y = slr_p05_diff * 100, linetype = "5%")) +
    geom_line(aes(x = year, y = slr_p95_diff * 100, linetype = "95%")) +
    facet_wrap(~ cluster, nrow = 1) +
    scale_linetype_manual(values = c(2,1,3), name = "Temperature\npercentile") +
    theme_bw() +
    labs(x = "", y = "Global mean sea level rise reduction [cm]")
  
  ggsave(outfile,plot = p, width = 10, height = 5)
  
}