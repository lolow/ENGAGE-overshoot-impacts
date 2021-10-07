

# Returns sea level rise as in Li et al. (2020)
sea_level_rise <- function(tas, gmt) {
  
  #parameter
  beta <- 1.33
  d_ml <- 50 #m
  d_tc <- 500 #m
  d_d <- 3150 #m
  w_e <- 0.5e-6 #ms-1
  w_d <- 0.2e-6 #ms-1
  gamma_ml <- 2.4e-4 # K-1
  gamma_tc <- 2.0e-4 # K-1
  gamma_d <- 2.1e-4 # K-1
  nbsecyear <- 31556952 #s

  # Temperature [1850:2200] - 351 , hist (1850:2019) - 170, model (2020:2100) - 81, constant (2101:2200) - 100
  t_surf <- c(gmt,tas[16:96],rep(tas[96],100))
  
  # Thermostatic SLR
  t_ml <- t_surf / beta
  t_tc <- rep(0, length(t_ml))
  t_d <- rep(0, length(t_ml))
  for (i in 2:351) {
    t_tc[i] <- t_tc[i - 1] + (w_e / d_tc * (t_ml[i - 1] - t_tc[i - 1]) - w_d / d_tc * (t_tc[i - 1] - t_d[i - 1])) * nbsecyear  
    t_d[i] <- t_d[i - 1] + w_d / d_d * (t_tc[i - 1] - t_d[i - 1]) * nbsecyear
  }
  
  slr_s = gamma_ml * t_ml * d_ml + gamma_tc * t_tc * d_tc + gamma_d * t_d * d_d
  #plot(1850:2200,slr_s)
  
  # GrIS SLR : Greenland ice sheet melt
  slr_gris_comp <- 71.5 * t_surf + 20.4 * t_surf^2 + 2.8 * t_surf^3
  slr_gris <- cumsum(slr_gris_comp) / 3.61e5
  
 
  # AntIS SLR : Antarctic ice sheet melt
  slr_antis_comp <- c(rep(0,145),0.00074 + 0.00022 * t_surf[146:351]) #1995:2200 
  slr_antis <- cumsum(slr_antis_comp)
  


  # MG SLR : mountain glaciers and ice cap melting
  slr_mg <- rep(0.01945, 351) 
  for (i in 142:351) {
    slr_mg[i] <- 0.01945 
    for (j in 141:(i - 1)) {
      slr_mg[i] <- slr_mg[i] + 0.0008 * t_surf[j] * (1 - slr_mg[j] / 0.41)^1.646
    }
  }
  

  # Global SLR
  slr <- slr_mg + slr_antis + slr_gris + slr_s

  return( list(year = 1850:2200, 
               slr = slr, 
               slr_mg = slr_mg, 
               slr_antis = slr_antis, 
               slr_gris = slr_gris, 
               slr_s = slr_s) )
  
}


project_cc_slr <- function(temp_magicc,temp_magicc_05,temp_magicc_95,gmt_hist,pair_scen_sel) {
  
  # Calibrate MAGICC temperature to match Arnell 2019 assumptions
  # 1981-2010 = 0.61C
  d1 <- gmt_hist[year >= 1981 & year <= 2010, mean(gmt)] - 0.61 # deviation from CRUhist used [-0.015C]
  hist_magicc <- temp_magicc[year >= 2005 & year <= 2015, .(gmt = mean(value)), by = year]
  hist_cru <- gmt_hist[year >= 2005 & year <= 2015, gmt - d1]
  
  diffgmt <- function(x) {
    return(abs(sum(hist_magicc$gmt - x - hist_cru)))
  }
  search <- optimize(diffgmt, c(0,1)) # minimizing distance between IAM ensemble and CRU
  d2 <- search$minimum
  
  # Prepare temperature of paired model_scenarios
  temp <- merge(temp_magicc, pair_scen_sel[delay == "NPi"], by = c("model","scenario"))
  temp[, value := value - d2]
  
  temp_p05 <- temp_magicc_05[, .(model,scenario,region,year,value_p05 = value - d2)]
  temp_p95 <- temp_magicc_95[, .(model,scenario,region,year,value_p95 = value - d2)]

  temp <- merge(temp, temp_p05, by = c("model","scenario","region","year"))
  temp <- merge(temp, temp_p95, by = c("model","scenario","region","year"))
  
  adj_gmt <- gmt_hist[year %in% 1850:2019]$gmt - d1
  
  slr <- temp[, c(sea_level_rise(.SD$value,adj_gmt)[1:2],
                  sea_level_rise(.SD$value_p05,adj_gmt)[2],
                  sea_level_rise(.SD$value_p95,adj_gmt)[2]), 
                  by = c("model","cbudget","poltype","cluster")]

  names(slr) <- c("model","cbudget","poltype","cluster","year","slr_p50","slr_p05","slr_p95")
  
  return(slr)
  
}
