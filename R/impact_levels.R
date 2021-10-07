# DICE-2016 
# HS-2017
# KW-2020
# damage functions with level quadratic function

read_param_levels <- function() {
  
  p_list <- list()
  # HS-2017 (preferred specification * 1.25 to include missing impacts = non-catastrophic)
  p_list = c(p_list, list(`HS2017 NCAT` = c(alpha1 = 0, alpha2 = -0.007438)))
  # TAKAKURA2019 (SSP-based fitting) rows = ssp{1..5} cols = a x + b x2 + c
  p_list = c(p_list, list(TAKAKURA2019 = matrix(c( 0.09169, 0.20563, -0.12881,
                                                   0.07625, 0.21465, -0.11746,
                                                  -0.32617, 0.33814,  0.19575,
                                                   0.05442, 0.24189, -0.13341,
                                                   0.14312, 0.17533, -0.15604),
                                               nrow = 5,  byrow = T)))
  
  return(p_list)
  
}

project_cc_impact_levels <- function(ssp_gdpcap,temp_magicc,temp_magicc_05,temp_magicc_95,
                                     gmt_hist,dmg_param) {
  
  # split scenarios according to SSP
  scen <- data.table(scenario = unique(temp_magicc$scenario))
  scen[, ssp := sapply(scenario,scen_ssp)]
  
  # SSP GWP
  ssp_gwp <- ssp_gdpcap[,.(gdp_ref = sum(gdpcap_nocc * pop) * 1e-3), by = c("ssp","year")]

  # Year should be numbers
  temp_magicc[, year := as.numeric(year)]
  temp_magicc_05[, year := as.numeric(year)]
  temp_magicc_95[, year := as.numeric(year)]

  # Temperature in 2005 from temp_magicc
  value_2005_50 <- 0.8045643
  value_2005_05 <- 0.7595234
  value_2005_95 <- 0.8627858
  
  #  Compute GWP with CC for all damages functions
  proj_gdp <- NULL
  for (dmgf in c("HS2017 NCAT")) {
    
    #median
    dd <- merge(temp_magicc,scen,by = "scenario")
    dd <- merge(dd,ssp_gwp,by = c("ssp","year"))
    dd[, gdp_cc := gdp_ref * (1 + 
                                (dmg_param[[dmgf]][1] * value + 
                                 dmg_param[[dmgf]][2] * value * value) -
                                (dmg_param[[dmgf]][1] * value_2005_50 + 
                                   dmg_param[[dmgf]][2] * value_2005_50 * value_2005_50)
                                )]
    dd[, sw := 1]
    dd[, c5model := 1]
    proj_gdp <- rbindlist(list(proj_gdp,
                               dd[, .(c5model, sw, model, scenario, year, gdp_cc)]))

    #p05
    dd <- merge(temp_magicc_05,scen,by = "scenario")
    dd <- merge(dd,ssp_gwp,by = c("ssp","year"))
    dd[, gdp_cc := gdp_ref * (1 + (dmg_param[[dmgf]][1] * value + 
                                     dmg_param[[dmgf]][2] * value * value) -
                                (dmg_param[[dmgf]][1] * value_2005_05 + 
                                   dmg_param[[dmgf]][2] * value_2005_05 * value_2005_05)
                              )]
    dd[, sw := 1]
    dd[, c5model := 2]
    proj_gdp <- rbindlist(list(proj_gdp,
                               dd[, .(c5model, sw, model, scenario, year, gdp_cc)]))
    
    #p95
    dd <- merge(temp_magicc_95,scen,by = "scenario")
    dd <- merge(dd,ssp_gwp,by = c("ssp","year"))
    dd[, gdp_cc := gdp_ref * (1 + (dmg_param[[dmgf]][1] * value + 
                                     dmg_param[[dmgf]][2] * value * value) -
                                (dmg_param[[dmgf]][1] * value_2005_95 + 
                                   dmg_param[[dmgf]][2] * value_2005_95 * value_2005_95)
                              )]
    dd[, sw := 1]
    dd[, c5model := 3]
    proj_gdp <- rbindlist(list(proj_gdp,
                               dd[, .(c5model, sw, model, scenario, year, gdp_cc)]))

  }
  
  for (dmgf in c("TAKAKURA2019")) {
    
    #median
    dd <- merge(temp_magicc,scen,by = "scenario")
    dd <- merge(dd,ssp_gwp,by = c("ssp","year"))
    dd[, ssp_num := as.numeric(str_extract(ssp,"\\d"))]
    dd[, gdp_cc := gdp_ref * (1 - (dmg_param[[dmgf]][ssp_num,1] * value + 
                                     dmg_param[[dmgf]][ssp_num,2] * value * value +
                                     dmg_param[[dmgf]][ssp_num,3]) / 100)]
    dd[, sw := 2]
    dd[, c5model := 1]
    proj_gdp <- rbindlist(list(proj_gdp,
                               dd[, .(c5model, sw, model, scenario, year, gdp_cc)]))

    #p05
    dd <- merge(temp_magicc_05,scen,by = "scenario")
    dd <- merge(dd,ssp_gwp,by = c("ssp","year"))
    dd[, ssp_num := as.numeric(str_extract(ssp,"\\d"))]
    dd[, gdp_cc := gdp_ref * (1 - (dmg_param[[dmgf]][ssp_num,1] * value + 
                                     dmg_param[[dmgf]][ssp_num,2] * value * value +
                                     dmg_param[[dmgf]][ssp_num,3]) / 100)]
    dd[, sw := 2]
    dd[, c5model := 2]
    proj_gdp <- rbindlist(list(proj_gdp,
                               dd[, .(c5model, sw, model, scenario, year, gdp_cc)]))
    
    #p95
    dd <- merge(temp_magicc_95,scen,by = "scenario")
    dd <- merge(dd,ssp_gwp,by = c("ssp","year"))
    dd[, ssp_num := as.numeric(str_extract(ssp,"\\d"))]
    dd[, gdp_cc := gdp_ref * (1 - (dmg_param[[dmgf]][ssp_num,1] * value + 
                                     dmg_param[[dmgf]][ssp_num,2] * value * value +
                                     dmg_param[[dmgf]][ssp_num,3]) / 100)]
    dd[, sw := 2]
    dd[, c5model := 3]
    proj_gdp <- rbindlist(list(proj_gdp,
                               dd[, .(c5model, sw, model, scenario, year, gdp_cc)]))
    
  }
  
  proj_gdp[, region := "World"]
  
  return(proj_gdp)
  
}
