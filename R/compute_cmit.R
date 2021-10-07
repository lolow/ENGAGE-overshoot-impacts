compute_cmit_npv <- function(gdp_mit, gdp_cc, pair_scen, discount_rates) {
  
  cmit_ann <- compute_cmit_ann(gdp_mit, gdp_cc, pair_scen, seq(2020,2100,by = 5))
  
  dd <- lapply(discount_rates, function(x) cmit_ann[!is.na(cmit),
                                                    .(dr = x, 
                                                      cmit = npvalue(.SD$year, .SD$cmit, x, 2020:2100)),
                                                    by = c("c5model","sw","model","scenario","region")])
  dd <- rbindlist(dd)
  
  return(dd)
  
}

compute_cmit_years <- function(gdp_mit, gdp_cc, pair_scen) {
  
  cmit_ann <- compute_cmit_ann(gdp_mit, gdp_cc, pair_scen, 
                                      c(seq(2020,2100,by = 10)))
  
  return(cmit_ann)
  
}

#gdp_cc <- readd(gdp_gwt_BHM.SR)
compute_cmit_ann <- function(gdp_mit, gdp_cc, pair_scen, years) {
  
  gdp_cc <- gdp_cc[, region := str_replace(region, "R10", "")]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(c5model,sw,model,
                         scenario_ref = scenario,
                         region,year,
                         gdp_cc_ref = gdp_cc)]

  # Annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[year %in% years & scenario != "EN_NoPolicy",
                  .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
                  by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, c("scenario_ref") := NULL]

  return(cost)
}

