compute_net_benef_npv <- function(gdp_mit, gdp_cc, pair_scen, discount_rates) {
  
  nben_ann <- compute_net_benefit_ann(gdp_mit, gdp_cc, pair_scen, seq(2020,2100,by = 10))
  
  dd <- lapply(discount_rates, function(x) nben_ann[!is.na(nbenefit),
                                                    .(dr = x, 
                                                      nbenefit = npvalue(.SD$year, .SD$nbenefit, x, 2020:2100)),
                                                    by = c("c5model","sw","model","scenario","region")])
  dd <- rbindlist(dd)
  
  return(dd)
  
}

compute_net_benef_years <- function(gdp_mit, gdp_cc, pair_scen) {
  
  nben_ann <- compute_net_benefit_ann(gdp_mit, gdp_cc, pair_scen, 
                                      c(seq(2020,2100,by = 10)))
  
  return(nben_ann)
  
}

#gdp_cc <- readd(gdp_gwt_BHM.SR)
compute_net_benefit_ann <- function(gdp_mit, gdp_cc, pair_scen, years) {
  
  gdp_cc <- gdp_cc[, region := str_replace(region, "R10", "")]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(c5model, sw, model,
                         scenario_ref = scenario,
                         region, year,
                         gdp_cc_ref = gdp_cc)]
  
  benef <- merge(gdp_cc_ref, gdp_cc[scenario != "EN_NoPolicy" & 
                                      !str_detect(scenario,"COV") &
                                      !str_detect(scenario,"DR") & 
                                      !str_detect(scenario,"NDCp") & 
                                      !str_detect(scenario, "INDCi")], 
                 by = c("c5model","sw",
                        "model","region","year"))
  
  # Annual avoided damages 
  benef <- benef[year %in% years, 
                 .(admg = gdp_cc - gdp_cc_ref),
                 by = c("c5model","sw",
                        "model","scenario","region","year")]
  
  setkey(benef,c5model,sw,model,scenario,region,year)
  
  # Annual mitigation cost [% of GDP REF] 
  cost <- copy(gdp_mit)
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[year %in% years & scenario != "EN_NoPolicy",
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
  cost[, c("scenario_ref","gdp_cc_ref") := NULL]
  
  setkey(cost,c5model,sw,model,scenario,region,year)
  
  net_benef <- merge(benef,cost,
                     by = c("c5model","sw",
                            "model","scenario","region","year"))
  
  net_benef[, nbenefit := admg - cmit]
  
  return(net_benef)
}

