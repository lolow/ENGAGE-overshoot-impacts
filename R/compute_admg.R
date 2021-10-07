compute_admg_npv <- function(gdp_cc, pair_scen, discount_rates) {
  
  admg_ann <- compute_admg_ann(gdp_cc, pair_scen, seq(2020,2100,by = 5))
  
  dd <- lapply(discount_rates, function(x) admg_ann[!is.na(admg),
                                                    .(dr = x, 
                                                      admg = npvalue(.SD$year, .SD$admg, x, 2020:2100)),
                                                    by = c("c5model","sw","model","scenario","region")])
  dd <- rbindlist(dd)
  
  return(dd)
  
}

compute_admg_years <- function(gdp_cc, pair_scen) {
  
  admg_ann <- compute_admg_ann(gdp_cc, pair_scen, 
                                      c(seq(2020,2100,by = 5)))
  
  return(admg_ann)
  
}

#gdp_cc <- readd(gdp_gwt_BHM.SR)
compute_admg_ann <- function(gdp_cc, pair_scen, years) {
  
  gdp_cc <- gdp_cc[, region := str_replace(region, "R10", "")]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(c5model,sw,model,
                         scenario_ref = scenario,
                         region,year,
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
  
  return(benef)
}

