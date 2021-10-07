# Generic impact functions

read_dmg_param <- function(ssp_gdp,wdi_stat) {
  return(c(read_param_bhm(),
           read_param_levels()))
}

# Project country-level damage function
# Aggregate at region-level
project_cc_impact <- function(ssp_gdpcap,clim_hist,climate,dmg_spec,dmg_param,year_base0 = 2000) {
  
  # Region mappings
  limits10_iso3 <- fread('data/limits10_iso3_mapping.csv')
  
  gdp_cc <- NULL
  
  cmodels <- unique(climate$c5model)
  sw_c <- unique(climate$sw)
  
  for (i in cmodels) {
    
    for (j in sw_c) {
    
    if (dmg_spec %in% c(bhm_spec)) {
      dd <- project_cc_impact_bhm(ssp_gdpcap,climate[c5model == i & sw == j],
                                  clim_hist,dmg_param,dmg_spec,year_base0)
    }

    # Regional aggregation
    map_ssp <- data.table(scenario = unique(dd$scenario),
                          ssp = sapply(unique(dd$scenario),scen_ssp, USE.NAMES = F))
    dd <- merge(dd,map_ssp, by = "scenario")
    dd <- merge(dd,ssp_gdpcap[,.(ssp,iso3,year,pop)],
                    by = c("ssp","iso3","year"))
    dd <- merge(dd,limits10_iso3, by = "iso3")
    
    # World
    dd0 <- dd[,.(gdp_cc = sum(gdpcap_cc * pop * 1e-3)), # -> Billions (1e6 * 1e-9)
              by = c("dmg_spec","runid","c5model","model","scenario","year")]
    dd0[,region := "World"]
    
    # Regions
    dd1 <- dd[,.(gdp_cc = sum(gdpcap_cc * pop * 1e-3)), # -> Billions
              by = c("dmg_spec","runid","c5model","model","scenario","region","year")]
    
    dd <- rbindlist(list(dd1,dd0),use.names = TRUE)
    
    dd[, sw := j]
    
    gdp_cc <- rbind(gdp_cc,dd)
    
    climate <- climate[!(c5model == i & sw == j)]
    
    #Clean memory
    rm(dd0,dd1)
    gc()

    }

  }

  return(gdp_cc)
}

shrink_gdp_engage <- function(gdp_gwt) {
  
  return(gdp_gwt[year %in% seq(2020,2100,by = 5) & region == "World", 
                 .(c5model, sw, model, scenario, region, year, gdp_cc)])
  
}

