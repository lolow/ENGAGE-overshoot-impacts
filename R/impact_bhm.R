# Burke et al. damage function

# Load coefficient for estimates and bootstrap
read_param_bhm <- function() {
  
  mb_list <- list()
  
  # Original BHM2015 dataset
  dta <- setDT(read.dta13('data/BDD2018/data/input/GrowthClimateDataset_Stata13.dta'))
  dta <- dta[, .(iso3 = iso, year, pop = Pop, gdp = TotGDP * 1.12538, #(USD2010->USD2018) 
                growth = growthWDI,temp = UDel_temp_popweight)]
  dta <- dta[!iso3 %in% c('COD', 'ROU')]
  dta <- dta[iso3 == 'ZAR', iso3 := 'COD']
  dta <- dta[iso3 == 'ROM', iso3 := 'ROU']
  dta <- na.omit(dta)
  mb_list = c(mb_list, list(`BHM DATASET` = dta))
  
  # Non-significant coefficients are set to zero for main estimates
  # (ref. replication code)
  # Pooled bootstrap_noLag 
  pb <- read_fst('data/BDD2018/data/output/bhm_sr_param.fst', as.data.table = T)
  mb <- as.matrix(pb[,.(b1,b2,optimal,normalize)])
  mb_list = c(mb_list, list(`BHM SR` = mb))

  return(mb_list)
}

g_rich <- function(par, temp) { return(par[1] * temp + par[3] * temp^2) }
g_poor <- function(par, temp) { return(par[2] * temp + par[4] * temp^2) }

g_pool <- function(par, temp) { return(par[1] * temp + par[2] * temp^2) }

# rid = 1 estimates
# rid = 2:1000 bootstrap coefficients
warming_effect <- function(temp, temp_baseline, param){
    return(g_pool(param,temp) - g_pool(param,temp_baseline))
}

project_bhm <- function(tas, gdpr, gdpc_2010, tas_base, param){
  stopifnot(length(tas) == 91) # 2010-2100
  stopifnot(length(gdpr) == 91) # 2010-2100
  
  .gdpcap <- rep(gdpc_2010,91)
  .delta <- rep(NA,91)
  for (i in 2:91) {
    .delta[i] <- warming_effect(tas[i] - tas[1] + tas_base, tas_base, param)
    .gdpcap[i] <- .gdpcap[i - 1] * (1 + gdpr[i] + .delta[i])
  }
  
  return(list(year = 2010:2100, 
              gdpcap_cc = .gdpcap))
}

warming_effect_rp <- function(temp, temp_baseline, gdpcap_tm1, param, y_star){
  if (gdpcap_tm1 > y_star) {
    return(g_rich(param,temp) - g_rich(param,temp_baseline))
  } else {
    return(g_poor(param,temp) - g_poor(param,temp_baseline))
  }
}

project_bhm_richpoor <- function(tas, gdpr, gdpc_2010, tas_base, param, y_star){
  stopifnot(length(tas) == 91) # 2010-2100
  stopifnot(length(gdpr) == 91) # 2010-2100
  
  .gdpcap <- rep(gdpc_2010,91)
  .delta <- rep(NA,91)
  for (i in 2:91) {
    .delta[i] <- warming_effect_rp(tas[i] - tas[1] + tas_base, tas_base, .gdpcap[i-1], param, y_star)
    .gdpcap[i] <- .gdpcap[i-1] * (1 + gdpr[i] + .delta[i])
  }
  
  return(list(year = 2010:2100, 
              gdpcap_cc = .gdpcap))
}

project_cc_impact_bhm <- function(ssp_gdpcap,climate,clim_hist,dmg_param,spec, year_base0 = 2000) {
  
  runid <- 1
  par <- dmg_param[[spec]]
  bhm_dta <- dmg_param[['BHM DATASET']]
  bhm_baseline <- bhm_dta[year >= year_base0, .(tas = mean(temp)), 
                          by = "iso3"]

  # split scenarios according to SSP
  scen <- data.table(scenario = unique(climate$scenario))
  
  scen[, ssp := sapply(scenario,scen_ssp)]

  # check iso3 list
  all_iso3 <- intersect(unique(climate$iso3), unique(ssp_gdpcap$iso3))
  all_ssp <- "ssp2"
  all_comb <- expand.grid(all_iso3,all_ssp,stringsAsFactors = F)
  
  # add missing country in bhm baseline
  miss <- clim_hist[iso3 %in% all_iso3[!all_iso3 %in% unique(bhm_dta$iso3)] &
                      year >= 2000,
            .(tas = mean(tas)),
            by = "iso3"]
  bhm_baseline <- rbind(bhm_baseline,miss)
  
  proj_gdpcap <- foreach(i = 1:nrow(all_comb),
                         .combine = rbind) %do% {
      
    i_iso3 <- all_comb[i,1]
    i_ssp <- all_comb[i,2]
    
    .gdpr <- ssp_gdpcap[ssp == i_ssp & iso3 == i_iso3, gdpr]
    .bclim <- bhm_baseline[iso3 == i_iso3, tas]
    .gdpc_2010 <- ssp_gdpcap[ssp == i_ssp & iso3 == i_iso3 & year == 2010, gdpcap_nocc]
    
    climate[scenario %in% scen[ssp == i_ssp, scenario] & iso3 == i_iso3,
            project_bhm(tas, .gdpr, .gdpc_2010, .bclim, par[runid,]),
            by = c("c5model,model,scenario,iso3")]
  }

  proj_gdpcap[, dmg_spec := spec]
  proj_gdpcap[, runid := runid]
    
  return(proj_gdpcap)
  
}

