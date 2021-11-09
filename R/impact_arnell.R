
iindic <- c(
  "Agricultural drought (SPEI) frequency",           # 01 [% chance]        
  "Agricultural drought (SPEI): proportion of time", # 02 [% of time]
  "Agricultural drought (SPI): frequency",           # 03 [% chance]
  "Agricultural drought (SPI): proportion of time",  # 04 [% of time]
  "Cooling degree days",                             # 05 [C]
  "Frequency of current 30-year flood",              # 06 [% chance]
  "Frequency of current 50-year flood",              # 07 [% chance]
  "Frost days",                                      # 08 [days/years]
  "Heating degree days",                             # 09 [C]
  "Heatwave duration",                               # 10 [days/years]
  "Heatwave frequency",                              # 11 [% chance]
  "Hot spell frequency: Maize",                      # 12 [% chance]
  "Hot spell frequency: Rice",                       # 13 [% chance]
  "Hot spell frequency: Soybean",                    # 14 [% chance]
  "Hot spell frequency: Spring Wheat",               # 15 [% chance]
  "Hot spell frequency: Winter Wheat",               # 16 [% chance]
  "Hot-humid days",                                  # 17 [days/years]
  "Hydrological drought frequency",                  # 18 [% chance]
  "Hydrological drought: proportion of time",        # 19 [% of time]
  "Major heatwave frequency",                        # 20 [% chance]
  "Reduction in crop duration: Maize",               # 21 [days]
  "Reduction in crop duration: Rice",                # 22 [days]
  "Reduction in crop duration: Soybean",             # 23 [days]
  "Reduction in crop duration: Spring Wheat",        # 24 [days]
  "Reduction in crop duration: Winter Wheat",        # 25 [days]
  "Reduction in rainfall: Maize",                    # 26 [% chance]
  "Reduction in rainfall: Rice",                     # 27 [% chance]
  "Reduction in rainfall: Soybean",                  # 28 [% chance]
  "Reduction in rainfall: Spring Wheat",             # 29 [% chance]
  "Reduction in rainfall: Winter wheat",             # 30 [% chance]
  "Runoff decreases",                                # 31 [% of area]
  "Runoff increases",                                # 32 [% of area]
  "Temperature increase"                             # 33 [C]
)

iindic2 <- c(
  "Agricultural drought (SPEI) frequency",           # 01 [% chance]        
  "Agricultural drought (SPEI): proportion of time", # 02 [% of time]
  "Agricultural drought (SPI): frequency",           # 03 [% chance]
  "Agricultural drought (SPI): proportion of time",  # 04 [% of time]
  "Cooling degree days",                             # 05 [C]
  "Frequency of current 30-year flood",              # 06 [% chance]
  "Frequency of current 50-year flood",              # 07 [% chance]
  "Frost days",                                      # 08 [days/years]
  "Heating degree days",                             # 09 [C]
  "Heatwave duration",                               # 10 [days/years]
  "Heatwave frequency",                              # 11 [% chance]
  "Hot spell frequency: Maize",                      # 12 [% chance]
  "Hot spell frequency: Rice",                       # 13 [% chance]
  "Hot spell frequency: Soybean",                    # 14 [% chance]
  "Hot spell frequency: Spring Wheat",               # 15 [% chance]
  "Hot spell frequency: Winter Wheat",               # 16 [% chance]
  "Hot-humid days",                                  # 17 [days/years]
  "Hydrological drought frequency",                  # 18 [% chance]
  "Hydrological drought: proportion of time",        # 19 [% of time]
  "Major heatwave frequency",                        # 20 [% chance]
  "Reduction in crop duration: Maize",               # 21 [days]
  "Reduction in crop duration: Rice",                # 22 [days]
  "Reduction in crop duration: Soybean",             # 23 [days]
  "Reduction in crop duration: Spring Wheat",        # 24 [days]
  "Reduction in crop duration: Winter Wheat",        # 25 [days]
  "Reduction in rainfall: Maize",                    # 26 [% chance]
  "Reduction in rainfall: Rice",                     # 27 [% chance]
  "Reduction in rainfall: Soybean",                  # 28 [% chance]
  "Reduction in rainfall: Spring Wheat",             # 29 [% chance]
  "Reduction in rainfall: Winter wheat",             # 30 [% chance]
  "Runoff decreases",                                # 31 [% of area]
  "Runoff increases",                                # 32 [% of area]
  "Temperature increase"                             # 33 [C]
)

iindic_labels <- iindic

iindic_ks <- c(1,2,5,8,9,10,11,20,21,22,23,24,25,31,32)
iindic_exc <-  c(1, 2, 5, 8, 9, 10, 11, 20, 21, 22, 23, 24, 25, 31, 32)

iindic_labels_short <- c(
  "Agri. drought: frequency",     # 01 [% chance]        
  "Agri. drought: time", # 02 [% of time]
  "Agricultural drought (SPI): frequency",           # 03 [% chance]
  "Agricultural drought (SPI): proportion of time",  # 04 [% of time]
  "Cooling degree days",                             # 05 [C]
  "Frequency of current 30-year flood",              # 06 [% chance]
  "Frequency of current 50-year flood",              # 07 [% chance]
  "Frost days",                                      # 08 [days/years]
  "Heating degree days",                             # 09 [C]
  "Heatwave duration",                               # 10 [days/years]
  "Heatwave frequency",                              # 11 [% chance]
  "Hot spell frequency: Maize",                      # 12 [% chance]
  "Hot spell frequency: Rice",                       # 13 [% chance]
  "Hot spell frequency: Soybean",                    # 14 [% chance]
  "Hot spell frequency: Spring Wheat",               # 15 [% chance]
  "Hot spell frequency: Winter Wheat",               # 16 [% chance]
  "Hot-humid days",                                  # 17 [days/years]
  "Hydrological drought frequency",                  # 18 [% chance]
  "Hydrological drought: proportion of time",        # 19 [% of time]
  "Major heatwave frequency",                        # 20 [% chance]
  "Crop days lost: Maize",                    # 21 [days]
  "Crop days lost: Rice",                     # 22 [days]
  "Crop days lost: Soybean",                  # 23 [days]
  "Crop days lost: S. Wheat",                 # 24 [days]
  "Crop days lost: W. Wheat",                 # 25 [days]
  "Reduction in rainfall: Maize",                    # 26 [% chance]
  "Reduction in rainfall: Rice",                     # 27 [% chance]
  "Reduction in rainfall: Soybean",                  # 28 [% chance]
  "Reduction in rainfall: Spring Wheat",             # 29 [% chance]
  "Reduction in rainfall: Winter wheat",             # 30 [% chance]
  "Runoff decreases",                                # 31 [% of area]
  "Runoff increases",                                # 32 [% of area]
  "Temperature increase"                             # 33 [C]
)

iindic_max <- c(1, 2, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 
                20, 26, 27, 28, 29, 30, 31, 32,
                33, 21:25)

iindic_hw <- c(which(iindic_labels=="Major heatwave frequency"),
               which(iindic_labels=="Heatwave frequency"),
               which(iindic_labels=="Heatwave duration"))

iindic_cr <- c(which(iindic=="Reduction in crop duration: Rice"),
               which(iindic=="Reduction in crop duration: Maize"),
               which(iindic=="Reduction in crop duration: Spring Wheat"))

iindic_min <- c(8, 9)

iindic_sum <- c()

iunit <- c(
  "[% chance]",        
  "[% of time]",
  "[% chance]",
  "[% of time]",
  "[C]",
  "[% chance]",
  "[% chance]",
  "[days/years]",
  "[C]",
  "[days/years]",
  "[% chance]",
  "[% chance]",
  "[% chance]",
  "[% chance]",
  "[% chance]",
  "[% chance]",
  "[days/years]",
  "[% chance]",
  "[% of time]",
  "[% chance]",
  "[days]",
  "[days]",
  "[days]",
  "[days]",
  "[days]",
  "[% chance]",
  "[% chance]",
  "[% chance]",
  "[% chance]",
  "[% chance]",
  "[% of area]",
  "[% of area]",
  "[C]"
)

iindic_map <- c("Major heatwave frequency",
                    "Heatwave frequency",
                    "Heatwave duration")

ireg <- c('Globe', 'Africa', 'Europe', 'North America', 'South America',
          'Asia', 'West Africa', 'Central Africa', 'East Africa', 'Southern Africa',
          'North Africa', 'Middle East', 'South Asia', 'South East Asia',
          'East Asia', 'Central Asia', 'Australasia', 'Western Europe',
          'Central Europe', 'Eastern Europe', 'Canada', 'USA',
          'Central America', 'Brazil', 'Rest of south America')

iregg <- c('Globe', 'Africa', 'Europe', 'North America', 'South America',
          'Asia')

ireg_labels <- ireg

ireg0 <- c('Globe', 'West Africa', 'Central Africa', 'East Africa', 'Southern Africa',
           'North Africa', 'Middle East', 'South Asia', 'South East Asia',
           'East Asia', 'Central Asia', 'Australasia', 'Western Europe',
           'Central Europe', 'Eastern Europe', 'Canada', 'USA',
           'Central America', 'Brazil', 'Rest of south America')

ireg_map <- c('West Africa', 'Central Africa', 'East Africa', 'Southern Africa',
              'North Africa', 'Middle East', 'South Asia', 'South East Asia',
              'East Asia', 'Central Asia', 'Australasia', 'Western Europe',
              'Central Europe', 'Eastern Europe', 'Canada', 'USA',
              'Central America', 'Brazil', 'Rest of south America')

ic5models <- c("bcccs1","cccma2","ccsm4_","cesm1c","cmcccm","cnrmc5",
               "csibom","csiro6","fgoals","gfdlc3","gfdles","gisseh",
               "gisser","hadgcc","hadgem","inmcm4","ipsllr","ipslmr",
               "miroc5","miroce","mpiesr","mricm3","noresm")
             
arnell_region_iso3 <- read_fst('data/arnell_region_iso3.fst', as.data.table = T)

read_data_arnell2019cc <- function() {
  
  sheets <- c('dt15','dt2','dt25','dt3','dt35','dt4')
  dtemp <- c(1.5,2,2.5,3,3.5,4)

  xlsfile <- 'data/Arnell2019-ClimaticChange/10584_2019_2464_MOESM2_ESM.xlsx'
  
  headers <- names(read_excel(xlsfile, sheet = 'dt15', skip = 2, n_max = 2))
  headers[1] <- "impact"
  headers[2] <- "unit"

  # Read impact per temperature increment
  imptd <- NULL
  for (i in seq_along(sheets)) {
    
    for (j in seq_along(ireg)) {

      tab <- read_excel(xlsfile, sheet = sheets[i], skip = 3 + 36 * (j - 1), 
                        n_max = 33, col_names = headers)
      tab <- setDT(tab)
      tab <- tab[,c(-6:-2)]
      tab <- melt(tab, id.vars = "impact",
                  variable.name = "c5model", variable.factor = FALSE)
      tab[, dgmt := dtemp[i]]
      tab[, region := ireg[j]]
      
      imptd <- rbindlist(list(imptd,tab))

    }
    
  }
  
  # Read reference impacts (1981–2010 = 0.61C)
  for (j in seq_along(ireg)) {
    
    tab <- read_excel(xlsfile, sheet = sheets[1], skip = 3 + 36 * (j - 1), 
                      n_max = 33, col_names = headers)
    tab <- setDT(tab)
    setnames(tab, "1981-2010", "value")
    tab <- tab[,c(1,3)]
    tab[, c5model := "hist"]
    tab[, dgmt := 0.61]
    tab[, region := ireg[j]]
    
    imptd <- rbindlist(list(imptd,tab), use.names = TRUE)
    
  }
  
  setkey(imptd, impact, c5model, region)
  
  return(imptd)
  
}

project_cc_impact_arnell <- function(temp_magicc_50,gmt_hist,pair_scen_sel,proxy_impact,interp = 0) {
  
  # Calibrate MAGICC temperature to match Arnell 2019 assumptions
  # 1981-2010 = 0.61C
  d1 <- gmt_hist[year >= 1981 & year <= 2010, mean(gmt)] - 0.61 # deviation from CRUhist used
  hist_magicc <- temp_magicc_50[year >= 2005 & year <= 2015, .(gmt = mean(value)), by = year]
  hist_cru <- gmt_hist[year >= 2005 & year <= 2015, gmt - d1]
  
  diffgmt <- function(x) {
    return(abs(sum(hist_magicc$gmt - x - hist_cru)))
  }
  search <- optimize(diffgmt, c(0,1)) # minimizing distance between IAM ensemble and CRU
  d2 <- search$minimum
  
  # Prepare temperature of paired model_scenarios
  temp <- merge(temp_magicc_50, pair_scen_sel[delay == "NPi"], by = c("model","scenario"))
  temp[, value := value - d2]
  temp <- dcast(temp, model + delay + cbudget + year + cluster ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  # Time horizon
  temp <- temp[year >= 2010 & year <= 2100]
  
  impacts <- NULL
  
  for (i in seq_along(iindic)) {
    
    imp0 <- NULL

    for (j in seq_along(ireg)) {

      # Compute avoided impacts for all iams and cmodels. 
      impreg <- NULL
      
      for (k in seq_along(ic5models)) {

        # Isolate damage function
        impfunc <- proxy_impact[impact == iindic[i] &
                                region == ireg[j] &
                                c5model %in% c("hist",ic5models[k])][order(dgmt)]
        stopifnot(nrow(impfunc) == 7)
        
        dd <- temp[,.(model,delay,cbudget,cluster,year)]
        
        # Spline interpolation
        if (interp == 1) {
          dd$full <- pmin(pmax(spline(x = impfunc$dgmt, 
                            y = impfunc$value, 
                            xout = temp$full, 
                            ties = min,
                            xmin = 0.61, xmax = 4)[['y']],
                            min(impfunc$value)),max(impfunc$value))
          
          dd$peak <- pmin(pmax(spline(x = impfunc$dgmt, 
                            y = impfunc$value, 
                            xout = temp$peak, 
                            xmin = 0.61, xmax = 4)[['y']],
                            min(impfunc$value)),max(impfunc$value))
        }

        # Linear interpolation
        if (interp == 0) {
          dd$full <- approx(x = impfunc$dgmt, 
                            y = impfunc$value, 
                            xout = temp$full,
                            rule = 2)[['y']]
          dd$peak <- approx(x = impfunc$dgmt, 
                            y = impfunc$value, 
                            xout = temp$peak,
                            rule = 2)[['y']]
        }
        
        if (i %in% 21:25) {
          dd$full <- -dd$full
          dd$peak <- -dd$peak
        }
        
        dd$c5model = k
        
        impreg <- rbindlist(list(impreg,dd))

      }

      # Difference in maximum chance or maximum
      if (i %in% iindic_max) {
        xx <- impreg[, .(indic_full = max(full),
                         indic_peak = max(peak),
                         indic_sum_full = sum(full[21:91]), #2030-2100
                         indic_sum_peak = sum(peak[21:91]), #2030-2100
                         indic = max(full) - max(peak), 
                         rindic = (max(full) - max(peak)) / max(peak) * 100, 
                         sindic = (sum(full) - sum(peak)) / sum(peak) * 100), 
                     by = c("c5model","model","delay","cbudget","cluster")]
        
      }
      
      # Difference in minimum
      if (i %in% iindic_min) {
        
        xx <- impreg[, .(indic_full = min(full),
                         indic_peak = min(peak),
                         indic_sum_full = sum(full),
                         indic_sum_peak = sum(peak),
                         indic = min(full) - min(peak), 
                         rindic = (min(full) - min(peak)) / min(peak) * 100,
                         sindic = (sum(full) - sum(peak)) / sum(peak) * 100), 
                     by = c("c5model","model","delay","cbudget","cluster")]
        
      }

      xx$ireg <- j
      imp0 <- rbindlist(list(imp0,xx))
      
    }
    
    imp0$iindic <- i
    impacts <- rbindlist(list(impacts,imp0))

  }
  
  return(impacts)
  
}

project_cc_impact_arnell_samp <- function(temp_magicc_dist,
                                     temp_magicc_prob,
                                     gmt_hist,
                                     pair_scen_sel,
                                     proxy_impact,
                                     iindic00,
                                     ireg00,
                                     cluster00,
                                     interp = 0,
                                     cluster_col = 1) {
  
  # Calibrate MAGICC temperature to match Arnell 2019 assumptions
  # 1981-2010 = 0.61C
  d1 <- gmt_hist[year >= 1981 & year <= 2010, mean(gmt)] - 0.61 # deviation from CRUhist used
  hist_magicc <- temp_magicc_prob[prob == 0.5 & year >= 2005 & year <= 2015, .(gmt = mean(value)), by = year]
  hist_cru <- gmt_hist[year >= 2005 & year <= 2015, gmt - d1]
  
  diffgmt <- function(x) {
    return(abs(sum(hist_magicc$gmt - x - hist_cru)))
  }
  search <- optimize(diffgmt, c(0,1)) # minimizing distance between IAM ensemble and CRU
  d2 <- search$minimum
  
  # Prepare temperature of paired model_scenarios
  if (cluster_col == 1) {
    xx <- pair_scen_sel[cluster == cluster00]
  } else if (cluster_col == 2) {
    xx <- pair_scen_sel[cluster2 == cluster00]
  }
  
  temp <- merge(temp_magicc_dist, xx, by = c("model","scenario"))
  temp <- dcast(temp, model + delay + poltype + cbudget + year ~ param,
                value.var = "value",
                fun.aggregate = sum)
  temp <- temp[year >= 2020 & year <= 2100]
  dist <- merge(temp_magicc_prob, pair_scen_sel[delay == "NPi"], by = c("model","scenario"))
  dist <- dcast(dist,  model + delay + poltype + cbudget + year ~ prob, 
                value.var = "value",
                fun.aggregate = sum)
  temp <- merge(temp,dist, by = c("model","delay","poltype","cbudget","year"))
  
  i <- which(iindic00 == iindic)
  j <- which(ireg00 == ireg)
  n <- nbsw # number of states of the world
  
  # Unique model-scenario pairs
  modscens <- unique(temp[,.(model,delay,cbudget)])
  nc <- n * nrow(modscens) * length(ic5models)
  nr <- length(2020:2100)
  samp_max_peak <- array(0,c(nc))
  samp_max_full <- array(0,c(nc))
  
  set.seed(42)
  pp <- c(0.05,0.1,0.25,0.33,0.5,0.67,0.75,0.9,0.95)
  
  max_imp_traj <- function(p0,SD,impfunc00) {
    
    # get temperature trajectory
    if (p0 <= 0.05) { 
      tt <- SD[,qnorm(p0,l_mean,l_sd)]
    } else if (p0 >= 0.95) {
      tt <- SD[,qlnorm(p0,r_meanlog,r_sdlog)]
    } else {
      tt <- SD[,spline(pp,c(`0.05`,`0.1`,`0.25`,`0.33`,`0.5`,`0.67`,`0.75`,`0.9`,`0.95`),
                       xout = p0)$y, by = seq_len(nrow(SD))][,V1]
    }
    tt <- tt - d2
    
    if (interp == 0) {
      samp <- approx(x = impfunc00$dgmt, 
                     y = impfunc00$value, 
                     xout = tt,
                     rule = 2)[['y']]
    }
    if (interp == 1) {
      samp <- spline(x = impfunc00$dgmt, 
                     y = impfunc00$value, 
                     xout = tt, 
                     ties = min,
                     xmin = 0.61, xmax = 4)[['y']]
    }
    
    #samp <- pmin(pmax(samp,min(impfunc00$value)),max(impfunc00$value))
    if (i %in% 21:25) {
      samp <- -samp
    }
    if (i %in% iindic_min) {
      return(min(samp))
    } else {
      return(max(samp))
    }

  }
  
  for (k in seq_along(ic5models)) {
    
    # Isolate damage function
    impfunc <- proxy_impact[impact == iindic[i] &
                              region == ireg[j] &
                              c5model %in% c("hist",ic5models[k])][order(dgmt)]
    stopifnot(nrow(impfunc) == 7)
    
    for (ms in 1:nrow(modscens)) {
      
      x1 <- temp[modscens[ms,], on=.(model,delay,cbudget)][poltype == "peak"]
      x2 <- temp[modscens[ms,], on=.(model,delay,cbudget)][poltype == "full"]
      
      idx <- (k - 1) * (nrow(modscens) * n) + (ms - 1) * n + 1
      samp_max_peak[idx:(idx+n-1)] <- sapply(runif(n), max_imp_traj, x1, impfunc)
      samp_max_full[idx:(idx+n-1)] <- sapply(runif(n), max_imp_traj, x2, impfunc)
      
    }
    
  }
  
  p50_peak = quantile(samp_max_peak,0.5)
  p05_peak = quantile(samp_max_peak,0.05)
  p95_peak = quantile(samp_max_peak,0.95)
  p10_peak = quantile(samp_max_peak,0.10)
  p90_peak = quantile(samp_max_peak,0.90)
  p33_peak = quantile(samp_max_peak,0.33)
  p67_peak = quantile(samp_max_peak,0.67)
  min_peak = min(samp_max_peak)
  max_peak = max(samp_max_peak)
  mean_peak = mean(samp_max_peak)
  
  p50_full = quantile(samp_max_full,0.5)
  p05_full = quantile(samp_max_full,0.05)
  p95_full = quantile(samp_max_full,0.95)
  p10_full = quantile(samp_max_full,0.10)
  p90_full = quantile(samp_max_full,0.90)
  p33_full = quantile(samp_max_full,0.33)
  p67_full = quantile(samp_max_full,0.67)
  min_full = min(samp_max_full)
  max_full = max(samp_max_full)
  mean_full = mean(samp_max_full)
  
  res <- data.table(
    cluster = cluster00,
    iindic = iindic00,
    ireg = ireg00,
    poltype = c(rep("peak",10),rep("full",10)),
    statistic = c("min","p05","p10","p33","p50","p67","p90","p95","max","mean",
                  "min","p05","p10","p33","p50","p67","p90","p95","max","mean"),
    value = c(min_peak,p05_peak,p10_peak,p33_peak,p50_peak,p67_peak,p90_peak,p95_peak,max_peak,mean_peak,
              min_full,p05_full,p10_full,p33_full,p50_full,p67_full,p90_full,p95_full,max_full,mean_full)
  )

  return(res)
  
}

project_cc_impact_arnell_samp_raw <- function(temp_magicc_dist,
                                          temp_magicc_prob,
                                          gmt_hist,
                                          pair_scen_sel,
                                          proxy_impact,
                                          iindic00,
                                          ireg00,
                                          cluster00,
                                          interp = 0) {
  
  if (is.numeric(iindic00)) {
    iindic00 <- iindic[iindic00]
  }
  
  if (is.numeric(ireg00)) {
    ireg00 <- ireg[ireg00]
  }
  
  if (is.numeric(cluster00)) {
    cluster00 <- clusters[cluster00]
  }
  
  # Calibrate MAGICC temperature to match Arnell 2019 assumptions
  # 1981-2010 = 0.61C
  d1 <- gmt_hist[year >= 1981 & year <= 2010, mean(gmt)] - 0.61 # deviation from CRUhist used
  hist_magicc <- temp_magicc_prob[prob == 0.5 & year >= 2005 & year <= 2015, .(gmt = mean(value)), by = year]
  hist_cru <- gmt_hist[year >= 2005 & year <= 2015, gmt - d1]
  
  diffgmt <- function(x) {
    return(abs(sum(hist_magicc$gmt - x - hist_cru)))
  }
  search <- optimize(diffgmt, c(0,1)) # minimizing distance between IAM ensemble and CRU
  d2 <- search$minimum
  
  # Prepare temperature of paired model_scenarios
  temp <- merge(temp_magicc_dist, pair_scen_sel[delay == "NPi" & cluster == cluster00], by = c("model","scenario"))
  temp <- dcast(temp, model + delay + poltype + cbudget + year ~ param,
                value.var = "value",
                fun.aggregate = sum)
  temp <- temp[year >= 2020 & year <= 2100]
  dist <- merge(temp_magicc_prob, pair_scen_sel[delay == "NPi"], by = c("model","scenario"))
  dist <- dcast(dist,  model + delay + poltype + cbudget + year ~ prob, 
                value.var = "value",
                fun.aggregate = sum)
  temp <- merge(temp,dist, by = c("model","delay","poltype","cbudget","year"))
  
  i <- which(iindic00 == iindic)
  j <- which(ireg00 == ireg)
  n <- nbsw_raw # number of states of the world
  
  # Unique model-scenario pairs
  modscens <- unique(temp[,.(model,delay,cbudget)])
  nc <- n * nrow(modscens) * length(ic5models)
  nr <- length(2020:2100)
  samp_max_peak <- array(0,c(nc))
  samp_max_full <- array(0,c(nc))
  
  set.seed(42)
  pp <- c(0.05,0.1,0.25,0.33,0.5,0.67,0.75,0.9,0.95)
  
  max_imp_traj <- function(p0,SD,impfunc00) {
    
    # get temperature trajectory
    if (p0 <= 0.05) { 
      tt <- SD[,qnorm(p0,l_mean,l_sd)]
    } else if (p0 >= 0.95) {
      tt <- SD[,qlnorm(p0,r_meanlog,r_sdlog)]
    } else {
      tt <- SD[,spline(pp,c(`0.05`,`0.1`,`0.25`,`0.33`,`0.5`,`0.67`,`0.75`,`0.9`,`0.95`),
                       xout = p0)$y, by = seq_len(nrow(SD))][,V1]
    }
    tt <- tt - d2
    
    if (interp == 0) {
      samp <- approx(x = impfunc00$dgmt, 
                     y = impfunc00$value, 
                     xout = tt,
                     rule = 2)[['y']]
    }
    if (interp == 1) {
      samp <- spline(x = impfunc00$dgmt, 
                     y = impfunc00$value, 
                     xout = tt, 
                     ties = min,
                     xmin = 0.61, xmax = 4)[['y']]
    }
    
    if (i %in% 21:25) {
      samp <- -samp
    }
    if (i %in% iindic_min) {
      return(min(samp))
    } else {
      return(max(samp))
    }
    
  }
  
  for (k in seq_along(ic5models)) {
    
    # Isolate damage function
    impfunc <- proxy_impact[impact == iindic[i] &
                              region == ireg[j] &
                              c5model %in% c("hist",ic5models[k])][order(dgmt)]
    stopifnot(nrow(impfunc) == 7)
    
    for (ms in 1:nrow(modscens)) {
      
      x1 <- temp[modscens[ms,], on=.(model,delay,cbudget)][poltype == "peak"]
      x2 <- temp[modscens[ms,], on=.(model,delay,cbudget)][poltype == "full"]
      
      idx <- (k - 1) * (nrow(modscens) * n) + (ms - 1) * n + 1
      samp_max_peak[idx:(idx+n-1)] <- sapply(runif(n), max_imp_traj, x1, impfunc)
      samp_max_full[idx:(idx+n-1)] <- sapply(runif(n), max_imp_traj, x2, impfunc)
      
    }
    
  }
  
  res <- data.table(
    cluster = cluster00,
    iindic = iindic00,
    ireg = ireg00,
    poltype = c(rep("peak",nc),rep("full",nc)),
    value = c(samp_max_peak,
              samp_max_full)
  )
  
  return(res)
  
}

project_cc_impact_arnell_samp_stat <- function(temp_magicc_dist,
                                              temp_magicc_prob,
                                              gmt_hist,
                                              pair_scen_sel,
                                              proxy_impact,
                                              iindic00,
                                              ireg00,
                                              cluster00,
                                              interp = 0) {
  
  if (is.numeric(iindic00)) {
    iindic00 <- iindic[iindic00]
  }
  
  if (is.numeric(ireg00)) {
    ireg00 <- ireg[ireg00]
  }
  
  if (is.numeric(cluster00)) {
    cluster00 <- clusters[cluster00]
  }
  
  # Calibrate MAGICC temperature to match Arnell 2019 assumptions
  # 1981-2010 = 0.61C
  d1 <- gmt_hist[year >= 1981 & year <= 2010, mean(gmt)] - 0.61 # deviation from CRUhist used
  hist_magicc <- temp_magicc_prob[prob == 0.5 & year >= 2005 & year <= 2015, .(gmt = mean(value)), by = year]
  hist_cru <- gmt_hist[year >= 2005 & year <= 2015, gmt - d1]
  
  diffgmt <- function(x) {
    return(abs(sum(hist_magicc$gmt - x - hist_cru)))
  }
  search <- optimize(diffgmt, c(0,1)) # minimizing distance between IAM ensemble and CRU
  d2 <- search$minimum
  
  # Prepare temperature of paired model_scenarios
  temp <- merge(temp_magicc_dist, pair_scen_sel[delay == "NPi" & cluster == cluster00], by = c("model","scenario"))
  temp <- dcast(temp, model + delay + poltype + cbudget + year ~ param,
                value.var = "value",
                fun.aggregate = sum)
  temp <- temp[year >= 2020 & year <= 2100]
  dist <- merge(temp_magicc_prob, pair_scen_sel[delay == "NPi"], by = c("model","scenario"))
  dist <- dcast(dist,  model + delay + poltype + cbudget + year ~ prob, 
                value.var = "value",
                fun.aggregate = sum)
  temp <- merge(temp,dist, by = c("model","delay","poltype","cbudget","year"))
  
  i <- which(iindic00 == iindic)
  j <- which(ireg00 == ireg)
  n <- nbsw_stat # number of states of the world
  
  # Unique model-scenario pairs
  modscens <- unique(temp[,.(model,delay,cbudget)])
  nr <- length(2020:2100)
  nc <- n * nrow(modscens) * length(ic5models)
  samp_max_peak <- array(0,c(nr,nc))
  samp_max_full <- array(0,c(nr,nc))
  
  set.seed(42)
  pp <- c(0.05,0.1,0.25,0.33,0.5,0.67,0.75,0.9,0.95)
  
  max_imp_traj <- function(p0,SD,impfunc00) {
    
    p0 <- runif(1)
    
    # get temperature trajectory
    if (p0 <= 0.05) { 
      tt <- SD[,qnorm(p0,l_mean,l_sd)]
    } else if (p0 >= 0.95) {
      tt <- SD[,qlnorm(p0,r_meanlog,r_sdlog)]
    } else {
      tt <- SD[,spline(pp,c(`0.05`,`0.1`,`0.25`,`0.33`,`0.5`,`0.67`,`0.75`,`0.9`,`0.95`),
                       xout = p0)$y, by = seq_len(nrow(SD))][,V1]
    }
    tt <- tt - d2
    
    if (interp == 0) {
      samp <- approx(x = impfunc00$dgmt, 
                     y = impfunc00$value, 
                     xout = tt,
                     rule = 2)[['y']]
    }
    if (interp == 1) {
      samp <- spline(x = impfunc00$dgmt, 
                     y = impfunc00$value, 
                     xout = tt, 
                     ties = min,
                     xmin = 0.61, xmax = 4)[['y']]
    }
    
    if (i %in% 21:25) {
      samp <- -samp
    }

    return(samp)

  }
  
  for (k in seq_along(ic5models)) {
    
    # Isolate damage function
    impfunc <- proxy_impact[impact == iindic[i] &
                              region == ireg[j] &
                              c5model %in% c("hist",ic5models[k])][order(dgmt)]
    stopifnot(nrow(impfunc) == 7)
    
    for (ms in 1:nrow(modscens)) {
      
      x1 <- temp[modscens[ms,], on=.(model,delay,cbudget)][poltype == "peak"]
      x2 <- temp[modscens[ms,], on=.(model,delay,cbudget)][poltype == "full"]
      
      idx <- (k - 1) * (nrow(modscens) * n) + (ms - 1) * n + 1
      samp_max_peak[,idx:(idx+n-1)] <- sapply(1:n, max_imp_traj, x1, impfunc)
      samp_max_full[,idx:(idx+n-1)] <- sapply(1:n, max_imp_traj, x2, impfunc)
      
    }
    
  }
  
  res <- list(peak = samp_max_peak,
              full = samp_max_full,
              iindic = i,
              ireg = j,
              cluster = which(clusters == cluster00))
  
  return(res)
  
}


# summarise table
analysis_impact_arnell <- function(outfile) {
  
  loadd(pair_scen_sel)
  
  dd <- readd(impact_samp)
  setnames(dd,"iindic","iindic_label")
  dd <- merge(dd[],
              data.table(iindic=1:length(iindic), iindic_label=iindic), by = "iindic_label")
  dd[,iindic_label := NULL]

  dd <- dd[cluster %in% clusters[c(1,4)]]
  #dd <- dd[ireg %in% ireg[1:6]]
  dd <- dd[!iindic %in% c(3,4,6,7,12:19,26:30,33)]
  dd[, impact := paste0(iindic_labels[iindic]," ",iunit[iindic])]
  dd[, impact := str_replace(impact, "%","\\%")]
  dd[, region := ireg]
  
  dd <- dcast(dd, cluster + iindic + impact + region ~ statistic + poltype)
  
  # Table paper
  dd[,col_peak := ifelse(iindic %in% c(10,21:32),
                     paste0(round(p50_peak,1)," [",round(p05_peak,1),";",round(p95_peak,1),"]"),
                     paste0(round(p50_peak,0)," [",round(p05_peak,0),";",round(p95_peak,0),"]"))]
  dd[,col_full := ifelse(iindic %in% c(10,21:32),
                           paste0(round(p50_full,1)," [",round(p05_full,1),";",round(p95_full,1),"]"),
                           paste0(round(p50_full,0)," [",round(p05_full,0),";",round(p95_full,0),"]"))]
  
  dd <- melt(dd, id.vars = c("cluster","region","impact"),
             measure.vars = c("col_full","col_peak"))
  
  dd <- dcast(dd, region + impact ~ cluster + variable)
  
  names(dd) <- c("region","impact","full2C","peak2C","full15C","peak15c")
  
  dd[, latex := paste(peak15c,full15C,peak2C,full2C, sep = "&")]
  
  fwrite(dd, outfile)
  
}


tail_analysis <- function(impact_indic, pair_scen_sel, outfile) {
  
  dd <- impact_indic[delay == "NPi"]
  
  ss <- dd[,
           .(q99_peak = quantile(indic_peak,0.99),
             q50_peak = quantile(indic_peak,0.50),
             q01_peak = quantile(indic_peak,0.01),
             q99_full = quantile(indic_full,0.99),
             q50_full = quantile(indic_full,0.50),
             q01_full = quantile(indic_full,0.01)),
           by = "cluster,ireg,iindic"]

  ss <- ss[!iindic %in% c(3,4,6,7,12:19,26:30,33)]
  ss <- ss[ireg %in% 1:6]

  ss[, ratio_full_peak_99 := (q99_full - q50_full) / (q99_peak - q50_peak)]
  ss[, ratio_full_peak_01 := (q01_full - q50_full) / (q01_peak - q50_peak)]

  ss[, impact := paste0(iindic_labels[iindic]," & ",iunit[iindic])]
  ss[, impact := str_replace(impact, "%","\\%")]
  ss[, region := ireg_labels[ireg]]

  ssx <- ss[cluster == clusters[1],
            .(ratio_full_peak_99 = mean(ratio_full_peak_99),
              ratio_full_peak_01 = mean(ratio_full_peak_01)),
            by = "iindic"]
  iimpact <- rev(c(ssx[ratio_full_peak_99<ratio_full_peak_01][order(-ratio_full_peak_01)][,iindic],
  ssx[ratio_full_peak_99>ratio_full_peak_01][order(ratio_full_peak_99)][,iindic]
  ))


  compute_coordx <- function(idx, up = TRUE) {
    xx <- as.numeric(str_split_fixed(idx," ",2))
    x0 <- xx[1]
    if (up) {
      x <- c(x0,x0,x0 + 1,x0)
    } else {
      x <- c(x0,x0 + 1,x0 + 1,x0)
    }
    return(x)
  }
  compute_coordy <- function(idx, up = TRUE) {
    xx <- as.numeric(str_split_fixed(idx," ",2))
    y0 <- which(xx[2]==iimpact)
    if (up) {
      y <- c(y0,y0 + 1,y0 + 1,y0)
    } else {
      y <- c(y0,y0,y0 + 1,y0)
    }
    return(y)
  }
  
  ss14 <- ss[cluster == clusters[1]]
  ss14 <- ss14[, c(2,3,10,11)]
  icode <- paste(ss14$ireg,ss14$iindic)
  
  values14 <- data.frame(
    id = icode,
    ii = ss14$iindic,
    jj = ss14$ireg,
    value1 = ss14$ratio_full_peak_01,
    value2 = ss14$ratio_full_peak_99
  )
  
  positions14 <- data.frame(
    id = rep(icode, each = 4),
    x1 = unlist(lapply(icode, compute_coordx)),
    y1 = unlist(lapply(icode, compute_coordy)),
    x2 = unlist(lapply(icode, compute_coordx, FALSE)),
    y2 = unlist(lapply(icode, compute_coordy, FALSE))
  )

  datapoly14 <- merge(values14, positions14, by = c("id"))
  
  ss2c <- ss[cluster == clusters[4]]
  ss2c <- ss2c[, c(2,3,10,11)]
  
  values2c <- data.frame(
    id = icode,
    ii = ss14$iindic,
    jj = ss14$ireg,
    value1 = ss2c$ratio_full_peak_01,
    value2 = ss2c$ratio_full_peak_99
  )
  
  positions2c <- data.frame(
    id = rep(icode, each = 4),
    x1 = unlist(lapply(icode, compute_coordx)),
    y1 = unlist(lapply(icode, compute_coordy)),
    x2 = unlist(lapply(icode, compute_coordx, FALSE)),
    y2 = unlist(lapply(icode, compute_coordy, FALSE))
  )
  
  datapoly2c <- merge(values2c, positions2c, by = c("id"))
  
  datapoly14$cluster = clusters[1]
  datapoly2c$cluster = clusters[4]
  
  datapoly <- rbind(datapoly14,datapoly2c)
  
  datapoly$cluster <- factor(datapoly$cluster, levels = clusters)
  
  p <- ggplot(datapoly) +
    geom_polygon(aes(x = x1, y = y1, fill = (value1), group = id)) +
    geom_polygon(aes(x = x2, y = y2, fill = (value2), group = id)) +
    geom_hline(yintercept = 14) +
    facet_wrap(~ cluster, strip.position = "bottom") +
    coord_equal() +
    scale_fill_gradient2(midpoint = 1, limits = (c(0.45,1.55)), 
                         name = "Tail ratio\nFull / Peak",
                         low = muted("green"), high = muted("red")) +
    scale_x_continuous(expand = c(0,0), 
                       breaks = 1:6 + 0.5, 
                       labels = ireg[1:6],
                       position = "top") +
    scale_y_continuous(expand = c(0,0), 
                       breaks = seq_along(iimpact) + 0.5, 
                       labels = iindic_labels_short[iimpact]) +
    labs(x = "", y = "") +
    theme_gray() +
    theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
    theme(legend.position = "right") +
    theme(strip.background = element_blank()) +
    theme(legend.title = element_text(size = 9)) + 
    theme(legend.box.margin = margin(25,0,0,0)) 
    
  p

  
  ggsave(outfile,width = 6, height = 6)
  
}

project_cc_impact_arnell_stat <- function(temp_magicc,
                                          gmt_hist,
                                          pair_scen_sel,
                                          proxy_impact,
                                          outfile) {
  
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
  temp <- dcast(temp, model + delay + cbudget + year + cluster ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  # Time horizon
  temp <- temp[year >= 2020 & year <= 2100]
  temp[, cluster := factor(cluster, levels = clusters)]
  temp[, cluster_id := as.numeric(cluster)]
  temp[, run_id := paste(model,delay,cbudget)]
  temp[, run_id := factor(run_id, levels = unique(run_id))]
  temp[, run_id := as.numeric(run_id)]
  
  
  impacts <- NULL
  
  iiter <- c( 1,2,5,8,9,10,11,20,21,22,23,24,25,31,32)
  jiter <- (1:6)
  kiter <- seq_along(ic5models)
  
  impacts <- NULL
  
  # Impact Indicator
  for (i in iiter) {
    
    # Region
    for (j in jiter) {
      
      # Climate model
      for (k in kiter) {
        
        print(paste(i,j,k,((which(i==iiter)-1)*length(kiter)*length(jiter)+(j-1)*length(kiter)+(k-1))*100/(length(iiter)*length(jiter)*length(kiter))))
        
        # Isolate damage function
        impfunc <- proxy_impact[impact == iindic[i] &
                                  region == ireg[j] &
                                  c5model %in% c("hist",ic5models[k])][order(dgmt)]
        stopifnot(nrow(impfunc) == 7)
        
        impact_full <- spline(x = impfunc$dgmt, 
                          y = impfunc$value, 
                          xout = temp$full, 
                          ties = min,
                          xmin = 0.61, xmax = 4)[['y']]
        impact_peak <- spline(x = impfunc$dgmt, 
                          y = impfunc$value, 
                          xout = temp$peak, 
                          xmin = 0.61, xmax = 4)[['y']]
        
        if (i %in% 21:25) {
          impact_full <- -impact_full
          impact_peak <- -impact_peak
        }
        
        # c("cluster","run","impact","region","year","cmodel","full","peak")
        dd <- matrix(c(temp$cluster_id,
                       temp$run_id,
                       rep(i, nrow(temp)),
                       rep(j, nrow(temp)),
                       rep(k, nrow(temp)),
                       as.numeric(temp$year),
                       impact_full,
                       impact_peak), 
                     nrow = nrow(temp), 
                     ncol = 8)

        impacts <- rbind(impacts,dd)
        
      }

    }

  }
  
  # Column names
  colnames(impacts) <- c("cluster","run","impact","region","cmodel","year","full","peak")
    
  # subset cluster 1
  impacts15C <- subset(impacts, impacts[,"cluster"] == 1)
    
  save(impacts15C,file = "results/impacts15C.RData")

  # subset cluster 2
  impacts16C <- subset(impacts, impacts[,"cluster"] == 2)
  
  save(impacts16C,file = "results/impacts16C.RData")
  
  # subset cluster 3
  impacts18C <- subset(impacts, impacts[,"cluster"] == 3)
  
  save(impacts18C,file = "results/impacts18C.RData")
  
  # subset cluster 4
  impacts2C <- subset(impacts, impacts[,"cluster"] == 4)
  
  save(impacts2C,file = "results/impacts2C.RData")
  
}

