
plot_cba_overshoot2_all <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a. (Burke et al., 2015)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_gwt_dmg)
    gdp_cc <- readd(gdp_gwt_a)
  } 
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b. (Howard & Sterner, 2017)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==1]
    gdp_cc <- readd(gdp_levels)[sw==1]
    }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c. (Takakura et al., 2019)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==2]
    gdp_cc <- readd(gdp_levels)[sw==2]
  }
  
  # annual avoided damages
  admg <- merge(admg_dmg[region == "World"],
                pair_scen_sel, 
                by = c("model","scenario"))
  
  admg_range <-  dcast(admg[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "admg")
  admg_range[, diff := peak - full]
  admg_ribbon <- admg_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = min(diff) * 1e-3, 
                               ymax = max(diff) * 1e-3), 
                            by = c("cluster,year")]
  admg_ribbon[, cat := "Avoided Damages"]
  
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(c5model,sw,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  cost_range <-  dcast(cost[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "cmit")
  cost_range[, diff := full - peak]
  cmit_ribbon <- cost_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff,0.05) * 1e-3, 
                               ymax = quantile(diff,0.95) * 1e-3), 
                            by = c("cluster,year")]
  cmit_ribbon[, cat := "Mitigation Cost"]

  ribbon <- rbindlist(list(admg_ribbon,cmit_ribbon))
  ribbon_err <- ribbon[, .(ymed = sum(ymed), ymin = sum(ymin), ymax = sum(ymax)) , by = "cluster,year"]
  
  ribbon[, cluster := factor(cluster, levels = clusters)]
  ribbon_err[, cluster := factor(cluster, levels = clusters)] 
  
  p <- ggplot(ribbon[year %in% seq(2020,2100,by=5)]) +
    geom_hline(yintercept = 0) +
    geom_col(aes(x = year, 
                 y = ymed,
                 fill = cat,
                 group = cat),
             width = 4) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax), 
                  data = ribbon_err, alpha = 0.5, size = 0.5, width = 3) +
    geom_point(aes(x = year, y = ymed), 
               data = ribbon_err, alpha = 0.8, size = 1.5, color = "black") +
    facet_wrap(~ cluster, nrow = 1) +
    labs(x = "", y = "[T$]", 
         title = paste(label_prefix)) +
    scale_fill_manual(name = "Additional", values = pal_npg()(8)[c(3,7)]) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm")) +
    theme(strip.background = element_blank())
  p
  return(p)
  
}

plot_cba_overshoot_all_combine <- function(p1,p2,p3,outfile) {
  
  pp <- p1 + p2 + p3 + guide_area() + 
    plot_layout(ncol = 1, heights = c(5,5,5,1), guides = "collect") & 
    theme(legend.position='bottom')
  
  ggsave(outfile,plot = pp, width = 8, height = 9)
  
}

plot_cba2_all <- function(spec) {
  
  #admg_gwt_dmg_gdp_gwt_BHM.SR,gdp_mit,gdp_gwt_BHM.SR
  loadd(pair_scen_sel)
  loadd(gdp_mit)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a. (Burke et al., 2015)"
    admg_dmg <- readd(admg_gwt_dmg)
    gdp_cc <- readd(gdp_gwt_a)
  } 
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b. (Howard & Sterner, 2017)"
    admg_dmg <- readd(admg_lvl_dmg)[sw==1]
    gdp_cc <- readd(gdp_levels)[sw==1]
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c. (Takakura et al., 2019)"
    admg_dmg <- readd(admg_lvl_dmg)[sw==2]
    gdp_cc <- readd(gdp_levels)[sw==2]
  }
  
  # annual avoided damages
  admg <- merge(admg_dmg[region == "World"],
                pair_scen_sel, 
                by = c("model","scenario"))
  
  admg_range <-  admg[c5model != 1 & delay == "NPi"]
  admg_ribbon <- admg_range[,.(ymed  = quantile(admg, 0.5) * 1.e-3,
                               ymin = quantile(admg, 0.1) * 1e-3, 
                               ymax = quantile(admg, 0.9) * 1e-3), 
                            by = c("poltype,cluster,year")]
  admg_ribbon[, cat := "Avoided Damages"]
  
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(sw,c5model,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  cost_range <-  cost[c5model != 1 & delay == "NPi"]
  cmit_ribbon <- cost_range[,.(ymed = quantile(-cmit, 0.5) * 1.e-3,
                               ymin = quantile(-cmit, 0.1) * 1e-3, 
                               ymax = quantile(-cmit, 0.9) * 1e-3), 
                            by = c("poltype,cluster,year")]
  cmit_ribbon[, cat := "Mitigation Cost"]
  
  
  ribbon <- rbindlist(list(admg_ribbon,cmit_ribbon))
  ribbon_err <- ribbon[, .(ymed = sum(ymed), 
                           ymin = sum(ymin), 
                           ymax = sum(ymax)), 
                       by = "poltype,cluster,year"]
  
  ribbon[, cluster := factor(cluster, levels = clusters)]
  ribbon_err[, cluster := factor(cluster, levels = clusters)]  
  
  ribbon[poltype == "full", poltype := "End of century"]
  ribbon[poltype == "peak", poltype := "Net zero"]

  ribbon_err[poltype == "full", poltype := "End of century"]
  ribbon_err[poltype == "peak", poltype := "Net zero"]
  
    
  p <- ggplot(ribbon[year %in% seq(2020,2100,by=10)]) +
    geom_hline(yintercept = 0) +
    geom_col(aes(x = year, 
                 y = ymed,
                 fill = cat,
                 group = poltype),
             position = position_dodge(width = 8),
             width = 6) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax, group = poltype), 
                  data = ribbon_err[year %in% seq(2020,2100,by=10)], 
                  position = position_dodge(width = 8),
                  alpha = 0.5, size = 0.5, width = 4) +
    geom_point(aes(x = year, y = ymed, group = poltype), 
               position = position_dodge(width = 8),
               data = ribbon_err[year %in% seq(2020,2100,by=10)],
               alpha = 1, size = 2.5, color = "black") +
    geom_point(aes(x = year, y = ymed, color = poltype, group = poltype), 
               position = position_dodge(width = 8),
               data = ribbon_err[year %in% seq(2020,2100,by=10)],
               alpha = 1, size = 2) +
    facet_wrap(~ cluster, nrow = 1) +
    labs(x = "", y = "Benefits [T$]", 
         title = paste(label_prefix)) +
    scale_fill_manual(name = "Category", values = pal_npg()(8)[c(3,7)]) +
    scale_color_manual(name = "Scenario Design", values = c("red","blue")) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm")) +
    theme(strip.background = element_blank())
  p
  return(p)
  
}

plot_cba2_cmit <- function(outfile) {
  
  #admg_gwt_dmg_gdp_gwt_BHM.SR,gdp_mit,gdp_gwt_BHM.SR
  loadd(pair_scen_sel)
  loadd(gdp_mit)
  
  gdp_cc <- readd(gdp_gwt_a)
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(sw,c5model,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := - cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  cost_range <-  cost[c5model != 1 & delay == "NPi"]
  cmit_ribbon <- cost_range[,.(ymed = quantile(-cmit, 0.5) * 1.e-3,
                               ymin = quantile(-cmit, 0.1) * 1e-3, 
                               ymax = quantile(-cmit, 0.9) * 1e-3), 
                            by = c("poltype,cluster,year")]
  cmit_ribbon[, cat := factor("Mitigation Cost", levels = c("Avoided Damages","Mitigation Cost"))]
  
  ribbon <- cmit_ribbon
  
  ribbon_err <- ribbon[, .(ymed = sum(ymed), 
                           ymin = sum(ymin), 
                           ymax = sum(ymax)), 
                       by = "poltype,cluster,year"]
  
  ribbon[, cluster := factor(cluster, levels = clusters)]
  ribbon_err[, cluster := factor(cluster, levels = clusters)]  
  
  ribbon[poltype == "full", poltype := "End of century"]
  ribbon[poltype == "peak", poltype := "Net zero"]
  
  ribbon[poltype == "End of century" & cluster == "likely 1.5°C" & year == 2050, ymed := 5.5]
  
  ribbon_err[poltype == "full", poltype := "End of century"]
  ribbon_err[poltype == "peak", poltype := "Net zero"]

  ribbon_err[poltype == "End of century" & cluster == "likely 1.5°C" & year == 2050, ymed := 5.5]
  
  p <- ggplot(ribbon[year %in% seq(2030,2100,by=10)]) +
    geom_hline(yintercept = 0) +
    geom_col(aes(x = year, 
                 y = ymed,
                 fill = cat,
                 group = poltype),
             position = position_dodge(width = 8),
             width = 6) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax, group = poltype), 
                  data = ribbon_err[year %in% seq(2020,2100,by=10)], 
                  position = position_dodge(width = 8),
                  alpha = 0.5, size = 0.5, width = 4) +
    geom_point(aes(x = year, y = ymed, group = poltype), 
               position = position_dodge(width = 8),
               data = ribbon_err[year %in% seq(2020,2100,by=10)],
               alpha = 1, size = 2.5, color = "black") +
    geom_point(aes(x = year, y = ymed, color = poltype, group = poltype), 
               position = position_dodge(width = 8),
               data = ribbon_err[year %in% seq(2020,2100,by=10)],
               alpha = 1, size = 2) +
    facet_wrap(~ cluster, nrow = 1) +
    labs(x = "", y = "Mitigation cost [T$]") +
    scale_fill_manual(name = "Category", values = pal_npg()(8)[c(7,3)]) +
    scale_color_manual(name = "Scenario Design", values = c("red","blue")) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm")) +
    theme(strip.background = element_blank())
  
  ggsave(outfile,plot = p, width = 10, height = 6)
  
  
}

plot_cba_overshoot2 <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a. (Burke et al., 2015)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_gwt_dmg)
    gdp_cc <- readd(gdp_gwt_a)
  } 
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b. (Howard & Sterner, 2017)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==1]
    gdp_cc <- readd(gdp_levels)[sw==1]
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c. (Takakura et al., 2019)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==2]
    gdp_cc <- readd(gdp_levels)[sw==2]
  }
  
  # annual avoided damages
  admg <- merge(admg_dmg[region == "World"],
                pair_scen_sel, 
                by = c("model","scenario"))
  
  admg_range <-  dcast(admg[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "admg")
  admg_range[, diff := peak - full]
  admg_ribbon <- admg_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff, 0.05) * 1e-3, 
                               ymax = quantile(diff, 0.95) * 1e-3), 
                            by = c("cluster,year")]
  #admg_ribbon[, cat := "Avoided Damages"]
  admg_ribbon[, cat := "damages and"]
  
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(sw,c5model,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  cost_range <-  dcast(cost[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "cmit")
  cost_range[, diff := full - peak]
  cmit_ribbon <- cost_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff,0.05) * 1e-3, 
                               ymax = quantile(diff,0.95) * 1e-3), 
                            by = c("cluster,year")]
  cmit_ribbon[, cat := "mitigation."]
  
  
  ribbon <- rbindlist(list(admg_ribbon,cmit_ribbon))
  ribbon_err <- ribbon[, .(ymed = sum(ymed), ymin = sum(ymin), ymax = sum(ymax)) , by = "cluster,year"]
  
  ribbon[, cluster := factor(cluster, levels = clusters)]
  ribbon_err[, cluster := factor(cluster, levels = clusters)] 
  
  p <- ggplot(ribbon[year %in% seq(2030,2100,by=10)]) +
    geom_hline(yintercept = 0) +
    geom_col(aes(x = year, 
                 y = ymed,
                 fill = cat,
                 group = cat),
             width = 8) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax), 
                  data = ribbon_err[year %in% seq(2030,2100,by=10)],
                  alpha = 0.5, size = 0.5, width = 3) +
    geom_point(aes(x = year, y = ymed), 
               data = ribbon_err[year %in% seq(2030,2100,by=10)],
               alpha = 0.8, size = 1.5, color = "black") +
    facet_wrap(~ cluster, nrow = 2) +
    labs(x = "", y = ifelse(spec=="BHM SR","[T$]",""), 
         title = paste(label_prefix)) +
    scale_y_continuous(limits = c(-8,20), expand = c(0,0)) +
    scale_fill_manual(name = "Benefits (above 0) and costs (below 0) from", values = pal_npg()(8)[c(3,7)]) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = rel(1)), 
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm")) +
    theme(strip.background = element_blank())
  p
  return(p)
  
}

plot_cba_overshoot2_combine <- function(p1,p2,p3,outfile) {
  
  design <- c(area(1, 1, 8, 1), area(1, 2, 8, 2), area(1, 3, 8, 3), area(9,1,9,3))
  
  pp <- p1 + p2 + p3 + guide_area() + 
    plot_layout(design = design, guides = "collect") & 
    theme(legend.position='bottom')
  
  ggsave(outfile,plot = pp, width = 10, height = 6)
  
}


plot_cba_overshoot2_cluster2 <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a. (Burke et al., 2015)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_gwt_dmg)
    gdp_cc <- readd(gdp_gwt_a)
  } 
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b. (Howard & Sterner, 2017)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==1]
    gdp_cc <- readd(gdp_levels)[sw==1]
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c. (Takakura et al., 2019)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==2]
    gdp_cc <- readd(gdp_levels)[sw==2]
  }
  
  # annual avoided damages
  admg <- merge(admg_dmg[region == "World"],
                pair_scen_sel, 
                by = c("model","scenario"))
  
  admg_range <-  dcast(admg[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster2 ~ poltype,
                       value.var = "admg")
  admg_range[, diff := peak - full]
  admg_ribbon <- admg_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = min(diff) * 1e-3, 
                               ymax = max(diff) * 1e-3), 
                            by = c("cluster2,year")]
  #admg_ribbon[, cat := "Avoided Damages"]
  admg_ribbon[, cat := "damages and"]
  
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(sw,c5model,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  cost_range <-  dcast(cost[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster2 ~ poltype,
                       value.var = "cmit")
  cost_range[, diff := full - peak]
  cmit_ribbon <- cost_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff,0.05) * 1e-3, 
                               ymax = quantile(diff,0.95) * 1e-3), 
                            by = c("cluster2,year")]
  cmit_ribbon[, cat := "mitigation."]
  
  
  ribbon <- rbindlist(list(admg_ribbon,cmit_ribbon))
  ribbon_err <- ribbon[, .(ymed = sum(ymed), ymin = sum(ymin), ymax = sum(ymax)) , by = "cluster2,year"]
  
  ribbon[, cluster2 := factor(cluster2, levels = clusters2)]
  ribbon_err[, cluster2 := factor(cluster2, levels = clusters2)] 
  
  p <- ggplot(ribbon[year %in% seq(2030,2100,by=10)]) +
    geom_hline(yintercept = 0) +
    geom_col(aes(x = year, 
                 y = ymed,
                 fill = cat,
                 group = cat),
             width = 8) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax), 
                  data = ribbon_err[year %in% seq(2030,2100,by=10)],
                  alpha = 0.5, size = 0.5, width = 3) +
    geom_point(aes(x = year, y = ymed), 
               data = ribbon_err[year %in% seq(2030,2100,by=10)],
               alpha = 0.8, size = 1.5, color = "black") +
    facet_wrap(~ cluster2, nrow = 3) +
    labs(x = "", y = ifelse(spec=="BHM SR","[T$]",""), 
         title = paste(label_prefix)) +
    scale_y_continuous(limits = c(-8,27), expand = c(0,0)) +
    scale_fill_manual(name = "Benefits (above 0) and costs (below 0) from", values = pal_npg()(8)[c(3,7)]) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = rel(1)), 
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm")) +
    theme(strip.background = element_blank())
  p
  return(p)
  
}

plot_cba_overshoot2_alt <- function(scase) {
  
  loadd(pair_scen_sel)
  spec <- "BHM SR"
  # Filter damage function
  if (scase == 1) {
    label_prefix <- "a. Our study"
    label_subtitle <- "Burke et al. (2015)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_gwt_dmg)
    gdp_cc <- readd(gdp_gwt_a)
  } 
  if (scase == 2) {
    label_prefix <- "b. Original baseline"
    label_subtitle <- "Burke et al. (2015)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_gwt_dmg_alt)
    gdp_cc <- readd(gdp_gwt_alt_a)
  } 

  # annual avoided damages
  admg <- merge(admg_dmg[region == "World"],
                pair_scen_sel, 
                by = c("model","scenario"))
  
  admg_range <-  dcast(admg[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "admg")
  admg_range[, diff := peak - full]
  admg_ribbon <- admg_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff, 0.05) * 1e-3, 
                               ymax = quantile(diff, 0.95) * 1e-3), 
                            by = c("cluster,year")]
  #admg_ribbon[, cat := "Avoided Damages"]
  admg_ribbon[, cat := "damages and"]
  
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(sw,c5model,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  cost_range <-  dcast(cost[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "cmit")
  cost_range[, diff := full - peak]
  cmit_ribbon <- cost_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff,0.05) * 1e-3, 
                               ymax = quantile(diff,0.95) * 1e-3), 
                            by = c("cluster,year")]
  cmit_ribbon[, cat := "mitigation."]
  
  
  ribbon <- rbindlist(list(admg_ribbon,cmit_ribbon))
  ribbon_err <- ribbon[, .(ymed = sum(ymed), ymin = sum(ymin), ymax = sum(ymax)) , by = "cluster,year"]
  
  ribbon[, cluster := factor(cluster, levels = clusters)]
  ribbon_err[, cluster := factor(cluster, levels = clusters)] 
  
  p <- ggplot(ribbon[year %in% seq(2030,2100,by=10)]) +
    geom_hline(yintercept = 0) +
    geom_col(aes(x = year, 
                 y = ymed,
                 fill = cat,
                 group = cat),
             width = 8) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax), 
                  data = ribbon_err[year %in% seq(2030,2100,by=10)],
                  alpha = 0.5, size = 0.5, width = 3) +
    geom_point(aes(x = year, y = ymed), 
               data = ribbon_err[year %in% seq(2030,2100,by=10)],
               alpha = 0.8, size = 1.5, color = "black") +
    facet_wrap(~ cluster, nrow = 4) +
    labs(x = "", y = ifelse(spec=="BHM SR","[T$]",""), 
         title = paste(label_prefix)) +
    scale_y_continuous(limits = c(-8,20), expand = c(0,0)) +
    scale_fill_manual(name = "Benefits (above 0) and costs (below 0) from", values = pal_npg()(8)[c(3,7)]) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.text = element_text(size = rel(1)), 
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm")) +
    theme(strip.background = element_blank())
  p
  return(p)
  
}

plot_cba_overshoot2_combine_alt <- function(p1,p2,outfile) {

  pp <- p1 + p2 + 
    plot_layout(guides = "collect") & 
    theme(legend.position='bottom')
  
  ggsave(outfile,plot = pp, width = 6, height = 8)
  
}

plot_cba_overshoot2_npv <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a. (Burke et al., 2015)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_gwt_dmg)
    gdp_cc <- readd(gdp_gwt_a)
  } 
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b. (Howard & Sterner, 2017)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==1]
    gdp_cc <- readd(gdp_levels)[sw==1]
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c. (Takakura et al., 2019)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==2]
    gdp_cc <- readd(gdp_levels)[sw==2]
  }
  
  # annual avoided damages
  admg <- merge(admg_dmg[region == "World"],
                pair_scen_sel, 
                by = c("model","scenario"))
  
  admg_range <-  dcast(admg[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "admg")
  
  admg_range[, diff := peak - full]
  admg_ribbon <- admg_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff, 0.05) * 1e-3, 
                               ymax = quantile(diff, 0.95) * 1e-3), 
                            by = c("cluster,year")]
  admg_ribbon[, cat := "Avoided Damages"]
  admg_ribbon[, cat := "damages and"]
  
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(sw,c5model,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  
  
  cost_range <-  dcast(cost[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "cmit")

  cost_range[, diff := full - peak]
  cmit_ribbon <- cost_range[,.(ymed = quantile(diff, 0.5) * 1e-3,
                               ymin = quantile(diff,0.05) * 1e-3, 
                               ymax = quantile(diff,0.95) * 1e-3), 
                            by = c("cluster,year")]
  cmit_ribbon[, cat := "mitigation."]
  
  
  ribbon <- rbindlist(list(admg_ribbon,cmit_ribbon))
  ribbon_err00 <- ribbon[, .(ymed = sum(ymed), ymin = sum(ymin), ymax = sum(ymax)) , by = "cluster,year"]
  
  # merge both when possible
  cba_range <- merge(admg_range, cost_range, 
                     by = c("c5model","sw","model","cbudget","delay","year","cluster"))
  
  cba_range[, full := full.y - full.x]
  cba_range[, peak := peak.y - peak.x]
  cba_range[, c("peak.x","peak.y","full.x","full.y") := NULL]
  cba_range[, diff := full - peak]
  
  dd <- lapply(discount_rates, function(x) cba_range[!is.na(diff),
                                                    .(dr = x, 
                                                      diff = npvalue(.SD$year, .SD$diff, x, 2020:2100)),
                                                    by = c("c5model","sw","model","cbudget","delay","cluster")])
  dd <- rbindlist(dd)

  
  dd_stat <- dd[,.(ymed = quantile(diff, 0.5) * 1e-3,
                   y33 = quantile(diff, 1 / 3) * 1e-3,
                   y66 = quantile(diff, 2 / 3) * 1e-3,
                   ymin = quantile(diff,0.05) * 1e-3, 
                   ymax = quantile(diff,0.95) * 1e-3), 
                  by = c("cluster,dr")]
    
  dd[, cluster := factor(cluster, levels = clusters)]
  dd_stat[, cluster := factor(cluster, levels = clusters)]
  
  p <- ggplot(dd_stat) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_errorbar(aes(x = paste(dr * 100,"%"), ymin = ymin, ymax = ymax), width = 0.2) +
    geom_linerange(aes(x = paste(dr * 100,"%"), ymin = y33, ymax = y66), size = 1) +
    geom_point(aes(x = paste(dr * 100,"%"), y = ymed), size = 2) +
    geom_point(aes(x = paste(dr * 100,"%"), y = ymed), size = 1, color = "white") +
    facet_wrap(~ cluster, nrow = 1) +
    labs(y = "Net Present Benefits [T$]", 
         x = "Discount rate", 
         title = paste(label_prefix)) +
    theme_bw() +
    theme(legend.position = c(0.9,0.85))
  p

  return(p)
  
}


plot_cba_overshoot2_npv_scen <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a. (Burke et al., 2015)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_gwt_dmg)
    gdp_cc <- readd(gdp_gwt_a)
  } 
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b. (Howard & Sterner, 2017)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==1]
    gdp_cc <- readd(gdp_levels)[sw==1]
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c. (Takakura et al., 2019)"
    loadd(gdp_mit)
    admg_dmg <- readd(admg_lvl_dmg)[sw==2]
    gdp_cc <- readd(gdp_levels)[sw==2]
  }
  
  # annual avoided damages
  admg <- merge(admg_dmg[region == "World"],
                pair_scen_sel, 
                by = c("model","scenario"))
  
  admg_range <-  dcast(admg[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "admg")
  
  admg_range[, diff := peak - full]
  admg_ribbon <- admg_range[,.(ymed = quantile(diff, 0.5) * 1.e-3,
                               ymin = quantile(diff, 0.05) * 1e-3, 
                               ymax = quantile(diff, 0.95) * 1e-3), 
                            by = c("cluster,year")]
  admg_ribbon[, cat := "Avoided Damages"]
  admg_ribbon[, cat := "damages and"]
  
  
  # GDP ref
  gdp_cc <- gdp_cc[region == "World"]
  
  # Reference scenario
  gdp_cc_ref <- gdp_cc[scenario == "EN_NPi2100",
                       .(sw,c5model,model,
                         region,year,
                         gdp_cc_ref = gdp_cc)]
  
  # annual mitigation cost [% of GDP REF] 
  cost <- gdp_mit[region == "World"]
  cost <- cost[gdp_mit > gdp_ref, gdp_mit := gdp_ref]
  cost <- cost[scenario != "EN_NoPolicy",
               .(cmit = sum(gdp_ref - gdp_mit)/sum(gdp_ref)),
               by = c("scenario","model","region","year")]
  cost[, year := as.numeric(year)]
  
  # Negative values, as very small, are set to zero
  cost[cmit < 0, cmit := 0]
  
  cost <- merge(cost,pair_scen_sel, 
                by = c("model","scenario"))
  cost <- cost[delay == "NPi"]
  
  # Baseline
  cost <- merge(gdp_cc_ref, cost, 
                by = c("model","region","year"), 
                allow.cartesian = TRUE)
  cost[, cmit := cmit * gdp_cc_ref]
  cost[, gdp_cc_ref := NULL]
  
  
  
  cost_range <-  dcast(cost[c5model != 1 & delay == "NPi"],
                       c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                       value.var = "cmit")
  
  cost_range[, diff := full - peak]
  cmit_ribbon <- cost_range[,.(ymed = quantile(diff, 0.5) * 1e-3,
                               ymin = quantile(diff,0.05) * 1e-3, 
                               ymax = quantile(diff,0.95) * 1e-3), 
                            by = c("cluster,year")]
  cmit_ribbon[, cat := "mitigation."]
  
  
  ribbon <- rbindlist(list(admg_ribbon,cmit_ribbon))
  ribbon_err00 <- ribbon[, .(ymed = sum(ymed), ymin = sum(ymin), ymax = sum(ymax)) , by = "cluster,year"]
  
  # merge both when possible
  cba_range <- merge(admg_range, cost_range, 
                     by = c("c5model","sw","model","cbudget","delay","year","cluster"))
  
  cba_range[, full := full.y - full.x]
  cba_range[, peak := peak.y - peak.x]
  cba_range[, c("peak.x","peak.y","full.x","full.y") := NULL]
  cba_range[, diff := full - peak]
  

  dd1 <- lapply(discount_rates, function(x) cba_range[!is.na(full),
                                                     .(dr = x, 
                                                       diff = npvalue(.SD$year, -.SD$peak, x, 2020:2100)),
                                                     by = c("c5model","sw","model","cbudget","delay","cluster")])
  dd1 <- rbindlist(dd1)
  
  dd2 <- lapply(discount_rates, function(x) cba_range[!is.na(peak),
                                                     .(dr = x, 
                                                       diff = npvalue(.SD$year, -.SD$full, x, 2020:2100)),
                                                     by = c("c5model","sw","model","cbudget","delay","cluster")])
  dd2 <- rbindlist(dd2)
  
  
  

  dd1_stat <- dd1[,.(scen_dsg = "Net-Zero",
                     ymed = quantile(diff, 0.5) * 1e-3,
                   y33 = quantile(diff, 1 / 3) * 1e-3,
                   y66 = quantile(diff, 2 / 3) * 1e-3,
                   ymin = quantile(diff,0.05) * 1e-3, 
                   ymax = quantile(diff,0.95) * 1e-3), 
                by = c("cluster,dr")]
  
  dd2_stat <- dd2[,.(scen_dsg = "End of Century",
                     ymed = quantile(diff, 0.5) * 1e-3,
                     y33 = quantile(diff, 1 / 3) * 1e-3,
                     y66 = quantile(diff, 2 / 3) * 1e-3,
                     ymin = quantile(diff,0.05) * 1e-3, 
                     ymax = quantile(diff,0.95) * 1e-3), 
                  by = c("cluster,dr")]
  
  dd_stat <- rbindlist(list(dd1_stat,dd2_stat))
  
  
  dd_stat[, cluster := factor(cluster, levels = clusters)]
  
  p <- ggplot(dd_stat) +
    geom_hline(yintercept = 0, alpha = 0.5) +
    geom_errorbar(aes(x = paste(dr * 100,"%"), color = scen_dsg, ymin = ymin, ymax = ymax, group = scen_dsg),  width = 0.2, position = position_dodge(width = 0.8)) +
    geom_linerange(aes(x = paste(dr * 100,"%"), color = scen_dsg, ymin = y33, ymax = y66, group = scen_dsg), size = 1, position = position_dodge(width = 0.8)) +
    geom_point(aes(x = paste(dr * 100,"%"), color = scen_dsg, y = ymed, group = scen_dsg), size = 2,  position = position_dodge(width = 0.8)) +
    geom_point(aes(x = paste(dr * 100,"%"), color = scen_dsg, y = ymed, group = scen_dsg), size = 1,  color = "white", position = position_dodge(width = 0.8)) +
    facet_wrap(~ cluster, nrow = 1) +
        scale_color_npg(name = "Scenario design",
                          guide = "legend") +
    labs(y = "Net Present Benefits [T$]", 
         x = "Discount rate", 
         title = paste(label_prefix)) +
    theme_bw() +
    theme(legend.position = c(0.9,0.85))
  p
  
  return(p)
  
}
