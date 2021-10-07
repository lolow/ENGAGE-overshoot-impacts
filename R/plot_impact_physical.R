

plot_list_diff_global_impact_physical <- function() {
  
  loadd(impact_indic_linear, 
        impact_indic_spline)
  ireg00 <- 1
  
  dd1 <- impact_indic_linear[ireg == ireg00, 
                             .(iindic, cluster,c5model,model,cbudget,
                               indic_full_linear = indic_full,
                               indic_peak_linear = indic_peak)]
  dd2 <- impact_indic_spline[ireg == ireg00, 
                             .(iindic, cluster,c5model,model,cbudget,
                               indic_full_spline = indic_full,
                               indic_peak_spline = indic_peak)]
  
  dd <- merge(dd1,dd2,by = c("iindic","cluster","c5model","model","cbudget"))
  
  dd[, indic_full := indic_full_linear - indic_full_spline]
  dd[, indic_peak := indic_peak_linear - indic_peak_spline]
  
  dd <- rbindlist(list(dd[,.(iindic, cluster, c5model, model, cbudget, indic = indic_full, poltype = "full")],
                  dd[,.(iindic, cluster, c5model, model, cbudget, indic = indic_peak, poltype = "peak")]))
  
  dd[, cluster := factor(cluster, levels = clusters)]
  
  dd[poltype == "full", poltype := "End of Century"]
  dd[poltype == "peak", poltype := "Net Zero"]
  
  plot_istat = function (i) {
    
    if (i %in% iindic_max) {
      criteria <- "Difference in maximum over 2020-2100"
    }
    
    if (i %in% iindic_min) {
      criteria <- "Difference in minimum over 2020-2100"
    }
    
    
    ylabel_prefix <- "Net reduction in"
    
    ggplot(dd[iindic == i], 
           aes(x = cluster)) +
      geom_violin(aes(y = indic, fill = poltype),
                  draw_quantiles = c(0.5),
                  position = position_dodge(width=0.5)) +
      theme_bw() +
      scale_fill_npg(name = "Scenario design") +
      theme(legend.position = "right") +
      labs(x = "Temperature increase in 2100",
           y = "",#paste(iunit[i]),
           title = name_indic(i),
           subtitle = criteria) +
      theme(axis.title.y = element_blank())
  }
  
  pp <- lapply(seq_along(iindic), plot_istat)
  
  return(pp)
  
}

plot_comp_impact_physical <- function(ireg00 = 1) {
  
  loadd(pair_scen_sel)
  istat <- readd(impact_samp)
  
  plot_istat = function (i) {
    
    if (i %in% iindic_max) {
      criteria <- "Maximum over 2020-2100"
    }
    
    if (i %in% iindic_min) {
      criteria <- "Minimum over 2020-2100"
    }
    
    if (i %in% iindic_sum) {
      criteria <- "Cumulative over 2020-2100"
    }

    
    istat[, cluster := factor(cluster, levels = clusters)]
    
    istat[poltype == "full", poltype := "End of Century"]
    istat[poltype == "peak", poltype := "Net Zero"]
    
    dd <- dcast(istat[iindic == iindic2[i] & ireg == ireg[ireg00]], cluster + poltype ~ statistic)
    
    ggplot(dd, 
           aes(x = cluster)) +
      geom_errorbar(aes(ymin = p05, ymax = p95, color = poltype), 
                    width = .2, position = position_dodge(width=0.5)) +
      geom_linerange(aes(ymin = p10, ymax = p90, color = poltype),
                     size = 1, position = position_dodge(width=0.5)) +
      geom_linerange(aes(ymin = p33, ymax = p67, color = poltype),
                     size = 2, position = position_dodge(width=0.5)) +
      geom_point(aes(y = p50, color = poltype), 
                 size = 3.5, position = position_dodge(width=0.5)) +
      geom_point(aes(y = p50, group = poltype), 
                 size = 3, position = position_dodge(width=0.5), color = "white") +
      theme_bw() +
      scale_color_npg(name = "Scenario design") +
      theme(legend.position = "right") +
      labs(x = "Temperature increase in 2100",
           y = "",#paste(iunit[i]),
           title = name_indic(i),
           subtitle = criteria) +
      theme(axis.title.y = element_blank())
  }
  
  pp <- lapply(1:33, plot_istat)
  
  return(pp)
  
}

plot_all_global_impact_physical <- function(pp, outfile) {

  p <- pp[[11]] + pp[[20]] + pp[[10]] +
       pp[[05]] + pp[[09]] + pp[[08]] +
       pp[[21]] + pp[[23]] + pp[[25]] + 
       pp[[24]] + pp[[22]] + patchwork::guide_area() +
       pp[[31]] + pp[[32]] + plot_spacer() +  
       pp[[02]] + pp[[01]] + plot_spacer() +
    plot_layout(ncol = 3, nrow = 6, guides = 'collect')
  
  ggsave(outfile, plot = p, width = 12, height = 14)
  
}

plot_comp_impact_physical2 <- function(ireg00 = 1) {
  
  loadd(pair_scen_sel)
  istat <- readd(impact_samp2)
  
  plot_istat = function (i) {
    
    if (i %in% iindic_max) {
      criteria <- "Maximum over 2020-2100"
    }
    
    if (i %in% iindic_min) {
      criteria <- "Minimum over 2020-2100"
    }
    
    if (i %in% iindic_sum) {
      criteria <- "Cumulative over 2020-2100"
    }
    
    
    istat[, cluster := factor(cluster, levels = clusters2)]
    
    istat[poltype == "full", poltype := "End of Century"]
    istat[poltype == "peak", poltype := "Net Zero"]
    
    dd <- dcast(istat[iindic == iindic2[i] & ireg == ireg[ireg00]], cluster + poltype ~ statistic)
    
    ggplot(dd, 
           aes(x = cluster)) +
      geom_errorbar(aes(ymin = p05, ymax = p95, color = poltype), 
                    width = .2, position = position_dodge(width=0.5)) +
      geom_linerange(aes(ymin = p10, ymax = p90, color = poltype),
                     size = 1, position = position_dodge(width=0.5)) +
      geom_linerange(aes(ymin = p33, ymax = p67, color = poltype),
                     size = 2, position = position_dodge(width=0.5)) +
      geom_point(aes(y = p50, color = poltype), 
                 size = 3.5, position = position_dodge(width=0.5)) +
      geom_point(aes(y = p50, group = poltype), 
                 size = 3, position = position_dodge(width=0.5), color = "white") +
      theme_bw() +
      scale_color_npg(name = "Scenario design") +
      theme(legend.position = "right") +
      labs(x = "Temperature increase in 2100",
           y = "",#paste(iunit[i]),
           title = name_indic(i),
           subtitle = criteria) +
      theme(axis.title.y = element_blank())
  }
  
  pp <- lapply(1:33, plot_istat)
  
  return(pp)
  
}


map_impact_heatwave <- function(outfile) {
  
  loadd(impact_samp_map,pair_scen_sel)
  
  iindic_ids <- c("Major heatwave frequency",
                  "Heatwave frequency",
                  "Heatwave duration")
  
  SD <- impact_samp_map[iindic %in% iindic_ids & 
                       cluster %in% clusters[c(1,4)]]
  SD[, iindic := factor(iindic, levels = iindic_ids)]

  SD_peak <- SD[poltype == "peak" & statistic == "p50"]
  SD_full <- SD[poltype == "full" & statistic == "p50"]
  
  # cluster 1 - indic 1 
  lim0 <- min(SD_peak[iindic == iindic_ids[1], value])
  lim1 <- max(SD_peak[iindic == iindic_ids[1], value])
  
  world_data <- SD_peak[cluster == clusters[1] & iindic == iindic_ids[1],
                    .(region = ireg, cluster, iindic, value)]
  world_data <- merge(world_data,arnell_region_iso3,by = "region")
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI")) 
  
  world <- merge(world,world_data, by.x = "adm0_a3", by.y = "iso3")
  
  target_crs <- '+proj=eqearth +wktext'
  world0 <- st_transform(world, crs = target_crs)

  p1 <- ggplot(data = world0) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_b(option = "plasma",
                         name = paste0("Major heatwave\nfrequency [%]"),
                         limits = c(lim0,lim1)) +
    theme(legend.position = "right") + 
    theme(legend.key.width = unit(0.8,"cm")) +
    labs(title = "likely 1.5\u00B0C") +
    theme(plot.title = element_text(size = 12, hjust = 0.5, margin = margin(20,20,20,20)))
  
  # cluster 6 - indic 1
  
  world_data <- SD_peak[cluster == clusters[4] & iindic == iindic_ids[1],
                        .(region = ireg, cluster, iindic, value)]
  world_data <- merge(world_data,arnell_region_iso3,by = "region")
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI")) 
  
  world <- merge(world,world_data, by.x = "adm0_a3", by.y = "iso3")
  
  target_crs <- '+proj=eqearth +wktext'
  world0 <- st_transform(world, crs = target_crs)

  p2 <- ggplot(data = world0) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_b(option = "plasma",
                         name = paste0("Major heatwave\nfrequency [%]"),
                         limits = c(lim0,lim1)) +
    theme(legend.position = "right") + 
    theme(legend.key.width = unit(0.8,"cm")) +
    labs(title = "below 2\u00B0C") +
    theme(plot.title = element_text(size = 12, hjust = 0.5, margin = margin(20,20,20,20)))
  
  pp1 <- p1 + p2 + plot_layout(ncol = 2, guides = 'collect')
  
  # cluster 1 - indic 2 
  lim0 <- min(SD_peak[iindic == iindic_ids[2], value])
  lim1 <- max(SD_peak[iindic == iindic_ids[2], value])
  
  world_data <- SD_peak[cluster == clusters[1] & iindic == iindic_ids[2],
                        .(region = ireg, cluster, iindic, value)]
  world_data <- merge(world_data,arnell_region_iso3,by = "region")
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI")) 
  
  world <- merge(world,world_data, by.x = "adm0_a3", by.y = "iso3")
  
  target_crs <- '+proj=eqearth +wktext'
  world0 <- st_transform(world, crs = target_crs)
  
  p3 <- ggplot(data = world0) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_b(option = "plasma",
                         name = paste0("Heatwave\nfrequency [%]"),
                         limits = c(lim0,lim1)) +
    theme(legend.position = "right") + 
    theme(legend.key.width = unit(0.8,"cm"))
  
  # cluster 6 - indic 2
  world_data <- SD_peak[cluster == clusters[4] & iindic == iindic_ids[2],
                        .(region = ireg, cluster, iindic, value)]
  world_data <- merge(world_data,arnell_region_iso3,by = "region")
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI")) 
  
  world <- merge(world,world_data, by.x = "adm0_a3", by.y = "iso3")
  
  target_crs <- '+proj=eqearth +wktext'
  world0 <- st_transform(world, crs = target_crs)
  
  p4 <- ggplot(data = world0) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_b(option = "plasma",
                         name = paste0("Heatwave\nfrequency [%]"),
                         limits = c(lim0,lim1)) +
    theme(legend.position = "right") + 
    theme(legend.key.width = unit(0.8,"cm")) 
  
  pp2 <- p3 + p4 + plot_layout(ncol = 2, guides = 'collect') 
 
  # cluster 1 - indic 3 
  lim0 <- min(SD_peak[iindic == iindic_ids[3], value])
  lim1 <- max(SD_peak[iindic == iindic_ids[3], value])
  
  world_data <- SD_peak[cluster == clusters[1] & iindic == iindic_ids[3],
                        .(region = ireg, cluster, iindic, value)]
  world_data <- merge(world_data,arnell_region_iso3,by = "region")
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI")) 
  
  world <- merge(world,world_data, by.x = "adm0_a3", by.y = "iso3")
  
  target_crs <- '+proj=eqearth +wktext'
  world0 <- st_transform(world, crs = target_crs)
  
  p5 <- ggplot(data = world0) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_b(option = "plasma",
                         name = paste0("Heatwave\nduration [days]"),
                         limits = c(lim0,lim1)) +
    theme(legend.position = "right") + 
    theme(legend.key.width = unit(0.8,"cm"))
  
  # cluster 6 - indic 3
  world_data <- SD_peak[cluster == clusters[4] & iindic == iindic_ids[3],
                        .(region = ireg, cluster, iindic, value)]
  world_data <- merge(world_data,arnell_region_iso3,by = "region")
  
  world <- ne_countries(scale = "small", returnclass = "sf")
  world <- subset(world,!adm0_a3 %in% c("ATA","FJI")) 
  
  world <- merge(world,world_data, by.x = "adm0_a3", by.y = "iso3")
  
  target_crs <- '+proj=eqearth +wktext'
  world0 <- st_transform(world, crs = target_crs)
  
  p6 <- ggplot(data = world0) +
    geom_sf(aes(fill = value)) +
    coord_sf(datum = target_crs,
             expand = FALSE,
             clip = "off") +
    theme_void() +
    scale_fill_viridis_b(option = "plasma",
                         name = paste0("Heatwave\nduration [days]"),
                         limits = c(lim0,lim1)) +
    theme(legend.position = "right") + 
    theme(legend.key.width = unit(0.8,"cm")) 
  
  pp3 <- p5 + p6 + plot_layout(ncol = 2, guides = 'collect') 
  
  pp <- pp1 / pp2 / pp3
  
  ggsave(outfile, plot = pp, width = 7, height = 7)
  
  return(pp)
  
}

name_indic <- function(i) {
  n <- paste0(iindic_labels[i]," ",iunit[i])
  n <- str_replace(n, ":", ":\n")
  n <- ifelse(str_length(n) > 35 & !str_detect(n,"\\n"), str_replace(n," \\[","\n \\["), n)
  return(n)
}

plot_impact_info_1 <- function(rr, outfile) {
  
  ### MAX IMPACT
  impact_test <- NULL
  for (i in iindic_ks) {
    impact_indic <- readd(paste0("impact_ks_",i,"_",rr,"L"), character_only = TRUE)
    # Add infos
    impact_indic[, iindic.label := iindic[i]]
    impact_indic[, iindic := i]

    # Compute Two-sample Kolmogorov-Smirnov test
    alter <- ifelse(i %in% iindic_min, "less", "greater")
    res <- setDT(glance(ks.test(impact_indic[poltype == "peak", value],
                                impact_indic[poltype == "full", value],
                                alternative = alter)))
    res[, iindic.label := iindic[i]]
    res[, iindic := i]
    impact_test <- rbindlist(list(impact_test,res))
  }
  
  iindic_order <- impact_test[order(-statistic)][,iindic]
  
  SD <- NULL
  for (i in iindic_ks) {
    dd <- readd(paste0("impact_ks_",i,"_",rr,"L"), character_only = TRUE)
    dd[, iindic.label := name_indic(i)]
    dd[, iindic := i]
    SD <- rbindlist(list(SD,dd))
  }
  SD[, iindic := factor(iindic, levels = iindic_order)]
  SD[, iindic.label := factor(iindic.label, levels = name_indic(iindic_order))]
  
  SD0 <- SD[, .(indic_med = quantile(value, 0.5)), 
            by = c("iindic.label","iindic","poltype")]
  
  p <- ggplot(SD) + 
    geom_density(aes(x = value, fill = poltype), alpha = 0.5) + 
    geom_vline(aes(color = poltype, xintercept = indic_med), 
               data = SD0, linetype="dashed", size = 1) +
    facet_wrap(~ iindic.label, ncol = 3, scales = "free") +
    scale_fill_npg(name = "Scenario design") +
    scale_color_npg(guide = FALSE) +
#    coord_cartesian(xlim =  c(quantile(SD$value,0.01),quantile(SD$value,0.99))) +
    labs(title = paste0(ireg[rr]," - likely 1.5\u00B0C scenarios"), 
         x = "", y = "") +
    theme_bw() +
    theme(legend.position = "bottom") +
    theme(axis.text.y = element_blank()) +
    theme(axis.title = element_blank()) +
    theme(legend.margin = margin(t = 0, unit='cm'))
  
  ggsave(filename = outfile, plot = p, width = 9, height = 12)
  
}

plot_impact_info_2 <- function(outfile) {

  iindic_ids <- iindic_hw
  
  rr <- 1
  
  SD <- NULL
  for (i in iindic_ids) {
    dd <- readd(paste0("impact_ks_",i,"_",rr,"L"), character_only = TRUE)
    dd[, iindic.label := name_indic(i)]
    dd[, iindic := i]
    SD <- rbindlist(list(SD,dd))
    for (j in 2:4) {
      dd <- readd(paste0("impact_hw_",i,"L_",j,"L"), character_only = TRUE)
      dd[, iindic.label := name_indic(i)]
      dd[, iindic := i]
      SD <- rbindlist(list(SD,dd))
    }
  }
  SD[, iindic := factor(iindic, levels = iindic_ids)]
  SD[, iindic.label := factor(iindic.label, levels = name_indic(iindic_ids))]
  SD[, cluster := factor(cluster, levels = clusters)]
  
  SD0 <- SD[, .(indic_med = quantile(value, 0.5)), 
            by = c("cluster","iindic.label","iindic","poltype")]
  
  p1 <- ggplot(SD[iindic == which(iindic_labels=="Major heatwave frequency")]) + 
    geom_density(aes(x = value, fill = poltype, 
                     group = paste(poltype,poltype)), 
                 alpha = 0.5) + 
    geom_vline(aes(color = poltype, xintercept = indic_med, 
                   group = paste(poltype,poltype)), 
               data = SD0[iindic.label == name_indic(which(iindic_labels=="Major heatwave frequency"))], 
               linetype="dashed", size = 1) +
    facet_grid( ~ cluster) +
    scale_fill_npg(name = "") +
    scale_color_npg(guide = FALSE) +
    labs(subtitle = name_indic(iindic_ids)[1], x = "", y = "") +
    theme_bw() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.placement = "outside")
  
  p2 <- ggplot(SD[iindic == which(iindic_labels=="Heatwave frequency")]) + 
    geom_density(aes(x = value, fill = poltype, 
                     group = paste(poltype,poltype)), 
                 alpha = 0.5) + 
    geom_vline(aes(color = poltype, xintercept = indic_med, 
                   group = paste(poltype,poltype)), 
               data = SD0[iindic.label == name_indic(which(iindic_labels=="Heatwave frequency"))], 
               linetype="dashed", size = 1) +
    facet_grid( ~ cluster) +
    scale_fill_npg(name = "") +
    scale_color_npg(guide = FALSE) +
    labs(subtitle = name_indic(iindic_ids)[2], x = "", y = "") +
    theme_bw() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.placement = "outside")
  
  p3 <- ggplot(SD[iindic == which(iindic_labels=="Heatwave duration")]) + 
    geom_density(aes(x = value, fill = poltype, 
                     group = paste(poltype,poltype)), 
                 alpha = 0.5) + 
    geom_vline(aes(color = poltype, xintercept = indic_med, 
                   group = paste(poltype,poltype)), 
               data = SD0[iindic.label == name_indic(which(iindic_labels=="Heatwave duration"))], 
               linetype="dashed", size = 1) +
    facet_grid( ~ cluster) +
    scale_fill_npg(name = "") +
    scale_color_npg(guide = FALSE) +
    coord_cartesian(xlim = c(0,30)) +
    labs(subtitle = name_indic(iindic_ids)[3],x = "", y = "") +
    theme_bw() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.placement = "outside")
  
  pp = p1 + p2 + p3 + plot_layout(ncol = 1)
  
  ggsave(outfile, plot = pp, width = 8, height = 8)
  
}

plot_impact_info_3 <- function(outfile) {
  
  iindic_ids <- iindic_cr
  
  rr <- 1
  
  SD <- NULL
  for (i in iindic_ids) {
    dd <- readd(paste0("impact_ks_",i,"_",rr,"L"), character_only = TRUE)
    dd[, iindic.label := name_indic(i)]
    dd[, iindic := i]
    SD <- rbindlist(list(SD,dd))
    for (j in 2:4) {
      dd <- readd(paste0("impact_cr_",i,"L_",j,"L"), character_only = TRUE)
      dd[, iindic.label := name_indic(i)]
      dd[, iindic := i]
      SD <- rbindlist(list(SD,dd))
    }
  }
  SD[, iindic := factor(iindic, levels = iindic_ids)]
  SD[, iindic.label := factor(iindic.label, levels = name_indic(iindic_ids))]
  SD[, cluster := factor(cluster, levels = clusters)]
  
  SD0 <- SD[, .(indic_med = quantile(value, 0.5)), 
            by = c("cluster","iindic.label","iindic","poltype")]
  
  p1 <- ggplot(SD[iindic == iindic_ids[1]]) + 
    geom_density(aes(x = value, fill = poltype, 
                     group = paste(poltype,poltype)), 
                 alpha = 0.5) + 
    geom_vline(aes(color = poltype, xintercept = indic_med, 
                   group = paste(poltype,poltype)), 
               data = SD0[iindic.label == name_indic(iindic_ids[1])], 
               linetype="dashed", size = 1) +
    facet_grid( ~ cluster) +
    scale_fill_npg(name = "") +
    scale_color_npg(guide = FALSE) +
    labs(subtitle = name_indic(iindic_ids)[1], x = "", y = "") +
    theme_bw() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.placement = "outside")
  
  p2 <- ggplot(SD[iindic == iindic_ids[2]]) + 
    geom_density(aes(x = value, fill = poltype, 
                     group = paste(poltype,poltype)), 
                 alpha = 0.5) + 
    geom_vline(aes(color = poltype, xintercept = indic_med, 
                   group = paste(poltype,poltype)), 
               data = SD0[iindic.label == name_indic(iindic_ids[2])], 
               linetype="dashed", size = 1) +
    facet_grid( ~ cluster) +
    scale_fill_npg(name = "") +
    scale_color_npg(guide = FALSE) +
    labs(subtitle = name_indic(iindic_ids)[2], x = "", y = "") +
    theme_bw() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.placement = "outside")
  
  p3 <- ggplot(SD[iindic == iindic_ids[3]]) + 
    geom_density(aes(x = value, fill = poltype, 
                     group = paste(poltype,poltype)), 
                 alpha = 0.5) + 
    geom_vline(aes(color = poltype, xintercept = indic_med, 
                   group = paste(poltype,poltype)), 
               data = SD0[iindic.label == name_indic(iindic_ids[3])], 
               linetype="dashed", size = 1) +
    facet_grid( ~ cluster) +
    scale_fill_npg(name = "") +
    scale_color_npg(guide = FALSE) +
    labs(subtitle = name_indic(iindic_ids)[3],x = "", y = "") +
    theme_bw() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.placement = "outside")
  
  pp = p1 + p2 + p3 + plot_layout(ncol = 1)
  
  ggsave(outfile, plot = pp, width = 8, height = 8)
  
}

matrix_significant_days <- function(outfile) {
  
  nbyears_50 <- readd(impact_exc_reject_50)
  nbyears_95 <- readd(impact_exc_reject_95)
  
  nbyears_50[, cluster := clusters[cluster]]
  nbyears_50[, impacts := iindic_labels[iindic]]
  nbyears_50[, ilab := iindic_labels_short[iindic]]
  nbyears_50[, region := iregg[ireg]]
  nbyears_50 <- nbyears_50[, .(cluster,region,impacts,ilab,impact_rejection)]

  nbyears_95[, cluster := clusters[cluster]]
  nbyears_95[, impacts := iindic_labels[iindic]]
  nbyears_95[, ilab := iindic_labels_short[iindic]]
  nbyears_95[, region := iregg[ireg]]
  nbyears_95 <- nbyears_95[, .(cluster,region,impacts,ilab,impact_rejection)]
    
  # Remove HDD and Frost Days from plot
  nbyears_50 <- nbyears_50[impacts != "Heating degree days"]
  nbyears_50 <- nbyears_50[impacts != "Frost days"]

  nbyears_95 <- nbyears_95[impacts != "Heating degree days"]
  nbyears_95 <- nbyears_95[impacts != "Frost days"]
  
  iorder <- iindic_labels_short[c(10, 11, 20, 5, 21:25, 1, 2, 31, 32)]
  
  nbyears_50_long <- nbyears_50
  nbyears_50_long[region == "North America", region := "N. America"]
  nbyears_50_long[region == "South America", region := "S. America"]
  nbyears_50_long[,cluster := factor(cluster,levels = clusters)]
  nbyears_50_long[,region := factor(region,levels = c("Globe","Africa","Europe","N. America","S. America","Asia"))]
  nbyears_50_long[,ilab := factor(ilab,levels = rev(iorder))]

  nbyears_95_long <- nbyears_95
  nbyears_95_long[region == "North America", region := "N. America"]
  nbyears_95_long[region == "South America", region := "S. America"]
  nbyears_95_long[,cluster := factor(cluster,levels = clusters)]
  nbyears_95_long[,region := factor(region,levels = c("Globe","Africa","Europe","N. America","S. America","Asia"))]
  nbyears_95_long[,ilab := factor(ilab,levels = rev(iorder))]
  
  
  p <- ggplot(nbyears_95_long) +
    geom_tile(aes(x = region, y = ilab, fill = impact_rejection)) +
    #facet_wrap(~ cluster, strip.position = "bottom", nrow = 1) +
    facet_wrap(~ cluster, nrow = 1) +
    coord_equal() +
    scale_fill_viridis_c(name = "Years\n(EOC > NZ)") +
    #scale_x_discrete(position = "top") +
    scale_x_discrete() +
    labs(x = "", y = "") +
    theme_gray() +
    #theme(axis.text.x = element_text(angle = 45, hjust = 0)) +
    theme(axis.text.x = element_text(angle = - 45 , hjust = 0)) +
    theme(legend.position = "right") +
    theme(strip.background = element_blank()) +
    theme(legend.title = element_text(size = 9)) + 
    theme(legend.box.margin = margin(25,0,0,0)) 

  ggsave(outfile, plot = p, width = 8, height = 5)
  
}

compare_max_dist_heatwave <- function(outfile) {
  
  impact_indic_stat <- readd(impact_samp)
  
  impact_indic <- rbindlist(list(readd(impact_10_1),readd(impact_10_4)))
  setnames(impact_indic,"iindic","iindic_label")
  impact_indic <- merge(impact_indic[],
        data.table(iindic=1:length(iindic), iindic_label=iindic), by = "iindic_label")
  impact_indic[,iindic_label := NULL]
  
  # add infos
  iindic_labels <- iindic
  impact_indic[, iindic.label := name_indic(iindic)]
  impact_indic <- impact_indic[!iindic %in% c(3,4,6,7,12:19,26:30,33)]
  
  # Focus on heatwave duration
  rr <- ireg[1]
  iindic_ids <- c(which(iindic=="Heatwave duration"))
  
  SD <- impact_indic[ireg == rr & 
                       iindic %in% iindic_ids & 
                       cluster %in% clusters[c(1,4)]]
  SD[, iindic := factor(iindic, levels = iindic_ids)]
  SD[, iindic.label := factor(iindic.label, levels = name_indic(iindic_ids))]
  SD[, cluster := factor(cluster, levels = clusters)]  
  
  SD0 <- impact_indic_stat[cluster %in% clusters[c(1,4)] & 
                             iindic == "Heatwave duration" & 
                             ireg == "Globe" & statistic == "p50"]
  
  
  p <- ggplot() + 
    geom_density(aes(x = value, fill = "Net zero", linetype = cluster),
                 data = SD[poltype == "peak"],
                 alpha = 0.5) + 
    geom_density(aes(x = value, fill = "End of century", linetype = cluster), 
                 data = SD[poltype == "full"],
                 alpha = 0.5) + 
    geom_vline(aes(color = "Net zero", xintercept = value, linetype = cluster, 
                   group = cluster), 
               data = SD0[poltype == "peak"], size = 0.5, alpha = 0.75) +
    geom_vline(aes(color = "End of century", xintercept = value, linetype = cluster, 
                   group = cluster), 
               data = SD0[poltype == "full"], size = 0.5, alpha = 0.75) +
    scale_fill_npg(name = "Scenario\ndesign") +
    scale_color_npg(guide = FALSE, name = "Cluster") +
    scale_x_continuous(limits = c(0,30)) + 
    labs(x = "days", y = "") +
    theme_bw() +
    theme(legend.position = "right",
          strip.background = element_blank(),
          strip.placement = "outside",
          axis.text.y = element_blank())
  
  ggsave(outfile, plot = p, width = 5, height = 3)
  
}

plot_impact_arnell_function <- function(outfile,interp = 0) {
  
  loadd(proxy_impact)
  
  # proxy_impact[!impact %in% iindic[c(3,4,6,7,12:19,26:30,33)]]
  sel_impacts <- c("Heatwave duration",
                   "Heatwave frequency",
                   "Reduction in crop duration: Maize",
                   "Agricultural drought (SPEI) frequency",
                   "Runoff decreases")
  sel_regions <- c('Globe', 'Africa', 'Europe', 'North America', 'South America',
                   'Asia')
  data_plot <- proxy_impact[impact %in% sel_impacts & 
                              region %in% sel_regions]
  data_plot <- data_plot[, region := factor(region, levels = sel_regions)]
  
  # Add hist for all c5model
  data_plot_hist <- data_plot[c5model == "hist"]
  data_plot00 <- data_plot[c5model != "hist"]
  for (c5m in unique(data_plot00$c5model)) {
    data_plot00 <- rbindlist(list(data_plot00,copy(data_plot_hist)[,c5model := c5m]))
  }
  
  if (interp == 0) {
    
  
  data_plot_spline <- data_plot00[,.(dgmt = seq(0.61,4,by = 0.01),
                                   value = pmin(pmax(approx(x = dgmt, y = value,
                                                  xout = seq(0.61,4,by = 0.01))[['y']],
                                                min(value)),max(value))
                                   ),
                                   by = c("impact","region","c5model")]

  }
  
  if (interp == 1) {
    
  data_plot_spline <- data_plot00[,.(dgmt = seq(0.61,4,by = 0.01),
                                     value = pmin(pmax(spline(x = dgmt, y = value, 
                                                              xout = seq(0.61,4,by = 0.01), 
                                                              ties = min,
                                                              xmin = 0.61, xmax = 4)[['y']],
                                                       min(value)),max(value))
                                    ), 
                                    by = c("impact","region","c5model")]
  }
  
  pp <- list()
  
  sel_iunit <- iunit[match(sel_impacts,iindic)]
  
  for(ii in seq_along(sel_impacts)) {
    
    p <- ggplot(data_plot[impact == sel_impacts[ii] & c5model != "hist"]) +
      geom_line(aes(dgmt,value,color = c5model), data = data_plot_spline[impact == sel_impacts[ii]]) +
      geom_point(aes(dgmt,value,color = c5model, shape = "CMIP5 model projection")) +
      geom_point(aes(dgmt,value, shape = "Historical observation\n(1981-2010)"), 
                 data = data_plot[impact == sel_impacts[ii] & c5model == "hist"],
                 color = "black") +
      ggtitle(sel_impacts[ii]) +
      facet_wrap( ~ region, nrow = 1) +
      labs(x = "Temperature increase from preindustrial [\u00B0C]", 
           y = sel_iunit[ii]) +
      theme_bw() +
      theme(legend.position = "bottom",
            strip.background = element_blank(),
            strip.placement = "outside")
    
    if (ii == 5 ){ 
      p <- p + scale_colour_viridis_d(name = "CMIP5 model",
                                      guide = guide_legend(nrow = 3,byrow = TRUE)) +
        scale_shape_discrete(name = "",
                             guide = guide_legend(nrow = 2,byrow = TRUE)) 
      } else {
        p <- p + scale_colour_viridis_d(name = "CMIP5 model",
                                        guide = FALSE) +
          scale_shape_discrete(name = "",
                               guide = FALSE)        
      }
    
    pp <- c(pp,list(p))
  }
  
  ppp = pp[[1]] + pp[[2]] + pp[[3]] + pp[[4]] + pp[[5]] + 
    plot_layout(nrow = 5, byrow = FALSE)
  ppp
  
  ggsave(outfile, plot = ppp, width = 10, height = 12)
  
  
}


plot_impact_arnell_function_diff <- function(outfile) {
  
  loadd(proxy_impact)
  
  # proxy_impact[!impact %in% iindic[c(3,4,6,7,12:19,26:30,33)]]
  sel_impacts <- c("Heatwave duration",
                   "Heatwave frequency",
                   "Reduction in crop duration: Maize",
                   "Agricultural drought (SPEI) frequency",
                   "Runoff decreases")
  sel_regions <- c('Globe', 'Africa', 'Europe', 'North America', 'South America',
                   'Asia')
  data_plot <- proxy_impact[impact %in% sel_impacts & 
                              region %in% sel_regions]
  data_plot <- data_plot[, region := factor(region, levels = sel_regions)]
  
  # Add hist for all c5model
  data_plot_hist <- data_plot[c5model == "hist"]
  data_plot00 <- data_plot[c5model != "hist"]
  for (c5m in unique(data_plot00$c5model)) {
    data_plot00 <- rbindlist(list(data_plot00,copy(data_plot_hist)[,c5model := c5m]))
  }
  
    
  data_plot_linear <- data_plot00[,.(dgmt = seq(0.61,4,by = 0.01),
                                       value_lin = pmin(pmax(approx(x = dgmt, y = value,
                                                                xout = seq(0.61,4,by = 0.01))[['y']],
                                                         min(value)),max(value))
    ),
    by = c("impact","region","c5model")]
    
  data_plot_spline <- data_plot00[,.(dgmt = seq(0.61,4,by = 0.01),
                                       value_spl = pmin(pmax(spline(x = dgmt, y = value, 
                                                                xout = seq(0.61,4,by = 0.01), 
                                                                ties = min,
                                                                xmin = 0.61, xmax = 4)[['y']],
                                                         min(value)),max(value))
    ), 
    by = c("impact","region","c5model")]
  
  data_plot <- merge(data_plot_linear,data_plot_spline,
                     by = c("impact","region","c5model","dgmt"))
  data_plot[, value_err := value_lin - value_spl]
  data_plot[, value_range := paste0("impact: [",round(min(value_spl)),";",round(max(value_spl)),"]"),
            by = c("impact","region")]
  data_plot[, value_pos := max(value_err), by = c("impact")]
  data_plot[, value_vjust := 1]
  data_plot[impact %in% c("Heatwave frequency","Runoff decreases"), value_pos := min(value_err), by = c("impact")]
  data_plot[impact %in% c("Heatwave frequency","Runoff decreases"), value_vjust := 0]
  
  pp <- list()
  
  sel_iunit <- iunit[match(sel_impacts,iindic)]
  
  for(ii in seq_along(sel_impacts)) {
    
    p <- ggplot(data_plot[impact == sel_impacts[ii] & c5model != "hist"]) +
      geom_line(aes(dgmt,value_err,color = c5model), data = data_plot[impact == sel_impacts[ii]]) +
      geom_smooth(aes(dgmt,value_err), data = data_plot[impact == sel_impacts[ii]], 
                  color = "black", se = FALSE) +
      geom_label(aes(x = 4, y = value_pos, label = value_range, vjust = value_vjust), 
                data = data_plot[impact == sel_impacts[ii] & c5model == "bcccs1" & dgmt == 0.61],
                hjust = 1, size = 3, label.size = NA) +
      ggtitle(sel_impacts[ii]) +
      facet_wrap( ~ region, nrow = 1) +
      labs(x = "Temperature increase from preindustrial [\u00B0C]", 
           y = paste("error ", sel_iunit[ii])) +
      theme_bw() +
      theme(legend.position = "bottom",
            strip.background = element_blank(),
            strip.placement = "outside")
    
    if (ii == 5 ){ 
      p <- p + scale_colour_viridis_d(name = "CMIP5 model",
                                      guide = guide_legend(nrow = 3,byrow = TRUE)) +
        scale_shape_discrete(name = "",
                             guide = guide_legend(nrow = 2,byrow = TRUE)) 
    } else {
      p <- p + scale_colour_viridis_d(name = "CMIP5 model",
                                      guide = FALSE) +
        scale_shape_discrete(name = "",
                             guide = FALSE)        
    }
    
    pp <- c(pp,list(p))
  }
  
  ppp = pp[[1]] + pp[[2]] + pp[[3]] + pp[[4]] + pp[[5]] +
    plot_layout(nrow = 5, byrow = FALSE)
  ppp
  
  ggsave(outfile, plot = ppp, width = 10, height = 12)
  
  
}
