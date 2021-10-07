


plot_admg_overshoot_all <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a) Burke et al. (2015)"
    dd <- readd(admg_gwt_dmg)
    }
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b) Howard & Sterner (2017)"
    dd <- readd(admg_lvl_dmg)[sw == 1]
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c) Takakura et al. (2019)"
    dd <- readd(admg_lvl_dmg)[sw == 2]
  }

  dd <- merge(dd,pair_scen_sel, by = c("model","scenario"))
  
  dw <- dcast(dd[region == "World" & c5model == 1 & delay == "NPi"],
              model + cbudget + delay + year + cluster ~ poltype,
              value.var = "admg",
              fun.aggregate = mean
  )
  dw[, diff := peak - full]
  dw[, N := length(unique(model,cbudget)), by = c("cluster","delay","year")]
  
  
  dwall <- dcast(dd[region == "World" & delay == "NPi" & c5model != 1],
                 c5model + sw + model + cbudget + delay + year + cluster ~ poltype,
                 value.var = "admg",
  )
  dwall[, diff := peak - full]
  
  dwrib <- dwall[,.(ymin = min(diff), 
                    ymax = max(diff)), 
                 by = c("cluster,year")]
  
  dwstat <- dwall[,.(ymin = min(diff) * 1e-3,
                     ymax = max(diff) * 1e-3,
                     ymed = quantile(diff,0.5) * 1e-3),
                  by = c("cluster","year")]
  
  lim0 <- min(dwrib$ymin)
  dwn <- dw[year == 2050,
            .(N = .N), by = c("cluster","delay")]
  dwn <- dcast(dwn, cluster ~ delay, fun.aggregate = sum)
  dwn[, N := NPi]
  
  dw[, delay := factor(delay, levels = c("NPi","NDC"))]
  dw[, cluster := factor(cluster, levels = clusters)]
  dwrib[, cluster := factor(cluster, levels = clusters)]
  dwn[, cluster := factor(cluster, levels = clusters)]
  dwstat[, cluster := factor(cluster, levels = clusters)]  
  
  p <- ggplot(dw) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(x = year, 
                    ymin = ymin * 1e-3,
                    ymax = ymax * 1e-3),
                data = dwrib,
                fill = 'grey',
                alpha = 0.5) +
    geom_line(aes(x = year, 
                  y = diff * 1e-3, 
                  color = model, 
                  group = paste(model,cbudget)), 
              alpha = 0.5) +
    geom_text(aes(x = 2100, y = lim0 * 1e-3, label = paste0("N=",N)), 
              data = dwn,
              hjust = 1,
              vjust = 0,
              size = 3) +
    geom_line(aes(x = year,y = ymed),
              data = dwstat,
              color = 'black', size = 1) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax),
                  data = dwstat[year == 2050],
                  size = 0.8, width = 2.5) +
    geom_errorbar(aes(x = year, ymin = ymin, ymax = ymax),
                  data = dwstat[year == 2100],
                  size = 0.8, width = 2.5) +
    geom_point(aes(x = year, y = ymed),
               data = dwstat[year == 2050],
               color = 'white', size = 3) +
    geom_point(aes(x = year, y = ymed),
               data = dwstat[year == 2100],
               color = 'white', size = 3) +
    geom_point(aes(x = year, y = ymed),
               data = dwstat[year == 2050],
               color = 'black', size = 2) +
    geom_point(aes(x = year, y = ymed),
               data = dwstat[year == 2100],
               color = 'black', size = 2) +
    facet_wrap(~ cluster, nrow = 1) +
    labs(x = "", y = "[T$]", 
         title = paste(label_prefix)) +
    scale_color_brewer(palette = "Set1", name = "Model") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  return(p)
  
}

plot_admg_overshoot_all_combine <- function(p1,p2,p3,outfile) {
  
  pp <- p1 + p2 + p3 + guide_area() + 
    plot_layout(ncol = 1, heights = c(5,5,5,1), guides = "collect") & 
    theme(legend.position='bottom')
  
  ggsave(outfile,plot = pp, width = 10, height = 10)
  
}

plot_admg_overshoot2 <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a) Burke et al. (2015)"
    dd <- readd(admg_gwt_dmg)
  } 
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b) Howard & Sterner (2017)"
    dd <- readd(admg_lvl_dmg)[sw == 1]
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c) Takakura et al. (2019)"
    dd <- readd(admg_lvl_dmg)[sw == 2]
  }
  
  dd <- merge(dd,pair_scen_sel, by = c("model","scenario"))
  
  dw <- dcast(dd[region == "World" & c5model == 1 & delay == "NPi"],
              model + cbudget + delay + year + cluster ~ poltype,
              value.var = "admg",
              fun.aggregate = mean
  )
  dw[, diff := peak - full]
  dw[, N := length(unique(model,cbudget)), by = c("cluster","delay","year")]
  
  
  dwall <- dcast(dd[region == "World" & delay == "NPi" & c5model != 1],
                 c5model + sw + model+cbudget+delay+year+cluster ~ poltype,
                 value.var = "admg"
  )
  dwall[, diff := peak - full]
  
  dwrib <- dwall[,.(ymin = quantile(diff,0.05), 
                    ymax = quantile(diff,0.95)), by = c("cluster,year")]
  
  dwstat <- dwall[,.(ymin = quantile(diff,0.05) * 1e-3,
                     ymax = quantile(diff,0.95) * 1e-3,
                     ymed = quantile(diff,0.5) * 1e-3),
                  by = c("cluster","year")]
  
  lim0 <- min(dwrib$ymin)
  dwn <- dw[year == 2050,
            .(N = .N), by = c("cluster","delay")]
  dwn <- dcast(dwn, cluster ~ delay, fun.aggregate = sum)
  dwn[, N := NPi]
  
  dw[, delay := factor(delay, levels = c("NPi","NDC"))]
  
  dw[, cluster := factor(cluster, levels = clusters)]
  dwrib[, cluster := factor(cluster, levels = clusters)]
  dwstat[, cluster := factor(cluster, levels = clusters)]
  
  p <- ggplot(dw) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(x = year, 
                    ymin = ymin * 1e-3,
                    ymax = ymax * 1e-3
                    ),
                data = dwrib,
                alpha = 0.25) +
    geom_line(aes(x = year,y = ymed),
              data = dwstat,
              alpha = 1,
              size = 1) +
    scale_x_continuous(breaks = c(2020,2060,2100)) +
    #scale_y_continuous(limits = c(-1, 12), expand = c(0,0)) +
  facet_wrap(~ cluster, nrow = 2) +
  labs(x = "", y = ifelse(spec=="BHM SR","Additionnal avoided damages [T$]",""), 
       title = paste(label_prefix)) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(8, "mm"),
          panel.spacing.y = unit(0, "mm")) +
    theme(strip.background = element_blank(),
          strip.placement = "inside")
  if (spec != "BHM SR") {
    p <- p + theme(axis.title = element_blank())
  }
  return(p)
  
}

plot_admg_overshoot2_perc <- function(spec) {
  
  loadd(pair_scen_sel)
  
  # Filter damage function
  if (spec == "BHM SR") {
    label_prefix <- "a) Burke et al. (2015)"
    xx1 <- readd(admg_gwt_dmg)
    xx2 <- readd(gdp_gwt_a)
    dd <- merge(xx1,xx2,
                by = c("c5model","sw","model","scenario","region","year"))
    dd[, admg := admg / gdp_cc]
    dd[, gdp_cc := NULL]
    rm(xx1,xx2)
  }
  if (spec == "HS2017 NCAT") {
    label_prefix <- "b) Howard & Sterner (2017)"
    xx1 <- readd(admg_lvl_dmg)[sw == 1]
    xx2 <- readd(gdp_levels)[sw == 1]
    dd <- merge(xx1,xx2,
                by = c("c5model","sw","model","scenario","region","year"))
    dd[, admg := admg / gdp_cc]
    dd[, gdp_cc := NULL]
    rm(xx1,xx2)
  }
  if (spec == "TAKAKURA2019") {
    label_prefix <- "c) Takakura et al. (2019)"
    xx1 <- readd(admg_lvl_dmg)[sw == 2]
    xx2 <- readd(gdp_levels)[sw == 2]
    dd <- merge(xx1,xx2,
                by = c("c5model","sw","model","scenario","region","year"))
    dd[, admg := admg / gdp_cc]
    dd[, gdp_cc := NULL]
    rm(xx1,xx2)
  }
  
  dd <- merge(dd,pair_scen_sel, by = c("model","scenario"))
  
  dw <- dcast(dd[region == "World" & c5model == 1 & delay == "NPi"],
              model + cbudget + delay + year + cluster ~ poltype,
              value.var = "admg",
              fun.aggregate = mean
  )
  dw[, diff := peak - full]
  dw[, N := length(unique(model,cbudget)), by = c("cluster","delay","year")]
  
  
  dwall <- dcast(dd[region == "World" & delay == "NPi" & c5model != 1],
                 c5model + sw + model+cbudget+delay+year+cluster ~ poltype,
                 value.var = "admg"
  )
  dwall[, diff := peak - full]
  
  dwrib <- dwall[,.(ymin = quantile(diff,0.05), 
                    ymax = quantile(diff,0.95)), by = c("cluster,year")]
  
  dwstat <- dwall[,.(ymin = quantile(diff,0.05),
                     ymax = quantile(diff,0.95),
                     ymed = quantile(diff,0.5)),
                  by = c("cluster","year")]
  
  lim0 <- min(dwrib$ymin)
  dwn <- dw[year == 2050,
            .(N = .N), by = c("cluster","delay")]
  dwn <- dcast(dwn, cluster ~ delay, fun.aggregate = sum)
  dwn[, N := NPi]
  
  dw[, delay := factor(delay, levels = c("NPi","NDC"))]
  
  dw[, cluster := factor(cluster, levels = clusters)]
  dwrib[, cluster := factor(cluster, levels = clusters)]
  dwstat[, cluster := factor(cluster, levels = clusters)]
  
  p <- ggplot(dw) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(x = year, 
                    ymin = ymin * 1e2,
                    ymax = ymax * 1e2),
    data = dwrib,
    alpha = 0.25) +
    geom_line(aes(x = year,y = ymed * 1e2),
              data = dwstat,
              alpha = 1,
              size = 1) +
    scale_x_continuous(breaks = c(2020,2060,2100)) +
    #scale_y_continuous(limits = c(-1, 12), expand = c(0,0)) +
    facet_wrap(~ cluster, nrow = 2) +
    labs(x = "", y = ifelse(spec=="BHM SR","Additionnal avoided damages [% of GDP]",""), 
         title = paste(label_prefix)) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(8, "mm"),
          panel.spacing.y = unit(0, "mm")) +
    theme(strip.background = element_blank(),
          strip.placement = "inside")
  if (spec != "BHM SR") {
    p <- p + theme(axis.title = element_blank())
  }
  return(p)
  
}


plot_admg_overshoot2_combine <- function(p1,p2,p3,outfile) {
  
  design <- c(area(1, 1, 8, 1), area(1, 2, 8, 2), area(1, 3, 8, 3))
  
  pp <- p1 + p2 + p3 + plot_layout(design = design)
  
  ggsave(outfile,plot = pp, width = 10, height = 5)
  
}

plot_admg_overshoot2_alt <- function(scase) {
  
  loadd(pair_scen_sel)
  
  spec <- "BHM SR"
  # Filter damage function
  if (scase == 1) {
    label_prefix <- "a) Our Study"
    label_prefix_caption <- "Burke et al. (2015)"
    dd <- readd(admg_gwt_dmg)
  } 
  if (scase == 2) {
    label_prefix <- "b) Original baseline"
    label_prefix_caption <- "Burke et al. (2015)"
    dd <- readd(admg_gwt_dmg_alt)
  } 
  
  dd <- merge(dd,pair_scen_sel, by = c("model","scenario"))
  
  dw <- dcast(dd[region == "World" & c5model == 1 & delay == "NPi"],
              model + cbudget + delay + year + cluster ~ poltype,
              value.var = "admg",
              fun.aggregate = mean
  )
  dw[, diff := peak - full]
  dw[, N := length(unique(model,cbudget)), by = c("cluster","delay","year")]
  
  dwall <- dcast(dd[region == "World" & delay == "NPi" & c5model != 1],
                 c5model + sw + model+cbudget+delay+year+cluster ~ poltype,
                 value.var = "admg"
  )
  dwall[, diff := peak - full]
  
  dwrib <- dwall[,.(ymin = quantile(diff,0.05), 
                    ymax = quantile(diff,0.95)), by = c("cluster,year")]
  
  dwstat <- dwall[,.(ymin = quantile(diff,0.05) * 1e-3,
                     ymax = quantile(diff,0.95) * 1e-3,
                     ymed = quantile(diff,0.5) * 1e-3),
                  by = c("cluster","year")]
  
  lim0 <- min(dwrib$ymin)
  dwn <- dw[year == 2050,
            .(N = .N), by = c("cluster","delay")]
  dwn <- dcast(dwn, cluster ~ delay, fun.aggregate = sum)
  dwn[, N := NPi]
  
  dw[, delay := factor(delay, levels = c("NPi","NDC"))]
  
  dw[, cluster := factor(cluster, levels = clusters)]
  dwrib[, cluster := factor(cluster, levels = clusters)]
  dwstat[, cluster := factor(cluster, levels = clusters)]
  
  p <- ggplot(dw) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(x = year, 
                    ymin = ymin * 1e-3,
                    ymax = ymax * 1e-3),
                data = dwrib,
                alpha = 0.25) +
    geom_line(aes(x = year,y = ymed),
              data = dwstat,
              alpha = 1,
              size = 1) +
    scale_x_continuous(breaks = c(2020,2060,2100)) +
    scale_y_continuous(limits = c(-1,12)) +
    facet_wrap(~ cluster, nrow = 4) +
    labs(x = "", y = ifelse(spec=="BHM SR","Additionnal avoided damages [T$]",""), 
         title = paste(label_prefix),
         subtitle = label_prefix_caption) +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(8, "mm"),
          panel.spacing.y = unit(0, "mm")) +
    theme(strip.background = element_blank(),
          strip.placement = "inside")
  
  p <- p + theme(axis.title = element_blank())
  
  return(p)
  
}

plot_admg_overshoot2_combine_alt <- function(p1,p2,outfile) {
  
  pp <- p1 + p2
  
  ggsave(outfile,plot = pp, width = 6, height = 8)
  
}


