plot_cmit <- function(cmit_gwt_dmg,pair_scen,outfile) {
  
  dd <- copy(cmit_gwt_dmg)
  dd[, cmit_share := cmit / gdp_cc_ref * 100]
  dd <- merge(dd[c5model == 1],
              pair_scen_sel, by = c("model","scenario"))
  
  p <- ggplot(dd[region == "World"]) +
    geom_line(aes(x = year, 
                  y = cmit_share, 
                  color = model, 
                  linetype = poltype, 
                  group = paste(model,scenario,poltype)), 
              alpha = 0.5) +
    facet_wrap(~ cbudget) +
    labs(x = "", y = "[% GDP]") +
    scale_color_brewer(palette = "Set1")
  p
  
  ggsave(outfile, width = 10, height = 6)
  
}

plot_cmit_overshoot <- function(outfile) {
  
  loadd(cmit_gwt_dmg,pair_scen_sel)
  
  dd <- merge(cmit_gwt_dmg,pair_scen_sel, by = c("model","scenario"))
  dd[, cmit_share := cmit / gdp_cc_ref * 100]
  dd <- dd[c5model == 1]
  
  dw <- dcast(dd[region == "World" & delay == "NPi"],
          model + cbudget + delay + year + cluster ~ poltype,
          value.var = "cmit_share",
          fun.aggregate = mean)
  dw[, diff := full - peak]
  dw[, N := length(unique(model,cbudget)), by = c("cluster","delay","year")]
  
  dwrib <- dw[,.(ymin = min(diff), ymax = max(diff)), by = c("cluster,year")]
  dwrib2 <- dw[,.(ymin = quantile(diff,0.25), ymax = quantile(diff,0.75)), by = c("cluster,year")]
  
  lim0 <- min(dwrib$ymin)
  dwn <- dw[year == 2050,
            .(N = length(unique(paste(model,cbudget)))), by = c("cluster","delay")]
  dwn <- dcast(dwn, cluster ~ delay, fun.aggregate = sum)
  dwn[, N := NPi]
  
  dw[, delay := factor(delay, levels = c("NPi","NDC"))]
  
  dw[, cluster := factor(cluster, levels = clusters)]
  dwn[, cluster := factor(cluster, levels = clusters)]
  dwrib[, cluster := factor(cluster, levels = clusters)]
  dwrib2[, cluster := factor(cluster, levels = clusters)]
  
  p <- ggplot(dw[delay == "NPi"]) +
    geom_hline(yintercept = 0) +
    geom_ribbon(aes(x = year, 
                  ymin = ymin,
                  ymax = ymax),
                data = dwrib,
                fill = 'grey',
                alpha = 0.2) +
    geom_ribbon(aes(x = year, 
                    ymin = ymin,
                    ymax = ymax),
                data = dwrib2,
                fill = 'grey',
                alpha = 0.7) +
    geom_line(aes(x = year, 
                  y = diff, 
                  color = model, 
                  group = paste(model,cbudget,delay)),
              alpha = 0.5) +
    geom_text(aes(x = 2100, 
                  y = lim0, 
                  label = paste0("N=",N)), 
              data = dwn,
              hjust = 1,
              vjust = 0,
              size = 3) +
    facet_wrap(~ cluster, nrow = 1) +
    labs(x = "", y = "Difference in mitigation cost [% GDP]") +
    scale_color_brewer(palette = "Set1", name = "Model") +
    #scale_size_manual(values = c(0.7,0.5), name = "Policy delay") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  p
  
  ggsave(outfile, width = 8, height = 5)
  
}
