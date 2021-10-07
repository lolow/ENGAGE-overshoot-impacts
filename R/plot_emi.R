plot_show_co2emi_gap_endtemp_main <- function() {
 
  loadd(co2_total,pair_scen_sel)
  
  co2tot <- merge(co2_total, pair_scen_sel, by = c("model","scenario"))
  co2tot[, year := as.numeric(year)]
  co2tot <- co2tot[cluster == clusters[1] & delay == "NPi"]
  co2tot <- co2tot[!(str_detect(model,"AIM") & cbudget != 600)]
  co2tot <- co2tot[!(str_detect(model,"COFFEE") & cbudget != 500)]
  co2tot <- co2tot[!(str_detect(model,"MESSAGE") & cbudget != 700)]
  co2tot <- co2tot[!(str_detect(model,"POLES") & cbudget != 500)]
  co2tot <- co2tot[!(str_detect(model,"TIAM") & cbudget != 700)]
  co2tot <- co2tot[!(str_detect(model,"REMIND"))]
  co2tot <- co2tot[!(str_detect(model,"GEM-E3"))]
  co2tot <- co2tot[!(str_detect(model,"WITCH") & cbudget != 800)]
  co2tot <- dcast(co2tot, model + delay + temp_2100_cb + year + cluster + cbudget ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  # interpolate values at year level
  co2tot_yr <- co2tot[,.(year = 2010:2100,
                         full = spline(x = year, y = full, xout = 2010:2100)$y,
                         peak = spline(x = year, y = peak, xout = 2010:2100)$y),
                      by = c("model")]
  
  co2tot_rib <- co2tot_yr[,.(ymin_full = min(full), ymax_full = max(full),
                             ymin_peak = min(peak), ymax_peak = max(peak)),
                          by = c("year")]
  
  p <- ggplot(co2tot_yr) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_ribbon(aes(x = year, ymin = ymin_full * 1e-3, ymax = ymax_full * 1e-3, fill = "End of\ncentury"), data = co2tot_rib, alpha = 0.25) +
    geom_ribbon(aes(x = year, ymin = ymin_peak * 1e-3, ymax = ymax_peak * 1e-3, fill = "Net zero"), data = co2tot_rib, alpha = 0.25) +
    geom_line(aes(x = year, y = full * 1e-3, color = "End of\ncentury", group = model), linetype = 1, size = 0.8) +
    geom_line(aes(x = year, y = peak * 1e-3, color = "Net zero", group = model), linetype = 1, size = 0.8) +
    geom_label_repel(aes(x = year, y = full * 1e-3, color = "End of\ncentury", label = "End of century"),
                     ylim = c(14,NA),
                     data = co2tot_yr[year == 2075 & model == "WITCH 5.0"]) +
    geom_label_repel(aes(x = year, y = peak * 1e-3, color = "Net zero", label = "Net zero"), 
                     ylim = c(NA, -1),
                     data = co2tot_yr[year == 2040 & model == "POLES ENGAGE"]) +
    coord_cartesian(xlim = c(2010,2100), ylim = c(-20,45), expand = FALSE) +
    scale_linetype(name = "Type") +
    scale_color_brewer(palette = "Set1", name = "Scenario", guide = "none") +
    scale_fill_brewer(palette = "Set1", name = "Scenario", guide =  "none") +
    labs(x = "", y = "CO2 emissions [GtCO2]") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  p
  
  return(p)
   
}

plot_show_co2emi_gap_endtemp_main_panel <- function(outfile) {
  
  loadd(co2_total,pair_scen_sel)
  
  co2tot <- merge(co2_total, pair_scen_sel, by = c("model","scenario"))
  co2tot[, year := as.numeric(year)]
  co2tot <- co2tot[cluster == clusters[1] & delay == "NPi"]
  co2tot <- co2tot[!(str_detect(model,"AIM") & cbudget != 600)]
  co2tot <- co2tot[!(str_detect(model,"COFFEE") & cbudget != 500)]
  co2tot <- co2tot[!(str_detect(model,"MESSAGE") & cbudget != 700)]
  co2tot <- co2tot[!(str_detect(model,"POLES") & cbudget != 500)]
  co2tot <- co2tot[!(str_detect(model,"TIAM") & cbudget != 800)]
  co2tot <- co2tot[!(str_detect(model,"REMIND"))]
  co2tot <- co2tot[!(str_detect(model,"GEM-E3"))]
  co2tot <- co2tot[!(str_detect(model,"WITCH") & cbudget != 800)]

  co2tot <- dcast(co2tot, model + delay + temp_2100_cb + year + cluster + cbudget ~ poltype,
                  value.var = "value",
                  fun.aggregate = mean)
  
  # interpolate values at year level
  co2tot_yr <- co2tot[,.(year = 2010:2100,
                         full = spline(x = year, y = full, xout = 2010:2100)$y,
                         peak = spline(x = year, y = peak, xout = 2010:2100)$y),
                      by = c("model")]
  
  co2tot_rib <- co2tot_yr[,.(ymin_full = min(full), ymax_full = max(full),
                             ymin_peak = min(peak), ymax_peak = max(peak)),
                          by = c("year")]
  
  p <- ggplot(co2tot_yr) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_line(aes(x = year, y = full * 1e-3, color = "End of\ncentury", group = model), linetype = 1, size = 1) +
    geom_line(aes(x = year, y = peak * 1e-3, color = "Net zero", group = model), linetype = 1, size = 1) +
    coord_cartesian(xlim = c(2010,2100), ylim = c(-20,47), expand = FALSE) +
    scale_linetype(name = "Type") +
    scale_color_brewer(palette = "Set1", name = "Scenario") +
    labs(x = "", y = "CO2 emissions [GtCO2]") +
    facet_wrap(~ model) +
    theme_bw() +
    theme(legend.position = "right",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  p
  
  ggsave(outfile, width = 10, height = 6)
  
}

plot_show_non_co2emi_gap_endtemp_main <- function() {
  
  loadd(kghg_total,co2_total,pair_scen_sel)
  
  emi <- merge(kghg_total,co2_total, by = c("model","scenario","region","year"))
  emi[, nonco2 := value.x - value.y]
  
  emi <- merge(emi, pair_scen_sel, by = c("model","scenario"))
  emi[, year := as.numeric(year)]
  emi <- emi[cluster == clusters[1] & delay == "NPi"]
  emi <- emi[!(str_detect(model,"AIM") & cbudget != 600)]
  emi <- emi[!(str_detect(model,"COFFEE") & cbudget != 500)]
  emi <- emi[!(str_detect(model,"MESSAGE") & cbudget != 700)]
  emi <- emi[!(str_detect(model,"POLES") & cbudget != 500)]
  emi <- emi[!(str_detect(model,"REMIND"))]
  emi <- emi[!(str_detect(model,"GEM-E3"))]
  emi <- emi[!(str_detect(model,"TIAM") & cbudget != 700)]
  emi <- emi[!(str_detect(model,"WITCH") & cbudget != 800)]
  emi <- dcast(emi, model + delay + temp_2100_cb + year + cluster + cbudget ~ poltype,
                  value.var = "nonco2",
                  fun.aggregate = mean)
  
  # interpolate values at year level
  emi_yr <- emi[,.(year = 2010:2100,
                         full = spline(x = year, y = full, xout = 2010:2100)$y,
                         peak = spline(x = year, y = peak, xout = 2010:2100)$y),
                      by = c("model")]
  
  emi_rib <- emi_yr[,.(ymin_full = min(full), ymax_full = max(full),
                             ymin_peak = min(peak), ymax_peak = max(peak)),
                          by = c("year")]
  emi_rib2 <- emi_yr[,.(ymin = min(full,peak), ymax = max(full,peak)),
                    by = c("model","year")]
  
  p <- ggplot(emi_yr) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_ribbon(aes(x = year, ymin = ymin * 1e-3, ymax = ymax * 1e-3, 
                    group = model), fill = "yellow",
                data = emi_rib2, alpha = 0.25) +
    geom_line(aes(x = year, y = full * 1e-3, color = "End of century", group = model), 
              linetype = 1, size = 1) +
    geom_line(aes(x = year, y = peak * 1e-3, color = "Net zero", group = model), 
              linetype = 1, size = 1) +
    geom_text(aes(x = year, y = full * 1e-3, 
                  label = str_sub(model, 1, 1)),
                  nudge_x = -5,
                  data = emi_yr[year == 2025 & model == "WITCH 5.0"]) +
    geom_text(aes(x = year, y = full * 1e-3, 
                  label = str_sub(model, 1, 1)),
                  nudge_y = 0.5,
                  data = emi_yr[year == 2050 & model == "MESSAGEix-GLOBIOM_1.1"]) +
    geom_text(aes(x = year, y = full * 1e-3, 
                  label = str_sub(model, 1, 1)), 
                  nudge_x = 5,
                  data = emi_yr[year == 2030 & model == "COFFEE 1.1"]) +
    geom_text(aes(x = year, y = full * 1e-3, 
                  label = str_sub(model, 1, 1)),
                  nudge_y = -0.6,
                  data = emi_yr[year == 2035 & model == "AIM/CGE V2.2"]) +
    geom_text(aes(x = year, y = full * 1e-3, 
                  label = str_sub(model, 1, 1)),
                  nudge_y = -0.5,
                  data = emi_yr[year == 2060 & model == "POLES ENGAGE"]) +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0,18), expand = FALSE) +
    scale_linetype(name = "Type") +
    scale_color_brewer(palette = "Set1", name = "") +
    scale_fill_brewer(palette = "Set1", name = "Scenario", guide = "none") +
    labs(x = "", y = "non-CO2 emissions [GtCO2e]") +
    theme_bw() +
    theme(legend.position = c(0.7,0.87),
          legend.background = element_blank(),
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  p
  
  return(p)
  
}

plot_show_non_co2emi_gap_endtemp_main_panel <- function(outfile) {
  
  loadd(kghg_total,co2_total,pair_scen_sel)
  
  emi <- merge(kghg_total,co2_total, by = c("model","scenario","region","year"))
  emi[, nonco2 := value.x - value.y]
  
  emi <- merge(emi, pair_scen_sel, by = c("model","scenario"))
  emi[, year := as.numeric(year)]
  emi <- emi[cluster == clusters[1] & delay == "NPi"]
  emi <- emi[!(str_detect(model,"AIM") & cbudget != 600)]
  emi <- emi[!(str_detect(model,"COFFEE") & cbudget != 500)]
  emi <- emi[!(str_detect(model,"MESSAGE") & cbudget != 700)]
  emi <- emi[!(str_detect(model,"POLES") & cbudget != 500)]
  emi <- emi[!(str_detect(model,"REMIND"))]
  emi <- emi[!(str_detect(model,"GEM-E3"))]
  emi <- emi[!(str_detect(model,"TIAM") & cbudget != 700)]
  emi <- emi[!(str_detect(model,"WITCH") & cbudget != 800)]
  emi <- dcast(emi, model + delay + temp_2100_cb + year + cluster + cbudget ~ poltype,
               value.var = "nonco2",
               fun.aggregate = mean)
  
  # interpolate values at year level
  emi_yr <- emi[,.(year = 2010:2100,
                   full = spline(x = year, y = full, xout = 2010:2100)$y,
                   peak = spline(x = year, y = peak, xout = 2010:2100)$y),
                by = c("model")]
  
  emi_rib <- emi_yr[,.(ymin_full = min(full), ymax_full = max(full),
                       ymin_peak = min(peak), ymax_peak = max(peak)),
                    by = c("year")]
  emi_rib2 <- emi_yr[,.(ymin = min(full,peak), ymax = max(full,peak)),
                     by = c("model","year")]
  
  p <- ggplot(emi_yr) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_ribbon(aes(x = year, ymin = ymin * 1e-3, ymax = ymax * 1e-3, 
                    group = model), fill = "yellow",
                data = emi_rib2, alpha = 0.25) +
    geom_line(aes(x = year, y = full * 1e-3, color = "End of century", group = model), 
              linetype = 1, size = 1) +
    geom_line(aes(x = year, y = peak * 1e-3, color = "Net zero", group = model), 
              linetype = 1, size = 1) +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0,18), expand = FALSE) +
    scale_linetype(name = "Type") +
    scale_color_brewer(palette = "Set1", name = "") +
    scale_fill_brewer(palette = "Set1", name = "Scenario", guide = "none") +
    labs(x = "", y = "non-CO2 emissions [GtCO2e]") +
    facet_wrap(~ model) +
    theme_bw() +
    theme(legend.position = "right",
          legend.background = element_blank(),
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  p
  
  ggsave(outfile, width = 10, height = 6)
  
}

plot_em_negative <- function(outfile) {

  loadd(beccs, ffccs, afforest, dac_ccs)
  loadd(pair_scen_sel)
  
  dd1 <- merge(beccs,pair_scen_sel, by = c("model","scenario"))
  
  p1 <- ggplot(dd1) +
    geom_line(aes(x = as.numeric(year), y = value * 1e-3, 
                  color = cbudget, group = paste(model,scenario)),
              alpha = 0.5, size = 1) +
    scale_color_viridis_c(name = "Carbon budget") +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0,20), expand = FALSE) +
    labs(x = "", y = "[GtCO2]") +
    ggtitle("A) Bioenergy with carbon capture and storage") +
    theme_bw()
  
  dd2 <- merge(ffccs,pair_scen_sel, by = c("model","scenario"))
  
  p2 <- ggplot(dd2) +
    geom_line(aes(x = as.numeric(year), y = value * 1e-3, 
                  color = cbudget, group = paste(model,scenario)),
              alpha = 0.5, size = 1) +
    scale_color_viridis_c(name = "Carbon budget") +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0,20), expand = FALSE) +
    labs(x = "", y = "[GtCO2]") +
    ggtitle("B) Fossil fuel with carbon capture and storage") +
    theme_bw()
  
  
  dd3 <- merge(afforest,pair_scen_sel, by = c("model","scenario"))
  
  p3 <- ggplot(dd3[model != "POLES ENGAGE"]) + # reporting error for POLES
    geom_line(aes(x = as.numeric(year), y = value * 1e-3, 
                  color = cbudget, group = paste(model,scenario)),
              alpha = 0.5, size = 1) +
    scale_color_viridis_c(name = "Carbon budget") +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0,20), expand = FALSE) +
    labs(x = "", y = "[GtCO2]") +
    ggtitle("C) Afforestation") +
    theme_bw()
  
  dd4 <- merge(dac_ccs,pair_scen_sel, by = c("model","scenario"))
  dd4[model == "REMIND-MAgPIE 2.1-4.2", value := - value]
  
  p4 <- ggplot(dd4) + # reporting error for POLES
    geom_line(aes(x = as.numeric(year), y = value * 1e-3, 
                  color = cbudget, group = paste(model,scenario)),
              alpha = 0.5, size = 1) +
    scale_color_viridis_c(name = "Carbon budget") +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0,20), expand = FALSE) +
    labs(x = "", y = "[GtCO2]") +
    ggtitle("D) Direct air capture with carbon capture and storage") +
    theme_bw()
  
  pp <- p1 + p2 + p3 + p4
  
  ggsave(outfile, width = 10, height = 6)

}

plot_cb_temp <- function(outfile) {
  
  dd <- readd(pair_scen_sel)
  
  dd[, cluster := factor(cluster, levels = clusters)]
  
  pp <- ggplot(dd[poltype == "peak"]) +
    geom_point(aes(x = cluster, y = cbudget, color = model),
                position = position_dodge(width = 0.5)) +
    scale_color_discrete(name = "Models") +
    labs(y = "Carbon budget [GtCO2]", x = "Temperature clusters") +
    coord_flip() + 
    theme_bw()
  
  ggsave(outfile, width = 10, height = 6)
  
}