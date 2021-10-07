

plot_show_temperature_gap <- function(gmt_hist,temp_magicc,pair_scen_sel,outfile) {

  # NPi     800  8
  temp <- merge(temp_magicc, pair_scen_sel, by = c("model","scenario"))
  temp[, value := value]
  temp[, year := as.numeric(year)]
  temp <- temp[cbudget == 800 & delay == "NPi"]
  temp <- dcast(temp, model + delay + temp_2100_cb + year ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  temp[, ymin := pmin(full, peak)]
  temp[, ymax := pmax(full, peak)]
  temp[, diff_temp := full - peak]
  
  temp[, max_full := max(full), by = c("model,temp_2100_cb,delay")]
  temp[, max_peak := max(peak), by = c("model,temp_2100_cb,delay")]
  
  
  p <- ggplot(temp) +
    geom_hline(aes(yintercept = max_full, color = "Full"), 
               data = temp[year == 2010], linetype = 3, alpha = 0.6, size = 0.5) +
    geom_hline(aes(yintercept = max_peak, color = "Peak"), 
               data = temp[year == 2010], linetype = 3, alpha = 0.6, size = 0.5) +
    geom_segment(aes(x = 2012, xend = 2012, yend = max_full, y = max_peak), 
                 data = temp[year == 2010], 
                 size = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
    geom_text_repel(aes(x = 2012, y = (max_full + max_peak) / 2, 
                        label = paste(round(max_full-max_peak, 2),"C")),
                     alpha = 0.7, 
                    data = temp[year == 2010]) +
    geom_line(aes(x = year, y = gmt), 
              data = gmt_hist) +
    geom_ribbon(aes(x = year,
                    ymin = ymin,
                    ymax = ymax),
                alpha = 0.25) +
    geom_line(aes(x = year, y = full, color = "end of\ncentury"), linetype = 1, size = 1) +
    geom_line(aes(x = year, y = peak, color = "net zero"), linetype = 1, size = 1) +
    #geom_point(aes(x = year, y = (full + peak) / 2),
    #           data = temp[year == 2100], size = 2) +
    geom_label_repel(aes(x = year, y = full, color = "end of\ncentury", label = "end of century"),
                     ylim = c(1.55,NA),
                    data = temp[year == 2080 & model == "WITCH 5.0"]) +
    geom_label_repel(aes(x = year, y = peak, color = "net zero", label = "net zero"), 
                     ylim = c(NA,1.35),
                     data = temp[year == 2080 & model == "WITCH 5.0"]) +
    facet_wrap( ~ model,ncol = 4) +
      coord_cartesian(xlim = c(2010,2100), ylim = c(0.6,1.8)) +
    scale_linetype(name = "Type") +
    scale_color_brewer(palette = "Set1", name = "Scenario", guide = "none") +
    labs(x = "", y = "Global mean temperature [\u00B0C]") +
    theme_bw() +
    theme(legend.position = "bottom",
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  
  ggsave(outfile, width = 10, height = 6)
  
  
}

plot_show_temperature_gap_endtemp_main <- function() {
  
  loadd(gmt_hist,temp_magicc_50,pair_scen_sel)
  
  # NPi     800  8
  temp <- merge(temp_magicc_50, pair_scen_sel, by = c("model","scenario"))
  temp[, year := as.numeric(year)]
  temp <- temp[cluster == clusters[1] & delay == "NPi"]
  temp <- temp[!(str_detect(model,"AIM") & cbudget != 600)]
  temp <- temp[!(str_detect(model,"COFFEE") & cbudget != 500)]
  temp <- temp[!(str_detect(model,"MESSAGE") & cbudget != 700)]
  temp <- temp[!(str_detect(model,"POLES") & cbudget != 600)]
  temp <- temp[!(str_detect(model,"TIAM") & cbudget != 800)]
#  temp <- temp[!(str_detect(model,"TIAM"))]
  temp <- temp[!(str_detect(model,"GEM-E3"))]
  temp <- temp[!(str_detect(model,"WITCH") & cbudget != 800)]
  temp <- temp[!(str_detect(model,"REMIND"))]
#  temp <- temp[!(str_detect(model,"REMIND")  & cbudget != 500)]
  temp <- dcast(temp, model + delay + temp_2100_cb + year + cluster + cbudget ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  temp[, ymin := pmin(full, peak)]
  temp[, ymax := pmax(full, peak)]
  temp[, diff_temp := full - peak]
  
  temp[, max_full := max(full), by = c("model,temp_2100_cb,delay,cluster")]
  temp[, max_peak := max(peak), by = c("model,temp_2100_cb,delay,cluster")]
  
  
  p <- ggplot(temp) +
        geom_hline(aes(yintercept = max_full, color = "End of\ncentury"), 
                   data = temp[year == 2010], linetype = 3, alpha = 0.6, size = 0.5) +
        geom_hline(aes(yintercept = max_peak, color = "Net zero"), 
                   data = temp[year == 2010], linetype = 3, alpha = 0.6, size = 0.5) +
        geom_segment(aes(x = 2012, xend = 2012, yend = max_full, y = max_peak), 
                     data = temp[year == 2010], size = 0.5, arrow = arrow(length = unit(0.03, "npc"))) +
        geom_text_repel(aes(x = 2012, y = (max_full + max_peak) / 2, 
                            label = paste0(round(max_full-max_peak, 2),"\u00B0C")),
                        alpha = 0.7, 
                        data = temp[year == 2010]) +
    geom_text(aes(x = 2100, y = 0.75, label = paste0(cbudget, "~GtCO[2]")),
              data = temp[year == 2010], parse = TRUE, hjust = 1, size = 3) +
    geom_line(aes(x = year, y = gmt), 
              data = gmt_hist) +
    geom_ribbon(aes(x = year,
                    ymin = ymin,
                    ymax = ymax,
                    group = temp_2100_cb),
                fill = "yellow",
                alpha = 0.25) +
    geom_line(aes(x = year, y = full, color = "End of\ncentury", group = temp_2100_cb), linetype = 1, size = 1) +
    geom_line(aes(x = year, y = peak, color = "Net zero", group = temp_2100_cb), linetype = 1, size = 1) +
    geom_label_repel(aes(x = year, y = full, color = "End of\ncentury", label = "End of century"),
                         ylim = c(1.65,NA),
                         data = temp[year == 2080 & model == "WITCH 5.0"]) +
    geom_label_repel(aes(x = year, y = peak, color = "Net zero", label = "Net zero"), 
                         ylim = c(NA,1.35),
                         data = temp[year == 2080 & model == "WITCH 5.0"]) +
    facet_wrap( ~ model,ncol = 3) +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0.7,1.9)) +
    scale_linetype(name = "Type") +
    scale_color_brewer(palette = "Set1", name = "Scenario", guide = "none") +
    labs(x = "", y = "Global mean temperature increase [\u00B0C]") +
    theme_bw() +
    theme(legend.position = "bottom",
          strip.background = element_blank(),
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(5, "mm"))

  return(p)
  
}


plot_show_temperature_gap_endtemp_cluster <- function(cc,outfile) {
  
  loadd(gmt_hist,temp_magicc_50,pair_scen_sel)
  
  # NPi     800  8
  temp <- merge(temp_magicc_50, pair_scen_sel, by = c("model","scenario"))
  temp[, year := as.numeric(year)]
  temp <- temp[cluster == clusters[cc] & delay == "NPi"]
  temp <- dcast(temp, model + delay + temp_max_peak + year + cluster ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  temp[, ymin := pmin(full, peak)]
  temp[, ymax := pmax(full, peak)]
  temp[, diff_temp := full - peak]
  
  temp[, max_full := max(full), by = c("model,temp_max_peak,delay,cluster")]
  temp[, max_peak := max(peak), by = c("model,temp_max_peak,delay,cluster")]
  
  if (cc == 1) {
    lim0 <- min(temp$temp_max_peak) - 0.05
    lim1 <- 1.55
  }
  if (cc == 2) {
    lim0 <- 1.55
    lim1 <- 1.65
  }
  if (cc == 3) {
    lim0 <- 1.65
    lim1 <- 1.8
  }
  if (cc == 4) {
    lim0 <- 1.8
    lim1 <- max(temp$temp_max_peak) + 0.05
  }
  
  xx <- data.frame(year = 2005:2100,ymin = lim0,ymax = lim1)
  
  tempn <- temp[year == 2050,.N, by = "model"]
    
  p <- ggplot(temp) +
    geom_ribbon(mapping = aes(x = year, ymin = ymin, ymax = ymax), data = xx,
                fill = "gray", alpha = 0.25) +
    geom_hline(yintercept = c(1.55, 1.65, 1.8), linetype = "dashed", size = 0.5, color = "gray") +
    geom_line(aes(x = year, y = gmt), 
              data = gmt_hist) +
    geom_ribbon(aes(x = year,
                    ymin = ymin,
                    ymax = ymax,
                    group = temp_max_peak),
                fill = "yellow",
                alpha = 0.25) +
    geom_line(aes(x = year, y = full, color = "End of\ncentury", group = temp_max_peak), linetype = 1, size = 1) +
    geom_line(aes(x = year, y = peak, color = "Net Zero", group = temp_max_peak), linetype = 1, size = 1) +
    geom_point(aes(x = year, y = (full + peak) / 2),
               data = temp[year == 2100], size = 2) +
    geom_text(aes(x = 2100, y = 0.5, label = paste0("N = ",N)), 
              data = tempn,
              hjust = 1,
              vjust = -1) +
    facet_wrap( ~ model,ncol = 4) +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0.6,2.2)) +
    scale_linetype(name = "Type") +
    scale_color_brewer(palette = "Set1", name = "Scenario") +
    labs(x = "", y = "Global mean temperature [\u00B0C]") +
    theme_bw() +
    theme(legend.position = c(0.9, 0.1),
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))

  ggsave(outfile, width = 10, height = 6)
  
  
}

plot_diff_temperature <- function(type,outfile) {
  
  if (type == "diag") {
    loadd(temp_magicc_50)
    sscen <- readd(pair_scen)
  }
  if (type == "final") {
    loadd(temp_magicc_50)
    sscen <- readd(pair_scen_sel)
  }
  
  temp <- merge(temp_magicc_50, sscen, by = c("model","scenario"))
  temp[, year := as.numeric(year)]
  temp <- dcast(temp, model + cbudget + delay + temp_2100_cb + year ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  temp[, diff_temp := full - peak]
  
  p <- ggplot(temp[delay == "NPi" & year >= 2010]) +
    geom_line(aes(x = year, y = diff_temp, 
                  color = temp_2100_cb, 
                  group = temp_2100_cb), size = 1) +
    facet_wrap( ~ model, ncol = 3) +
    scale_x_continuous(guide = guide_axis(check.overlap = T)) + 
    scale_color_viridis_c(name = "Temperature in 2100 [\u00B0C]"#, 
                          #breaks = seq(1,2, by = 0.1),
                          #limits = c(1,2)
                          ) +
    labs(x = "", y = "Difference in temperature [\u00B0C]") +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.key.width = unit(2,"cm"),
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  
  ggsave(outfile, width = 10, height = 6)
  
  
}

# Statistics about trajectories
plot_temperature_integral <- function(outfile) {
  
  loadd(temp_magicc_50,pair_scen_sel)
  
  temp <- merge(temp_magicc_50, pair_scen_sel, by = c("model","scenario"))
  temp[, year := as.numeric(year)]
  temp <- dcast(temp[delay == "NPi"], 
                model + delay + cbudget + year + temp_2100_cb ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  temp[, diff_pos := pmax(0,full - peak)]
  temp[, diff_neg := pmin(0,full - peak)]
  
  temp_stat <- temp[, .(int_pos = sum(diff_pos),
                        int_neg = sum(diff_neg),
                        max_pos = max(diff_pos),
                        min_pos = min(diff_neg)),by = "model,delay,cbudget,temp_2100_cb"]
  
  temp_stat[, delay := factor(delay, levels = c("NPi","NDC"))]
  
  p1 <- ggplot(temp_stat[int_pos > 0 & delay == "NPi"]) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = c(1.55,1.65,1.8), color = "gray", linetype = "dashed") +
    geom_point(aes(x = temp_2100_cb, y = int_pos, color = model), size = 2) +
    geom_line(aes(x = temp_2100_cb, y = int_pos, color = model)) +
    scale_color_npg(name = "Model", guide = "none") +
    scale_y_continuous(breaks = seq(0,10,by=2)) +
    labs(title = "a) Full > Peak ",
         x = "Temperature increase in 2100 [\u00B0C]", 
         y = "Cumulative temperature [\u00B0C\u00B7year]") +
    coord_cartesian(ylim = c(0,10)) +
    theme_bw()

  p2 <- ggplot(temp_stat[int_neg < 0 & delay == "NPi"]) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = c(1.55,1.65,1.8), color = "gray", linetype = "dashed") +
    geom_point(aes(x = temp_2100_cb, y = int_neg, color = model, group = paste(model,delay)), size = 2) +
    geom_line(aes(x = temp_2100_cb, y = int_neg, color = model)) +
    scale_y_continuous(breaks = -(seq(0,10,by=2))) +
    scale_color_npg(name = "Model") +
    labs(title = "b) Full < Peak ",
         x = "Temperature increase in 2100 [\u00B0C]", 
         y = "Cumulative temperature [\u00B0C\u00B7year]") +
    coord_cartesian(ylim = c(-10,0)) +
    theme_bw() +
    theme(legend.position = c(0.7, 0.4))

  pp <- p1 + p2 #+ plot_layout(guides = 'collect')
  
  ggsave(outfile, plot = pp,width = 10, height = 5)
  
  
}

plot_temperature_max <- function(outfile) {
  
  loadd(temp_magicc_50,pair_scen_sel)
  
  temp <- merge(temp_magicc_50, pair_scen_sel, by = c("model","scenario"))
  temp[, year := as.numeric(year)]
  temp <- dcast(temp[delay == "NPi"], model + delay + cbudget + temp_2100_cb ~ poltype,
                value.var = "value",
                fun.aggregate = max)
  
  temp[, diff := full - peak]

  p1 <- ggplot(temp) +
    geom_hline(yintercept = 0, color = "gray") +
    geom_vline(xintercept = c(1.55,1.65,1.8), color = "gray", linetype = "dashed") +
    geom_point(aes(x = temp_2100_cb, y = diff, color = model), size = 2) +
    geom_line(aes(x = temp_2100_cb, y = diff, color = model)) +
    #scale_x_continuous(breaks = c(500,600,700,800,900,1000,1200,1400)) +
    scale_color_npg(name = "Model") +
    #scale_y_continuous(breaks = seq(0,10,by=2)) +
    labs(x = "Temperature increase in 2100 [\u00B0C]", 
         y = "[\u00B0C]") +
    #coord_cartesian(ylim = c(0,10)) +
    theme_bw()
  p1

  ggsave(outfile, plot = p1,width = 8, height = 5)
  
}

plot_temperature <- function(outfile) {
  
  loadd(gmt_hist,temp_magicc_50,pair_scen_sel)
  
  temp <- merge(temp_magicc_50, pair_scen_sel, by = c("model","scenario"))
  temp[, year := as.numeric(year)]
  temp <- dcast(temp, model + delay + temp_2100_cb + year ~ poltype,
                value.var = "value",
                fun.aggregate = mean)
  
  p <- ggplot(temp[delay == "NPi"]) +
    geom_line(aes(x = year, y = full, color = temp_2100_cb, linetype = "Full", group = temp_2100_cb)) +
    geom_line(aes(x = year, y = peak, color = temp_2100_cb, linetype = "Peak", group = temp_2100_cb)) +
    geom_line(aes(x = year, y = gmt), data = gmt_hist) +
    facet_wrap(~ model, nrow = 3) +
    scale_linetype(name = "Scenario design") +
    scale_x_continuous(guide = guide_axis(check.overlap = T)) + 
    scale_color_viridis_c(name = "Temperature in 2100 [\u00B0C]", 
                          breaks = seq(1.3,2, by = 0.1),
                          limits = c(1.25,2)) +
    labs(x = "", y = "Global mean temperature [\u00B0C]") +
    coord_cartesian(xlim = c(2010,2100), ylim = c(0.6,2)) +
    theme_bw() +
    theme(legend.position = "bottom",
          legend.direction = "horizontal",
          legend.key.width = unit(1,"cm"),
          panel.spacing.x = unit(4, "mm"),
          panel.spacing.y = unit(2, "mm"))
  
  ggsave(outfile, width = 10, height = 6)
  
}

plot_temperature_max_cbudget <- function(outfile) {
  
  loadd(pair_scen_sel)
  
  ddtag <- pair_scen_sel[poltype == "peak" & delay == "NPi", 
                         .(temp_2100_cb = temp_2100_cb[length(temp_2100_cb)],
                           cbudget=cbudget[length(cbudget)]), 
                         by = "model"]
  
  ddn <- pair_scen_sel[poltype == "peak" & delay == "NPi", 
                       .(N = .N, tt = mean(temp_2100_cb)), 
                       by = "cluster"]
  
  p1 <- ggplot(pair_scen_sel[poltype == "peak" & delay == "NPi"]) +
    geom_hline(yintercept = c(1.55,1.65,1.8), color = "gray", linetype = "dashed") +
    geom_line(aes(x = cbudget, y = temp_2100_cb, group = model), color = "black", size = 0.5) +
    geom_point(aes(x = cbudget, y = temp_2100_cb, color = cluster), size = 3) +
    geom_text_repel(aes(x = cbudget, y = temp_2100_cb, label = model), 
                    data = ddtag, size = 3, 
                    #ylim =  c(2, NA), 
                    nudge_y      = 0.05,
                    #direction   = "x",
                    #angle       = 90,
                    #vjust       = 0,
                    segment.size = 0.2) +
    geom_text(aes(x = 2200, y = tt, label = paste("N =",N)), 
              data = ddn) +
    scale_colour_viridis_d(name = "Cluster") +
    theme_bw() +
    labs(x = "Cumulative CO2 emissions [GtCO2]", 
         y = "Temperature in 2100 [\u00B0C]") +
    coord_cartesian(ylim = c(1.3,2.2))

  ggsave(outfile, plot = p1,width = 8, height = 5)
  
  
}
