# 1 + 20 cmodels
cmodels <- c(
  "CMIP5",
  "ACCESS1-0", "BNU-ESM", "CCSM4", "CMCC-CMS", "GFDL-CM3",
  "GFDL-ESM2G", "GFDL-ESM2M", "GISS-E2-H", "GISS-E2-H-CC", "GISS-E2-R",
  "GISS-E2-R-CC", "HadGEM2-CC", "HadGEM2-ES", "IPSL-CM5A-LR", "IPSL-CM5A-MR",
  "IPSL-CM5B-LR", "MPI-ESM-LR", "MPI-ESM-MR", "NorESM1-M", "inmcm4"
)

bhm_spec <- c("BHM SR")

dmg_spec_gwt <- c(bhm_spec)

discount_rates <- c(0.01,0.02,0.03)

disc_years <- 2020:2200

clusters <- c("likely 1.5\u00B0C",
              "likely 1.6\u00B0C",
              "below 1.8\u00B0C",
              "below 2\u00B0C")
clusters_map <- clusters[c(1,4)]
clusters2 <- c("[500-700] GtCO2",
               "[800-1000] GtCO2",
               "[1200-1600] GtCO2")

plan <- drake_plan(
  
  # WDI Statistics for calibration
  
  wdi_stat = target(read_fst(file_in("data/wdi_stat.fst"), as.data.table = T)),
  
  # ORIGINAL SSP ASSUMPTIONS
  
  ssp_csv = target("data/SspDb_country_data_2013-06-12.csv",
                   format = "file"),
  ## OECD Population [millions]
  ssp_pop = ssp_query(ssp_csv, "OECD Env-Growth", "Population"),
  ## OECD GDP PPP [billion US$2005/yr]
  ssp_gdp = ssp_query(ssp_csv, "OECD Env-Growth", "GDP|PPP"),
  ## Compute GDPcap for SSP scenario [USD2018/yr]
  ssp_gdpcap = compute_gdp_cap(ssp_gdp, ssp_pop, wdi_stat),
  
  # ENGAGE DB

  ## Global Mean Temperature [C] exp
  temp_magicc_05 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P5",
                             world = TRUE),
  temp_magicc_10 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P10",
                                world = TRUE),
  temp_magicc_25 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P25",
                                world = TRUE),
  temp_magicc_33 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P33",
                                world = TRUE),
  temp_magicc_50 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|MED",
                                world = TRUE),
  temp_magicc_67 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P67",
                                world = TRUE),
  temp_magicc_75 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P75",
                                world = TRUE),
  temp_magicc_90 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P90",
                                world = TRUE),
  temp_magicc_95 = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|P95",
                             world = TRUE),
  temp_magicc_exp = engage_query("AR5 climate diagnostics|Temperature|Global Mean|MAGICC6|Expected value",
                                world = TRUE),
  
  ## GDP|MER["billion US$2010/yr"]
  gdp_mit_mer = engage_query("GDP|MER"),
  ## GDP|PPP["billion US$2010/yr"]
  gdp_mit_ppp = engage_query("GDP|PPP"),
  ## Area under MAC Curve ["billion US$2010/yr"]
  gdp_mit_mac = engage_query("Policy Cost|Area under MAC Curve"),
  ## Area under MAC Curve ["billion US$2010/yr"]
  gdp_mit_nrg = engage_query("Policy Cost|Additional Total Energy System Cost"),
  ## CO2 emissions [MtCO2]
  co2_total = engage_query("Emissions|CO2", world = TRUE),
  ## CH4 emissions
  ch4_total = engage_query("Emissions|CH4", world = TRUE),
  ## N2O emissions
  n2o_total = engage_query("Emissions|N2O", world = TRUE),
  ## f-gases emissions
  fgases_total = engage_query("Emissions|F-Gases", world = TRUE),
  ## f-gases emissions
  kghg_total = engage_query("Emissions|Kyoto Gases", world = TRUE),
  ## BECCS
  beccs = engage_query("Carbon Sequestration|CCS|Biomass", world = TRUE),
  ## Fossil Fuel CCS
  ffccs = engage_query("Carbon Sequestration|CCS|Fossil", world = TRUE),
  ## Land-use|Afforestation
  afforest = engage_query("Carbon Sequestration|Land Use", world = TRUE),
  ## DAC
  dac_ccs = engage_query("Carbon Sequestration|Direct Air Capture", world = TRUE),
  
  ## Information about scenarios
  endb_info = get_scen_info(temp_magicc_50,co2_total,gmt_hist),
  pair_scen = paired_scenarios(endb_info),
  pair_scen_sel = filter_scen(pair_scen),
  
  # Mitigation costs

  ## consolidate GDP REF and GDP MIT ["billion US$2010/yr"]
  gdp_mit = consolidate_gdp_mit(
    gdp_mit_mer,
    gdp_mit_ppp,
    gdp_mit_mac,
    gdp_mit_nrg,
    endb_info
  ),

  # CLIMATE
  
  # temperature distribution
  temp_magicc_prob = gather_temp_dist(temp_magicc_05,
                                      temp_magicc_10,
                                      temp_magicc_25,
                                      temp_magicc_33,
                                      temp_magicc_50,
                                      temp_magicc_67,
                                      temp_magicc_75,
                                      temp_magicc_90,
                                      temp_magicc_95),
  temp_magicc_dist = compute_temp_dist(temp_magicc_prob,
                                       pair_scen_sel),

  ## climate downscaling coefficients
  clim_dwnscl = fread(file_in("data/fit_estimates.csv")),
  clim_dwnscl_alt = fread(file_in("data/fit_estimates_b2000.csv")),
  
  ## Country-level temperature
  climate = target(downscale_climate(clim_dwnscl,
                                     cmodel,
                                     pair_scen_sel,
                                     temp_magicc_prob,
                                     temp_magicc_dist,
                                     num_scen),
                   transform = cross(cmodel = !!cmodels, num_scen = !!num_scens)
  ),
  
  ## Country-level temperature
  climate_alt = target(downscale_climate(clim_dwnscl_alt,
                                     cmodel,
                                     pair_scen_sel,
                                     temp_magicc_prob,
                                     temp_magicc_dist,
                                     num_scen),
                   transform = cross(cmodel = !!cmodels, num_scen = !!num_scens)
  ),

  ## Climate historical timeseries extended until 2019
  clim_hist = read_hist_climate(file_in("data/hist_temp.csv"),
                                climate_CMCC.CMS_1L),

  ## CRU GMT until 2019 wrt 1850-1880
  gmt_hist = read_hist_gmt(),

  # Economic Impact of CC
  dmg_param = target(read_dmg_param(ssp_gdp, wdi_stat),
                     format = "rds"
  ),

  ## Project GDP per capita with climate and aggregate GDP
  gdp_gwt = target(project_cc_impact(
     ssp_gdpcap, clim_hist, climate, "BHM SR", dmg_param),
     transform = map(climate)
  ),

  ## Project GDP per capita with climate and aggregate GDP
  gdp_gwt_alt = target(project_cc_impact(
    ssp_gdpcap, clim_hist, climate_alt, "BHM SR", dmg_param, year_base0 = 1980),
    transform = map(climate_alt)
  ),

  gdp_gwt_s = target(shrink_gdp_engage(gdp_gwt),
                             transform = map(gdp_gwt)),
  
  gdp_gwt_a = target(rbind(gdp_gwt_s),
                         transform = combine(gdp_gwt_s)),

  gdp_gwt_alt_s = target(shrink_gdp_engage(gdp_gwt_alt),
                     transform = map(gdp_gwt_alt)),
  
  gdp_gwt_alt_a = target(rbind(gdp_gwt_alt_s),
                     transform = combine(gdp_gwt_alt_s)),
    
  # Mitigation costs
  cmit_gwt_dmg = target(compute_cmit_years(
     gdp_mit,
     gdp_gwt_a,
     pair_scen_sel)),
  
  cmit_npv_gwt_dmg = target(compute_cmit_npv(
     gdp_mit,
     gdp_gwt_a,
     pair_scen_sel,
     discount_rates
  )),
  
  # Avoided damages - GWT
  admg_gwt_dmg = target(compute_admg_years(
     gdp_gwt_a,
     pair_scen_sel
   )),
  admg_gwt_dmg_alt = target(compute_admg_years(
    gdp_gwt_alt_a,
    pair_scen_sel
  )),
  
  admg_npv_gwt_dmg = target(compute_admg_npv(
     gdp_gwt_a,
     pair_scen_sel,
     discount_rates
   )),
  
   
  # Net benefits - GWT
  nben_gwt_dmg = target(compute_net_benef_years(
     gdp_mit,
     gdp_gwt_a,
     pair_scen_sel
  )),
  
  nben_npv_gwt_dmg = target(compute_net_benef_npv(
     gdp_mit,
     gdp_gwt_a,
     pair_scen_sel,
     discount_rates
  )),
  
  # Net benefits - LEVELS
  gdp_levels = target(project_cc_impact_levels(
     ssp_gdpcap,
     temp_magicc_50,
     temp_magicc_05,
     temp_magicc_95,
     gmt_hist,
     dmg_param)
  ),
  nben_lvl_dmg = target(compute_net_benef_years(
     gdp_mit,
     gdp_levels,
     pair_scen_sel)),
  nben_npv_lvl = target(compute_net_benef_npv(
     gdp_mit,
     gdp_levels,
     pair_scen_sel,
     discount_rates)
  ),
  
  # Avoided damages - LEVELS
  admg_lvl_dmg = target(compute_admg_years(
     gdp_levels,
     pair_scen_sel
  )),
  
  # Geophysical impact of CC
  proxy_impact = read_data_arnell2019cc(),
  
  impact_samp = target(project_cc_impact_arnell_samp(temp_magicc_dist,
                                                     temp_magicc_prob,
                                                     gmt_hist,
                                                     pair_scen_sel,
                                                     proxy_impact,
                                                     iindic,
                                                     iregg,
                                                     clusters,
                                                     interp = 0),
                       dynamic = cross(clusters,iregg,iindic)),
  
  impact_samp2 = target(project_cc_impact_arnell_samp(temp_magicc_dist,
                                                     temp_magicc_prob,
                                                     gmt_hist,
                                                     pair_scen_sel,
                                                     proxy_impact,
                                                     iindic,
                                                     iregg,
                                                     clusters2,
                                                     interp = 0,
                                                     cluster_col = 2),
                       dynamic = cross(clusters2,iregg,iindic)),
  
  impact_samp_map = target(project_cc_impact_arnell_samp(temp_magicc_dist,
                                                         temp_magicc_prob,
                                                         gmt_hist,
                                                         pair_scen_sel,
                                                         proxy_impact,
                                                         iindic_map,
                                                         ireg_map,
                                                         clusters_map,
                                                         interp = 0),
                           dynamic = cross(clusters_map,ireg_map,iindic_map)),
  
  impact_10_1 = project_cc_impact_arnell_samp_raw(temp_magicc_dist,
                                                temp_magicc_prob,
                                                gmt_hist,
                                                pair_scen_sel,
                                                proxy_impact,
                                                iindic[10],
                                                iregg[1],
                                                clusters[1]),
  impact_10_4 = project_cc_impact_arnell_samp_raw(temp_magicc_dist,
                                                  temp_magicc_prob,
                                                  gmt_hist,
                                                  pair_scen_sel,
                                                  proxy_impact,
                                                  iindic[10],
                                                  iregg[1],
                                                  clusters[4]),
  
  impact_ks = target(project_cc_impact_arnell_samp_raw(temp_magicc_dist,
                                                temp_magicc_prob,
                                                gmt_hist,
                                                pair_scen_sel,
                                                proxy_impact,
                                                iindic_id,
                                                ireg00,
                                                clusters[1]),
                     cross(iindic_id = !! iindic_ks, ireg00 = !! (1:6))),
  
  impact_hw = target(project_cc_impact_arnell_samp_raw(temp_magicc_dist,
                                                       temp_magicc_prob,
                                                       gmt_hist,
                                                       pair_scen_sel,
                                                       proxy_impact,
                                                       iindic_id,
                                                       iregg[1],
                                                       cluster),
                     cross(iindic_id = !! iindic_hw, cluster = !! (2:4))),
  
  impact_cr = target(project_cc_impact_arnell_samp_raw(temp_magicc_dist,
                                                       temp_magicc_prob,
                                                       gmt_hist,
                                                       pair_scen_sel,
                                                       proxy_impact,
                                                       iindic_id,
                                                       iregg[1],
                                                       cluster),
                     cross(iindic_id = !! iindic_cr, cluster = !! (2:4))),
  
  impact_exc = target(project_cc_impact_arnell_samp_stat(temp_magicc_dist,
                                                       temp_magicc_prob,
                                                       gmt_hist,
                                                       pair_scen_sel,
                                                       proxy_impact,
                                                       iindic_id,
                                                       iregg,
                                                       cluster),
                     cross(iindic_id = !! iindic_exc, iregg = !! (1:6), cluster = !! (1:4)),
                     format = "rds"),
  
  impact_exc_pb_50 = target(impact_exceed_prob(impact_exc, 0.5),
                         map(impact_exc),
                         format = "rds"),
  
  impact_exc_rejectdt_50 = target(extract_impact_rejection(impact_exc_pb_50),
                             map(impact_exc_pb_50)),
  
  impact_exc_reject_50 = target(rbind(impact_exc_rejectdt_50),
                               combine(impact_exc_rejectdt_50)),

  impact_exc_pb_95 = target(impact_exceed_prob(impact_exc, 0.95),
                            map(impact_exc),
                            format = "rds"),
  
  impact_exc_rejectdt_95 = target(extract_impact_rejection(impact_exc_pb_95),
                                  map(impact_exc_pb_95)),
  
  impact_exc_reject_95 = target(rbind(impact_exc_rejectdt_95),
                                combine(impact_exc_rejectdt_95)),
  
  
  impact_indic_linear = project_cc_impact_arnell(temp_magicc_50,
                                                    gmt_hist,pair_scen_sel,
                                                    proxy_impact),
  impact_indic_spline = project_cc_impact_arnell(temp_magicc_50,
                                                 gmt_hist,pair_scen_sel,
                                                 proxy_impact,interp = 1),
  
  impact_slr = project_cc_slr(temp_magicc_50,
                              temp_magicc_05,
                              temp_magicc_95,
                              gmt_hist,
                              pair_scen_sel),
  
)

plan2 <- drake_plan(
    
  # Paper plot
   
  ## Figure 1: Show difference in temperature
  #fig1 = Panel a : Temperature trajectories
  #       Panel b1 : CO2 emissions
  #       Panel b2 : non-CO2 emissions
  fig1a = plot_show_temperature_gap_endtemp_main(),
  fig1b1 = plot_show_co2emi_gap_endtemp_main(),
  fig1b2 = plot_show_non_co2emi_gap_endtemp_main(),
  fig1 = plot_fig1(fig1a, fig1b1, fig1b2, file_out("results/paper_fig1_temp.pdf")),
  fig1_si1 = plot_show_co2emi_gap_endtemp_main_panel(file_out("results/paper_fig1_si_totco2.pdf")),
  fig1_si2 = plot_show_non_co2emi_gap_endtemp_main_panel(file_out("results/paper_fig1_si_nonco2.pdf")),
  
  ## Table 1: geophysical impacts
  summary_impact = analysis_impact_arnell(file_out("results/summary_impact.csv")),
   
  # Figure 2-3: Selection of non-economic impacts
  fig2a = compare_max_dist_heatwave(file_out("results/paper_fig2a.pdf")),
  fig2c = matrix_significant_days(file_out("results/paper_fig2c.pdf")),
  fig2b = plot_exceed_prod_fig2(file_out("results/paper_fig2b.pdf")),
   
  # Figure 3: Avoided damages
  fig3a = plot_admg_overshoot2("BHM SR"),
  fig3b = plot_admg_overshoot2("HS2017 NCAT"),
  fig3c = plot_admg_overshoot2("TAKAKURA2019"),
  fig3 = target(plot_admg_overshoot2_combine(fig3a,fig3b,fig3c,
                                             file_out("results/paper_fig3_admg.pdf"))),

  fig3a1 = plot_admg_overshoot2_perc("BHM SR"),
  fig3b1 = plot_admg_overshoot2_perc("HS2017 NCAT"),
  fig3c1 = plot_admg_overshoot2_perc("TAKAKURA2019"),
  fig31 = plot_admg_overshoot2_combine(fig3a1,fig3b1,fig3c1,
                                       file_out("results/paper_fig3_admg_perc.pdf")),
  
     
  fig3a_sa = plot_admg_overshoot2_alt(1),
  fig3b_sa = plot_admg_overshoot2_alt(2),
  fig3_sa = plot_admg_overshoot2_combine_alt(fig3a_sa,fig3b_sa,
                                   file_out("results/paper_fig3_admg_sa.pdf")),
  
  # Figure 4: Net benefits
  fig4a = plot_cba_overshoot2("BHM SR"),
  fig4b = plot_cba_overshoot2("HS2017 NCAT"),
  fig4c = plot_cba_overshoot2("TAKAKURA2019"),
  fig4 = plot_cba_overshoot2_combine(fig4a,fig4b,fig4c,
                                                      file_out("results/paper_fig4_nben.pdf")),

  fig4a2 = plot_cba_overshoot2_cluster2("BHM SR"),
  fig4b2 = plot_cba_overshoot2_cluster2("HS2017 NCAT"),
  fig4c2 = plot_cba_overshoot2_cluster2("TAKAKURA2019"),
  fig42 = plot_cba_overshoot2_combine(fig4a2,fig4b2,fig4c2,
                                     file_out("results/paper_fig4_si_nben_cluster2.pdf")),
  
  fig4a_sa = plot_cba_overshoot2_alt(1),
  fig4b_sa = plot_cba_overshoot2_alt(2),
  fig4_sa = plot_cba_overshoot2_combine_alt(fig4a_sa,fig4b_sa,
                                     file_out("results/paper_fig4_nben_sa.pdf")),
    
  # Table SI
  # List of select models for the 2 exercises
   
  # Figure SI: Plot diff temperature.
  fig1_s1 = plot_diff_temperature("final", file_out("results/paper_fig1_si_diff_time.pdf")),
  fig1_s2 = plot_temperature_integral(file_out("results/paper_fig1_si_time_integration.pdf")),
  fig1_s4 = plot_temperature(file_out("results/paper_fig1_si_temp_all.pdf")),
  fig1_s5 = plot_temperature_max(file_out("results/paper_fig1_si_temp_max.pdf")),
  fig1_s6 = plot_temperature_max_cbudget(file_out("results/paper_fig1_si_temp_max_cb.pdf")),
   
  fig1_s7a = plot_show_temperature_gap_endtemp_cluster(1,file_out("results/paper_fig1_si_cluster1.pdf")),
  fig1_s7b = plot_show_temperature_gap_endtemp_cluster(2,file_out("results/paper_fig1_si_cluster2.pdf")),
  fig1_s7c = plot_show_temperature_gap_endtemp_cluster(3,file_out("results/paper_fig1_si_cluster3.pdf")),
  fig1_s7d = plot_show_temperature_gap_endtemp_cluster(4,file_out("results/paper_fig1_si_cluster4.pdf")),
  
  # # Figures SI: Non-economic impacts
  fig2_s3l = plot_comp_impact_physical(), 
  fig2_s3 = plot_all_global_impact_physical(fig2_s3l,file_out("results/paper_fig2_si_fp.pdf")),
  fig2_s6 = map_impact_heatwave(file_out("results/paper_fig2_map_heatwave.pdf")),
  fig2_s1 = plot_exceed_prod_15C(file_out("results/paper_fig2_exceedence_15C.pdf")),
  fig2_s7l = plot_list_diff_global_impact_physical(),
  fig2_s7 = plot_all_global_impact_physical(fig2_s7l,file_out("results/paper_fig2_si_sa_interp.pdf")),
  
  fig2_s4 = plot_slr(file_out("results/paper_fig2_si_slr.pdf")),
  fig2_s5 = plot_slr_overshoot(file_out("results/paper_fig2_si_slr_overshoot.pdf")),
  fig2_s8l = plot_comp_impact_physical2(), 
  fig2_s8 = plot_all_global_impact_physical(fig2_s8l,file_out("results/paper_fig2_si_fp_cluster2.pdf")),
  
  # # # Figures SI: Economic impacts
  fig4_s1 = target(plot_cmit_overshoot(file_out("results/paper_fig4_si_cmit_overshoot.pdf"))),
   
  fig4_s3a = plot_admg_overshoot_all("BHM SR"),
  fig4_s3b = plot_admg_overshoot_all("HS2017 NCAT"),
  fig4_s3c = plot_admg_overshoot_all("TAKAKURA2019"),
  fig4_s3 = plot_admg_overshoot_all_combine(fig4_s3a,fig4_s3b,fig4_s3c,
                                            file_out("results/paper_fig4_si_cmip5_admg.pdf")),
   
  fig4_s5a = plot_cba_overshoot2_all("BHM SR"),
  fig4_s5b = plot_cba_overshoot2_all("HS2017 NCAT"),
  fig4_s5c = plot_cba_overshoot2_all("TAKAKURA2019"),
  fig4_s5 = plot_cba_overshoot_all_combine(fig4_s5a,fig4_s5b,fig4_s5c,
                                                   file_out("results/paper_fig4_si_cmip5_cba2.pdf")),
  
  fig4_s9a = plot_cba_overshoot2_npv("BHM SR"),
  fig4_s9b = plot_cba_overshoot2_npv("HS2017 NCAT"),
  fig4_s9c = plot_cba_overshoot2_npv("TAKAKURA2019"),
  fig4_s9 = plot_cba_overshoot_all_combine(fig4_s9a,fig4_s9b,fig4_s9c,
                                           file_out("results/paper_fig4_si_cba2_npv.pdf")),
   
  fig4_sxa = plot_cba_overshoot2_npv_scen("BHM SR"),
  fig4_sxb = plot_cba_overshoot2_npv_scen("HS2017 NCAT"),
  fig4_sxc = plot_cba_overshoot2_npv_scen("TAKAKURA2019"),
  fig4_sx = plot_cba_overshoot_all_combine(fig4_sxa,fig4_sxb,fig4_sxc,
                                           file_out("results/paper_fig4_si_cba2_npv_scen.pdf")),
  
  fig4_s6a = plot_cba2_all("BHM SR"),
  fig4_s6b = plot_cba2_all("HS2017 NCAT"),
  fig4_s6c = plot_cba2_all("TAKAKURA2019"),
  fig4_s6 = plot_cba_overshoot_all_combine(fig4_s6a,fig4_s6b,fig4_s6c,
                                                   file_out("results/paper_fig4_si_cmip5_nbenef.pdf")),
   
  fig4_s6_a = plot_cba2_cmit(file_out("results/paper_fig4_si_cmit.pdf")),
  
  # Additional impact plots
  impact_info_1_1 = plot_impact_info_1(1,str_glue("results/paper_fig2_si_impacts_order_Globe.pdf")),
  impact_info_1_2 = plot_impact_info_1(2,str_glue("results/paper_fig2_si_impacts_order_Africa.pdf")),
  impact_info_1_3 = plot_impact_info_1(3,str_glue("results/paper_fig2_si_impacts_order_Europe.pdf")),
  impact_info_1_4 = plot_impact_info_1(4,str_glue("results/paper_fig2_si_impacts_order_North_America.pdf")),
  impact_info_1_5 = plot_impact_info_1(5,str_glue("results/paper_fig2_si_impacts_order_South_America.pdf")),
  impact_info_1_6 = plot_impact_info_1(6,str_glue("results/paper_fig2_si_impacts_order_Asia.pdf")),
  
  impact_info_2_1 = plot_impact_info_2(str_glue("results/paper_fig2_si_impacts_global_heatwave_full.pdf")),

  impact_info_3_1 = plot_impact_info_3(str_glue("results/paper_fig2_si_impacts_global_cropred_full.pdf")),
  
    
  impact_plt1 = plot_impact_arnell_function(file_out("results/paper_fig2_si_imp_function_linear.pdf")),
  impact_plt2 = plot_impact_arnell_function(file_out("results/paper_fig2_si_imp_function_spline.pdf"),
                                            interp = 1),
  impact_plt3 = plot_impact_arnell_function_diff(file_out("results/paper_fig2_si_imp_function_diff.pdf")),
  
  diag_pi = plot_illus_temp_dist(file_out("results/paper_fig_si_diag_rng_ill.pdf")),
  
  netemi = plot_em_negative(file_out("results/paper_fig_si_net_emi.pdf")),
  map_cb_temp = plot_cb_temp(file_out("results/paper_fig_si_cb_temp.pdf"))

)
