
R10reg_long <- c("Countries of Sub-Saharan Africa",
                 "Countries of centrally-planned Asia; primarily China",
                 "Eastern and Western Europe (i.e., the EU28)",
                 "Countries of South Asia; primarily India",
                 "Countries of Latin America and the Caribbean",
                 "Countries of the Middle East; Iran, Iraq, Israel, Saudi Arabia, Qatar, etc.",
                 "North America; primarily the United States of America and Canada",
                 "Pacific OECD",
                 "Countries from the Reforming Ecomonies of the Former Soviet Union",
                 "Other countries of Asia")

R10reg_long <- c("R10AFRICA",
                 "R10CHINA+",
                 "R10EUROPE",
                 "R10INDIA+",
                 "R10LATIN_AM",
                 "R10MIDDLE_EAST",
                 "R10NORTH_AM",
                 "R10PAC_OECD",
                 "R10REF_ECON",
                 "R10REST_ASIA")

R10reg_short <- c("AFRICA",
                  "CHINA+", 
                  "EUROPE",
                  "INDIA+",
                  "LATIN_AM",
                  "MIDDLE_EAST",
                  "NORTH_AM",
                  "PAC_OECD",
                  "REF_ECON",
                  "REST_ASIA")

R10reg <- data.table(short_name = c(R10reg_short,"World"),
                     long_name = c(R10reg_long,"World"))

engage_query <- function(query_var, world = FALSE) {
  
  engage_csv <- "data/output/snapshot_world.csv"
  
  csv_header <- tolower(unlist(fread(engage_csv, nrows = 1, header = F),
                               use.names = F))
  header_ids <- csv_header[1:5]
  coltype <- c(rep("character", 5), rep("numeric", length(csv_header) - 5))
  
  dd <- fread(str_glue("grep -e ',{query_var},' {engage_csv}"), 
              col.names = csv_header,
              colClasses = coltype)
  dd <- long_table(dd, ids = header_ids)
  dd <- dd[!is.na(value)]
  
  dd <- dd[region == "World", .(model, scenario, region = "World", year, value)]
  setkey(dd, model, scenario, region, year)
  
  if (!world) {
    
    engage_csv <- "data/output/snapshot_Compare_R10.csv"
    csv_header <- tolower(unlist(fread(engage_csv, nrows = 1, header = F),
                                 use.names = F))
    header_ids <- csv_header[1:5]
    coltype <- c(rep("character", 5), rep("numeric", length(csv_header) - 5))
    
    dd0 <- fread(str_glue("grep -e ',{query_var},' {engage_csv}"), 
                col.names = csv_header,
                colClasses = coltype)
    dd0 <- long_table(dd0, ids = header_ids)
    dd0 <- dd0[!is.na(value)]

    dd0 <- dd0[region %in% c("World",R10reg_long), .(model, scenario, region, year, value)]
    
    dd <- merge(dd,R10reg,by.x = "region",by.y = "long_name")
    dd[, region := short_name]
    dd[, short_name := NULL]
    setkey(dd, model, scenario, region, year)
    
  }

  dd <- dd[model != "Reference"]
  
  return(dd)

}

get_scen_info <- function(temp_magicc,co2_total,gmt_hist) {
  
  dd <- unique(temp_magicc[,.(model,scenario)]) 
  dd <- dd[!str_detect(scenario,"NDC")]
  dd <- dd[!str_detect(scenario,"_COV")]
  dd <- dd[!str_detect(scenario,"_DR")]
  dd[,cbudget := as.numeric(str_sub(str_extract(scenario,'_\\d+'),2,-1))]
  dd[,poltype := ifelse(is.na(cbudget),'ref','peak')]
  dd[,poltype := ifelse(str_detect(scenario,'0f$'),'full',poltype)]
  dd[,poltype := ifelse(str_detect(scenario,'0r$'),'reg',poltype)]
  dd[,delay   := ifelse(str_detect(scenario,'EN_INDCi'),'NDC',NA)]
  dd[,delay   := ifelse(str_detect(scenario,'NDCp'),'NDCp',delay)]
  dd[,delay   := ifelse(str_detect(scenario,'EN_NPi'),'NPi',delay)]
  dd[,delay   := ifelse(str_detect(scenario,'EN_NoPolicy'),'no',delay)]
  
  # Exclude reg poltype
  dd <- dd[poltype != "reg"]
  
  # Compute cumulative emissions
  cbudget_2019_2100 <- function(SD) {
    return(sum(approx(x = SD$year,y = SD$value,xout = 2019:2100)[['y']]) * 1e-3)
  }
  comp_cbudget <- co2_total[, .(cum_co2tot = cbudget_2019_2100(.SD)), by = c("model","scenario")]
  dd <- merge(dd, comp_cbudget, by = c("model","scenario"))

  dd <- merge(dd, temp_magicc[year == 2100, .(model,scenario,temp_2100 = value)]) # unbiased temperature
  
  dd <- merge(dd, temp_magicc[, .(temp_max = max(value)), by = "model,scenario"]) # unbiased temperature

  return(dd)
  
}


paired_scenarios <- function(endb_info) {
  
  # Keep interested model
  dd <- endb_info[model != "Reference"]
  dd <- endb_info[poltype %in% c("peak","full")]

  # Check NPi pairs
  pairs <- dd[poltype %in% c("peak","full"), .(model,delay,cbudget,poltype,cum_co2tot)]
  pairs <- dcast(pairs, model + delay + cbudget ~ poltype, 
                 value.var = 'cum_co2tot', 
                 fun.aggregate = sum)
  pairs <- pairs[peak > 0]
  pairs <- pairs[full > 0]
  
  # find the model scenarios in original db
  dd <- merge(pairs[,.(model,delay,cbudget)], 
              endb_info[poltype %in% c("peak","full")],
              by = c("model","delay","cbudget"))
  dd[, temp_2100_cb := mean(temp_2100), by = "model,delay,cbudget"]
  dd[, overshoot_temp_max := abs(temp_max[1] - temp_max[2]), by = "model,delay,cbudget"]
  dd[, temp_max_peak := min(temp_max), by = "model,delay,cbudget"]
  # check
  dd[, overshoot_temp_max_2100 := max(temp_max) - temp_2100_cb, by = "model,delay,cbudget"]

  return(dd)

}

filter_scen <- function(pair_scen) {
  
  dd <- pair_scen

  dd <- dd[!(model == "TIAM-ECN 1.1" & scenario == "EN_NPi2020_600")] # diff cbudget peak-full off
  dd <- dd[!(model == "TIAM-ECN 1.1" & scenario == "EN_NPi2020_600f")] 
  dd <- dd[!(model == "IMAGE 3.0" & scenario == "EN_NPi2020_1200f")] # diff cbudget peak-full off
  dd <- dd[!(model == "IMAGE 3.0" & scenario == "EN_NPi2020_1200")] 
  dd <- dd[!(model == "POLES ENGAGE" & scenario == "EN_NPi2020_1000f")] # diff cbudget peak-full off 
  dd <- dd[!(model == "POLES ENGAGE" & scenario == "EN_NPi2020_1000")]
  
  dd <- dd[!(model == "TIAM-ECN 1.1" & scenario == "EN_NPi2020_500")] # unrealistic CCS in 2020
  dd <- dd[!(model == "TIAM-ECN 1.1" & scenario == "EN_NPi2020_500f")] 
  dd <- dd[!(model == "TIAM-ECN 1.1" & scenario == "EN_NPi2020_700")] # unrealistic CCS in 2020
  dd <- dd[!(model == "TIAM-ECN 1.1" & scenario == "EN_NPi2020_700f")] 
  
  # Clusters
  
  # Only NPi
  dd <- dd[delay == "NPi"]
  
  # Selection of a range of carbon budget
  dd <- dd[cbudget >= 500]
  
  # cbudget below 2C for peak temperature
  dd <- dd[temp_max_peak <= 2]
  
  # Create Temperature cluster
  dd[, cluster := ifelse(temp_2100_cb <= 2, clusters[4], NA)]
  dd[, cluster := ifelse(temp_2100_cb <= 1.8, clusters[3], cluster)]
  dd[, cluster := ifelse(temp_2100_cb <= 1.67, clusters[2], cluster)]
  dd[, cluster := ifelse(temp_2100_cb <= 1.57, clusters[1], cluster)]

  # create carbon budget clusters
  dd[, cluster2 := ifelse(cbudget >= 1200, clusters2[3], NA)]
  dd[, cluster2 := ifelse(cbudget < 1200, clusters2[2], cluster2)]
  dd[, cluster2 := ifelse(cbudget < 800, clusters2[1], cluster2)]

  return(dd)
  
}

# Consolidate GDP mitigation from ENGAGE database
consolidate_gdp_mit <- function(gdp_mit_mer, gdp_mit_ppp, gdp_mit_mac, gdp_mit_nrg, endb_info) {
  
  # Take GDP|MER by default
  iams <- c("AIM/CGE V2.2","GEM-E3_V2021","MESSAGEix-GLOBIOM_1.1",
            "REMIND-MAgPIE 2.1-4.2","WITCH 5.0")
  gdp1 <- gdp_mit_mer[scenario %in% unique(endb_info$scenario) & 
                        model %in% iams]
  gdp1 <- gdp1[scenario != "EN_NoPolicy"]
  gdp1_ref <- gdp1[
    scenario == "EN_NPi2100",
    .(model, scenario_ref = scenario, region, year, value_ref = value)
  ]
  gdp1 <- gdp1[scenario != "EN_NPi2100"]
  gdp1 <- merge(gdp1, gdp1_ref, by = c("model", "region", "year"))

  # Approximate GDP loss using Area under MAC Curve
  iams <- c("IMAGE 3.0")
  gdp2 <- gdp_mit_mac[scenario %in% unique(endb_info$scenario) & 
                         model %chin% iams]
  gdp2 <- gdp2[scenario != "EN_NoPolicy"]
  gdp2_ref <- gdp_mit_mer[scenario == "EN_NPi2100" & 
                            model %in% iams, 
                     .(model, scenario_ref = scenario, region, year, value_ref = value)]
  gdp2 <- gdp2[scenario != "EN_NPi2100"]
  gdp2 <- merge(gdp2, gdp2_ref, by = c("model", "region", "year"))
  gdp2[, gdp_mit := value_ref - value]
  
  # Approximate GDP loss using additional energy system cost
  iams <- c("TIAM-ECN 1.1","COFFEE 1.1")
  gdp3 <- gdp_mit_nrg[scenario %in% unique(endb_info$scenario) & model %chin% iams]
  gdp3 <- gdp3[scenario != "EN_NoPolicy"]
  gdp3_ref <- gdp_mit_ppp[model %in% iams & scenario == "EN_NPi2100",
                             .(value_ref = mean(value)),
                             by = "region,scenario,year"]
  setnames(gdp3_ref,"scenario","scenario_ref")
  gdp3 <- gdp3[scenario != "EN_NPi2100"]
  gdp3 <- merge(gdp3, gdp3_ref, by = c("region", "year"))
  gdp3[, gdp_mit := value_ref - value]

  # Harmonize tables
  gdp1 <- gdp1[, .(model, scenario, region, year, gdp_ref = value_ref, gdp_mit = value)]
  gdp2 <- gdp2[, .(model, scenario, region, year, gdp_ref = value_ref, gdp_mit)]
  gdp3 <- gdp3[, .(model, scenario, region, year, gdp_ref = value_ref, gdp_mit)]
  
  return(rbindlist(list(gdp1, gdp3)))
}
