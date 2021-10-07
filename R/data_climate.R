nbswc <- 333
num_scens <- 1:127

downscale_climate <- function(clim_dwnscl,
                              cmodel,
                              pair_scen_sel,
                              temp_magicc_prob,
                              temp_magicc_dist,
                              num_scen) {
  
   
  estim <- clim_dwnscl[mo == cmodel][r.squared > 0.5]
  
  modscens <- unique(temp_magicc_dist[,c(paste(model,scenario))])[num_scen]
  
  ttft <- temp_magicc_dist[paste(model,scenario) %in% modscens & year >= 2010 & year <= 2100]
  ttpb <- temp_magicc_prob[paste(model,scenario) %in% modscens & year >= 2010 & year <= 2100,
                           .(model,scenario,year,param=prob,value)]
  tt <- rbindlist(list(ttft,ttpb))
  tt <- dcast(tt, model + scenario + year ~ param)
  
  tt_2005 <- temp_magicc_prob[paste(model,scenario) %in% modscens & year == 2005]
  tt_2005 <- dcast(tt_2005, model + scenario + year ~ prob)

  # sw
  set.seed(42)
  pp <- c(0.05,0.1,0.25,0.33,0.5,0.67,0.75,0.9,0.95)
  p0 <- runif(nbswc)

  tt_2020 <- tt[year < 2020, .( sw = 1:nbswc, 
                                value = spline(x = pp,
                                y = c(`0.05`,`0.1`,`0.25`,`0.33`,`0.5`,`0.67`,`0.75`,`0.9`,`0.95`),
                                xout = p0)$y), by = .(model,scenario,year)]

  p05 <- p0[p0 < 0.05]
  tt_p05 <- tt[year >= 2020, .( sw = which(p0 < 0.05),value = qnorm(p05,l_mean,l_sd)), 
               by = .(model,scenario,year)]
  
  pmid <- p0[p0 >= 0.05 & p0 <= 0.95]
  tt_pmid <- tt[year >= 2020, .( sw = which(p0 >= 0.05 & p0 <= 0.95), 
                                 value = spline(x = pp,
                                 y = c(`0.05`,`0.1`,`0.25`,`0.33`,`0.5`,`0.67`,`0.75`,`0.9`,`0.95`),
                                 xout = pmid)$y), by = .(model,scenario,year)]

  p95 <- p0[p0 > 0.95]
  tt_p95 <- tt[year >= 2020, .( sw = which(p0 > 0.95),
                                value = qlnorm(p95,r_meanlog,r_sdlog)), 
               by = .(model,scenario,year)]

  ttt_2005 <- tt_2005[, .( sw = 1:nbswc,
                           value_2005 = spline(x = pp,
                              y = c(`0.05`,`0.1`,`0.25`,`0.33`,`0.5`,`0.67`,`0.75`,`0.9`,`0.95`),
                              xout = p0)$y), by = .(model,scenario)]

  ttt <- rbindlist(list(tt_2020,tt_p05, tt_pmid, tt_p95))
    
  ttt <- merge(ttt, ttt_2005, by = c("model", "scenario", "sw"))
  
  setkey(ttt, model, scenario, year, sw)
  
  # Compute projected probabilistic temperature at country-level
  tas <- mapply(function(gmt, gmt_2005) estim$temp * (gmt - gmt_2005) + estim$inter, 
                ttt$value, ttt$value_2005, 
                SIMPLIFY = FALSE, USE.NAMES = FALSE) %>%
    unlist(use.names = FALSE) 
  
  # Add indexes
  clim = data.table(tas = tas)
  clim[, c5model := which(cmodels == cmodel)]
  clim[, iso3 := rep(estim$reg, times = nrow(ttt))]
  clim[, model := rep(ttt$model, each = nrow(estim))]
  clim[, scenario := rep(ttt$scenario, each = nrow(estim))]
  clim[, year := rep(ttt$year, each = nrow(estim))]
  clim[, sw := rep(ttt$sw, each = nrow(estim))]
  
  setkey(clim, c5model, model, scenario, iso3, year, sw)
    
  return(clim)
  
}

read_hist_climate <- function(clim_hist_csv, climate) {
  
  # load historical temp 1980-2017
  dd <- fread(clim_hist_csv)
  
  # select iso3 and columns
  dd <- dd[iso3 %in% unique(climate$iso3), .(year,iso3,tas = tas_pop_wgt_mean_hist)]

  return(dd)
    
}

read_hist_gmt <- function() {
  
  hadcrut4gl <- fread("https://crudata.uea.ac.uk/cru/data/temperature/HadCRUT4-gl.dat", fill = TRUE)
  
  # keep year and annual value
  cru_ann <- hadcrut4gl[!is.na(V14), c(1, 14)]
  setnames(cru_ann, c("V1", "V14"), c("year", "gmt"))
  
  # remove current year (likely incomplete)
  cru_ann <- cru_ann[year < 2020]
  
  # average 1850-1880
  ref_preind <- cru_ann[year >= 1850 & year <= 1880, mean(gmt)]
  
  # compute gmt from preind
  cru_ann[, gmt := gmt - ref_preind]
  
  return(cru_ann)
}

## TEMPERATURE DIST
gather_temp_dist <- function(temp_magicc_05,
                              temp_magicc_10,
                              temp_magicc_25,
                              temp_magicc_33,
                              temp_magicc_50,
                              temp_magicc_67,
                              temp_magicc_75,
                              temp_magicc_90,
                              temp_magicc_95) {
  
  temp_magicc_05[, prob := 0.05]
  temp_magicc_10[, prob := 0.10]
  temp_magicc_25[, prob := 0.25]
  temp_magicc_33[, prob := 0.33]
  temp_magicc_50[, prob := 0.50]
  temp_magicc_67[, prob := 0.67]
  temp_magicc_75[, prob := 0.75]
  temp_magicc_90[, prob := 0.90]
  temp_magicc_95[, prob := 0.95]
  
  temp_dist <- rbindlist(list(temp_magicc_05,
                              temp_magicc_10,
                              temp_magicc_25,
                              temp_magicc_33,
                              temp_magicc_50,
                              temp_magicc_67,
                              temp_magicc_75,
                              temp_magicc_90,
                              temp_magicc_95))
  
  setkey(temp_dist,model,scenario,region,year,prob)
  
  return(temp_dist)
  
}

compute_temp_dist <- function(temp_magicc_prob,
                      pair_scen_sel) {

  # Reduce the scope of the distribution
  temp_dist <- merge(pair_scen_sel,temp_magicc_prob,by = c("model","scenario"))
  temp_dist <- rbindlist(list(temp_dist,temp_magicc_prob[str_detect(scenario,"NPi2100$")]), fill = TRUE)
  temp_dist <- temp_dist[year >= 2020]
  
  dd_left <- temp_dist[prob < 0.25, .(param = c("l_mean","l_sd","l_obj"), 
                    value = c(suppressMessages(get.norm.par(p = prob,
                                                            q = value,
                                                            show.output = FALSE, 
                                                            plot = FALSE)))),
     by = c("model","scenario","year")]
  
  dd_right <- temp_dist[prob > 0.75, 
                       .(param = c("r_meanlog","r_sdlog","r_obj"), 
                        value = c(suppressMessages(get.lnorm.par(p = prob, 
                                                                 q = value,
                                                                 show.output = FALSE, 
                                                                 plot = FALSE)))),
                        by = c("model","scenario","year")]
  # detect bad fit and retry
  bad_fit <- dd_right[param == "r_meanlog" & value < 0, paste(model,scenario,year)]
  dd_right <- dd_right[!paste(model,scenario,year) %in% bad_fit]
  
  dd_right_01 <- temp_dist[prob >= 0.75 & (paste(model,scenario,year) %in% bad_fit), 
                           .(param = c("r_meanlog","r_sdlog","r_obj"), 
                             value = c(suppressMessages(get.lnorm.par(p = prob, 
                                                                      q = value,
                                                                      show.output = FALSE, 
                                                                      plot = FALSE)))),
                           by = c("model","scenario","year")]
  
  return(rbindlist(list(dd_left,dd_right,dd_right_01)))
  
}

check_temp_dist <- function(perc) {
  
  loadd(temp_magicc_dist)
  loadd(temp_magicc_prob)
   
  temp_magicc_perc = readd(target = paste0("temp_magicc_",perc), character_only = TRUE)
  
   rdist <- function(n,mean,sd,meanlog,sdlog,pp,qq) {
     p = runif(n)
     return(c(qnorm(p[p<=0.05],mean,sd),
              #approx(pp,qq,p[p > 0.05 & p < 0.95])$y,
              spline(pp,qq,xout = p[p > 0.05 & p < 0.95])$y,
              qlnorm(p[p>=0.95],meanlog,sdlog)))
   }
   
   params <- dcast(temp_magicc_dist, model + scenario + year ~ param, sum)
   dist <- dcast(temp_magicc_prob, model + scenario + year ~ prob, sum)
   params <- merge(params,dist, by = c("model","scenario","year"))
     
   if (perc == "exp") {
     agg <- function(x) {mean(x)}
   } else {
     qq <- as.numeric(perc) / 100
     agg <- function(x) {quantile(x,qq)}
   }
   
   pp <- params[, .(p_dist = agg(rdist(1e5,l_mean,l_sd,r_meanlog,r_sdlog,
                                       c(0.05,0.1,0.25,0.33,0.5,
                                         0.67,0.75,0.9,0.95),
                                       c(`0.05`,`0.1`,`0.25`,`0.33`,`0.5`,
                                         `0.67`,`0.75`,`0.9`,`0.95`)))),
                by = c("model","scenario","year")]
   dd <- merge(pp, temp_magicc_perc, by = c("model","scenario","year"))
   dd[, diff := p_dist - value]
   return(dd)
   
}


plot_illus_temp_dist <- function(outfile) {
  
  loadd(temp_magicc_dist)
  loadd(temp_magicc_prob)
 
  dd1 <- temp_magicc_prob[model == "WITCH 5.0" & year == 2040 & scenario == "EN_NPi2020_1000"]
  dd2 <- temp_magicc_dist[model == "WITCH 5.0" & year == 2040 & scenario == "EN_NPi2020_1000"]
  
  pb <- c(0.001,0.005,seq(0.01,0.99, by = 0.01),0.995,0.999)
  
  dd0 <- data.table(prob = pb, value = c(qnorm(pb[pb<=0.05],
                                       dd2[param=="l_mean",value],
                                       dd2[param=="l_sd",value]),
                                 spline(dd1$prob,
                                        dd1$value,
                                        xout = pb[pb > 0.05 & pb < 0.95])$y,
                                 qlnorm(pb[pb>=0.95],
                                        dd2[param=="r_meanlog",value],
                                        dd2[param=="r_sdlog",value])))
  
  p1 <- ggplot() +
    geom_hline(aes(yintercept = c(0,1))) +
    geom_line(aes(x = value, y = prob), data = dd0,
              linetype = 2, size = 0.75, 
              alpha = 1, color = "blue") +
    geom_point(aes(x = value, y = prob), data = dd1,
               size = 3, alpha = 0.66, color = "red") +
    scale_x_continuous(expand = c(0,0)) +
    scale_y_continuous(expand = c(0.01,0.01)) +
    labs(x = "Global mean temperature [\u00B0C]",
         y = "Probability P(T<t)",
         title = "Cumulative distribution") +
    theme_bw() 
  p1
  
  rdist <- function(n,mean,sd,meanlog,sdlog,pp,qq) {
    p = runif(n)
    return(c(qnorm(p[p<=0.05],mean,sd),
             #approx(pp,qq,p[p > 0.05 & p < 0.95])$y,
             spline(pp,qq,xout = p[p > 0.05 & p < 0.95])$y,
             qlnorm(p[p>=0.95],meanlog,sdlog)))
  }
  
  samp <- rdist(1e6,
                dd2[param=="l_mean",value],
                dd2[param=="l_sd",value], 
                dd2[param=="r_meanlog",value],
                dd2[param=="r_sdlog",value],
                dd1$prob,
                dd1$value)
  
  p2 <- ggplot(data.frame(x=samp), aes(x=x)) +
    geom_histogram(aes(y = ..density..), colour = "black", fill = "white", bins = 100) +
    #geom_density(alpha = 0.6, fill = "#FF6666", n = 50) +
    scale_x_continuous(expand = c(0,0), limits = c(min(dd0$value), max(dd0$value))) +
    scale_y_continuous(expand = c(0.01,0.01)) +
    labs(x = "Global mean temperature [\u00B0C]",
         y = "",
         title = "Density") +
    theme_bw() 
  p2
  
  pp <- p1 + p2 + 
    plot_annotation(tag_levels = 'a')
  pp
  
  ggsave(outfile, plot = pp, width = 10, height = 5)
  
}

## Override fitting functioncfrom rriskDistributions for faster optimization
get.lnorm.par <- function (p = c(0.025, 0.5, 0.975), q, show.output = FALSE, plot = FALSE, 
                           tol = 0.001, fit.weights = rep(1, length(p)), scaleX = c(0.1, 
                                                                                    0.9), ...) 
{
  if (length(p) != length(q) | length(p) != length(fit.weights) | 
      length(q) != length(fit.weights)) {
    stop("INVALID INPUT, 'p', 'q' and 'fit.weights' are not of the same length! The vectors of quantiles, probabilities and weightings should be of the same length.", 
         call. = FALSE)
  }
  if (prod(order(p) == seq(1:length(p))) == 0 | prod(order(q) == 
                                                     seq(1:length(q))) == 0) {
    stop("INVALID INPUT, the vector of probabilities/percentiles is not ordered!", 
         call. = FALSE)
  }
  if (min(p) < 0 | max(p) > 1) {
    stop("INVALID INPUT, items of the probability vector should lie between 0 and 1!", 
         call. = FALSE)
  }
  if (min(q) <= 0) {
    stop("INVALID INPUT, percentiles are out of the domain (0, inf) => Lognormal distribution couldn't be fitted!", 
         call. = FALSE)
  }
  if (length(p) != length(q) | length(p) != length(fit.weights) | 
      length(q) != length(fit.weights)) {
    stop("INVALID INPUT, 'p', 'q' and 'fit.weights' are not of the same length! The vectors of quantiles, probabilities and weightings should be of the same length.", 
         call. = FALSE)
  }
  if (length(q) < 2) {
    stop("INVALID INPUT, at least two quantiles must be known!", 
         call. = FALSE)
  }
  if (!is.logical(show.output)) {
    stop("INVALID INPUT, the argument 'show.output' should be logical!", 
         call. = FALSE)
  }
  if (!is.logical(plot)) {
    stop("INVALID INPUT, the argument 'plot' should be logical!", 
         call. = FALSE)
  }
  if (!is.numeric(tol) | length(tol) != 1 | tol < 0) {
    stop("INVALID INPUT, the argument 'tol' should be a single positive numerical value!", 
         call. = FALSE)
  }
  fit.weights.original <- fit.weights
  fit.weights <- fit.weights/sum(fit.weights)
  minimize <- function(theta) {
    summand <- suppressWarnings(stats::plnorm(q = q, meanlog = theta[1], 
                                              sdlog = theta[2]) - p)
    summand <- summand * fit.weights
    sum(summand^2)
  }
  fit <- c()
  fit$value <- tol + 1
  try1 <- try(fit <- stats::optim(par = c(1, 0.35), minimize, 
                                  method = "L-BFGS-B", lower = c(-10000, 0.001), upper = c(10000, 
                                                                                           10000)), silent = TRUE)
  if (fit$value >= tol) {
    warning("The fitting procedure 'L-BFGS-B' has failed (convergence error occurred or specified tolerance not achieved)!", 
            call. = FALSE)
    fit <- c()
    fit$value <- tol + 1
    try2 <- try(fit <- stats::optim(par = c(1, 3), minimize, 
                                    method = "Nelder-Mead"), silent = TRUE)
    if (fit$value >= tol) {
      warning("The fitting procedure 'Nelder-Mead' has failed (convergence error occurred or specified tolerance not achieved)!", 
              call. = FALSE)
      Par <- NA
    }
    else if (fit$value < tol) {
      message("The fitting procedure 'Nelder-Mead' was successful!\n(Used this fallback optimization method because 'L-BFGS-B' has failed...)")
      Par <- c(fit$par,fit$value)
      if (show.output) 
        print(fit)
    }
  }
  else if (fit$value < tol) {
    message("The fitting procedure 'L-BFGS-B' was successful!")
    Par <- c(fit$par,fit$value)
    if (show.output) 
      print(fit)
  }
  return(Par)
}


get.norm.par <- function (p = c(0.025, 0.5, 0.975), q, show.output = FALSE, plot = FALSE, 
                          tol = 0.001, fit.weights = rep(1, length(p)), scaleX = c(0.1, 
                                                                                   0.9), ...) 
{
  if (!is.numeric(p) | !is.numeric(q) | !is.numeric(fit.weights)) {
    stop("INVALID INPUT, not numerical items in the input vectors 'p', 'q' and/or 'fit.weights'!", 
         call. = FALSE)
  }
  if (prod(order(p) == seq(1:length(p))) == 0 | prod(order(q) == 
                                                     seq(1:length(q))) == 0) {
    stop("INVALID INPUT, the vector of probabilities/percentiles is not ordered!", 
         call. = FALSE)
  }
  if (min(p) < 0 | max(p) > 1) {
    stop("INVALID INPUT, items of the probability vector should lie between 0 and 1!", 
         call. = FALSE)
  }
  if (length(p) != length(q) | length(p) != length(fit.weights) | 
      length(q) != length(fit.weights)) {
    stop("INVALID INPUT, 'p', 'q' and 'fit.weights' are not of the same length! The vectors of quantiles, probabilities and weightings should be of the same length.", 
         call. = FALSE)
  }
  if (length(q) < 2) {
    stop("INVALID INPUT, at least two quantiles must be known!", 
         call. = FALSE)
  }
  if (!is.logical(show.output)) {
    stop("INVALID INPUT, the argument 'show.output' should be logical!", 
         call. = FALSE)
  }
  if (!is.logical(plot)) {
    stop("INVALID INPUT, the argument 'plot' should be logical!", 
         call. = FALSE)
  }
  if (!is.numeric(tol) | length(tol) != 1 | tol < 0) {
    stop("INVALID INPUT, the argument 'tol' should be a single positive numerical value!", 
         call. = FALSE)
  }
  fit.weights.original <- fit.weights
  fit.weights <- fit.weights/sum(fit.weights)
  lm.fit <- stats::lm(q ~ p)
  suppressWarnings(m <- stats::predict(lm.fit, newdata = list(p = 0.5))[[1]])
  suppressWarnings(s <- (stats::predict(lm.fit, newdata = list(p = 0.975))[[1]] - 
                           m)/1.96)
  minimize <- function(theta) {
    summand <- suppressWarnings(stats::pnorm(q = q, mean = theta[1], 
                                             sd = theta[2]) - p)
    summand <- summand * fit.weights
    sum(summand^2)
  }
  fit <- c()
  fit$value <- tol + 1
  try1 <- try(fit <- stats::optim(par = c(m, s), minimize, 
                                  method = "L-BFGS-B", lower = c(-10000, 0.001), upper = c(10000, 
                                                                                           10000)), silent = TRUE)
  if (fit$value >= tol) {
    warning("The fitting procedure 'L-BFGS-B' has failed (convergence error occurred or specified tolerance not achieved)!", 
            call. = FALSE)
    fit <- c()
    fit$value <- tol + 1
    try2 <- try(fit <- stats::optim(par = c(m, s), minimize, 
                                    method = "BFGS"), silent = TRUE)
    if (fit$value >= tol) {
      warning("The fitting procedure 'BFGS' has failed (convergence error occurred or specified tolerance not achieved)!", 
              call. = FALSE)
      Par <- NA
    }
    else if (fit$value < tol) {
      message("The fitting procedure 'BFGS' was successful!\n(Used this fallback optimization method because 'L-BFGS-B' has failed...)")
      Par <- c(fit$par,fit$value)
      if (show.output) 
        print(fit)
    }
  }
  else if (fit$value < tol) {
    message("The fitting procedure 'L-BFGS-B' was successful!")
    Par <- c(fit$par,fit$value)
    if (show.output) 
      print(fit)
  }
  return(Par)
}