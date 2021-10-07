#########################################################
### Authors: Simone Padoan                            ###
### Emails: simone.padoan@unibocconi.it               ###
### Institution: Department of Decision Sciences,     ###
### University Bocconi of Milan, Italy,               ###
### File name: script_impacts_future_climate.r        ###
### Description:                                      ###
### This file contains the commands to analyse        ###
### the tail probability of impacts for several       ###
### regions                                           ###
### by Simone A. Padoan                               ###
### Last change: 09/12/2020                           ###
#########################################################

#########################################################
### START
### REAL DATA ANALYSIS OF TAIL PROBABILITY
###
###
#########################################################

impact_exceed_prob <- function(impact_samp_stat, threshold = 0.5) {
  
# DEFINE SELECTION VARIABLES

# define time span
time_id <- 2020:2100
time_size <- length(time_id)

# DEFINE VARIABLES THAT COLLECT RESULTS
# define the tail probabilities for the different impacts, regions and years
tail_probability <- array(0, c(6, time_size))

# define the thresholds for the different impacts and regions
impact_threshold <- array(0)

# define a test to check whether there is equal exceeding probability, i.e.
# H_0 (or null hypothesis) against the alternative hypothesis H_1, i.e. that
# the full distribution has longer tail than the peak distribution.
# N.B. We look for empirical evidence to reject H_0
impact_test <- array(0, c(time_size))

# define the number of rejections for the different impacts and regions
impact_rejection <- array(0)

# define the significance level of the test
alpha <- 0.05

# define a high threshold as the median of the peak distribution
# over the century
impact_threshold <- quantile(impact_samp_stat[["peak"]], prob = threshold)

i <- impact_samp_stat[["iindic"]]

# LOOP YEARS
for(k in 1:time_size){
  
  time_ssize <- ncol(impact_samp_stat[["full"]])
  
  if(i %in% iindic_min) {
    time_num_exceed <- c(sum(impact_samp_stat[["full"]][k,]<impact_threshold),
                         sum(impact_samp_stat[["peak"]][k,]<impact_threshold))
  } else {
    time_num_exceed <- c(sum(impact_samp_stat[["full"]][k,]>impact_threshold),
                         sum(impact_samp_stat[["peak"]][k,]>impact_threshold))
  }
  
  # estimation of the exceedance probability for the full distribution
  est <- binom.confint(x = time_num_exceed[1], n = time_ssize, 
                       conf.level = 0.95, tol = 1e-8, methods = "exact" )
  tail_probability[1,k] <- est[1,4] # mean
  tail_probability[2,k] <- est[1,5] # lower
  tail_probability[3,k] <- est[1,6] # upper
  # estimation of the exceedance probability for the peak distribution
  est <- binom.confint(x = time_num_exceed[2], n = time_ssize, 
                       conf.level = 0.95, tol = 1e-8, methods = "exact")
  tail_probability[4,k] <- est[1,4] # mean
  tail_probability[5,k] <- est[1,5] # lower
  tail_probability[6,k] <- est[1,6] # upper
  
  # perform the test for checking whether we reject the null hypothesis
  if(i %in% iindic_min) {
    test <- binom.test(time_num_exceed[1], time_ssize, tail_probability[4,k],
                       alternative = "less")
  } else {
    test <- binom.test(time_num_exceed[1], time_ssize, tail_probability[4,k],
                       alternative = "greater")
  }
  
  # save test outcome
  impact_test[k] <- test$p.value
  
}

# save the number of rejections for given impact and the different regions
impact_rejection <- sum(impact_test < 0.05)

# Build results
res <- list(iindic = impact_samp_stat[["iindic"]],
            ireg = impact_samp_stat[["ireg"]],
            cluster = impact_samp_stat[["cluster"]],
            impact_rejection = impact_rejection,
            impact_threshold = impact_threshold,
            tail_probability = tail_probability
            )

}

extract_impact_rejection <- function(x) {
  data.table(iindic = x[["iindic"]],
             ireg = x[["ireg"]],
             cluster = x[["cluster"]],
             impact_rejection = x[["impact_rejection"]])
}


plot_exceed_prod_fig2 <- function(outfile) {

  impact_exc_pb <- readd(impact_exc_pb_95_impact_exc_10_1L_1L)
  time_id <- 2020:2100
  time_size <- length(time_id)
  pdf(outfile, width=5.25, height=4.25)
  par(mai=c(.5,.5,.2,.1), mgp=c(1.6,.6,0))
  impact_tailprob <- impact_exc_pb[["tail_probability"]]
      plot(time_id, impact_tailprob[1,], pch=19, col="red", lwd=1,cex = .5,
           ylim=c(0, max(impact_tailprob)), las=1, xlab="Time", ylab="",
           main=eval(bquote(expression(.(iindic[10])*", likely 1.5"*~degree*C))))
      points(time_id, impact_tailprob[4,], pch=19, col="blue", lwd=1, cex = .5)
      text(2060, 1, labels=eval(bquote(expression("Exceeding Probability of"~.(impact_exc_pb[["impact_threshold"]])~.(iunit[10])))))
      # THIRD LOOP FOR THE DIFFERENT YEARS
      for(k in 1:time_size){
        segments(time_id[k], impact_tailprob[2,k], time_id[k],
                 impact_tailprob[3,k], col="red")
        segments(time_id[k], impact_tailprob[5,k], time_id[k],
                 impact_tailprob[6,k], col="blue")
      }
      legend("bottomright", col=c("red", "red", "blue", "blue"),
             pch=c(19,124,19,124),
             legend=c(expression(hat(p)[n]~"-EOC"),
                      expression(paste(bold("95%-CI-"), italic("EOC"))),
                      expression(hat(p)[n]~"-NZ"),
                      expression(paste(bold("95%-CI-"), italic("NZ")))),
             bty="n", cex=1.3)
  dev.off()
    
}


plot_exceed_prod_15C <- function(outfile) {

time_id <- 2020:2100
time_size <- length(time_id)
# START GRAPHICAL REPRESENTATIONS
pdf(outfile,width=5, height=4.5)
par(mai=c(.5,.5,.2,.1), mgp=c(1.6,.6,0))
# FIRST LOOP FOR THE DIFFERENT IMPACTS
for(i in iindic_exc){
  # SECOND LOOP FOR THE DIFFERENT REGIONS
  for(j in 1:6){
    impact_exc_pb <- readd(str_glue("impact_exc_pb_95_impact_exc_{i}_{j}L_1L"), character_only = T)
    impact_tailprob <- impact_exc_pb[["tail_probability"]]
    plot(time_id, impact_tailprob[1,], pch=19, col="red", lwd=2,
         ylim=c(min(impact_tailprob),max(impact_tailprob)), las=1, xlab="Time", ylab="",
         main=eval(bquote(expression(.(iindic[i])*","~.(ireg[j])))))
    points(time_id, impact_tailprob[4,], pch=19, col="blue", lwd=2)
    text(2060, 1, labels=eval(bquote(expression("Exceeding Probability of "~.(round(impact_exc_pb[["impact_threshold"]],2))~.(iunit[i])))))
    # THIRD LOOP FOR THE DIFFERENT YEARS
    for(k in 1:time_size){
      segments(time_id[k], impact_tailprob[2,k], time_id[k],
               impact_tailprob[3,k], col="red")
      segments(time_id[k], impact_tailprob[5,k], time_id[k],
               impact_tailprob[6,k], col="blue")
    }
    legend("bottomright", col=c("red", "red", "blue", "blue"),
           pch=c(19,124,19,124),
           legend=c(expression(hat(p)[n]~"-EOC"),
                    expression(paste(bold("95%-CI-"), italic("EOC"))),
                    expression(hat(p)[n]~"-NZ"),
                    expression(paste(bold("95%-CI-"), italic("NZ")))),
           bty="n", cex=1.3)
  }
}
dev.off()

}

