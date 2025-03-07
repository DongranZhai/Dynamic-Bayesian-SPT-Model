# Aug.14.2024: change to 28 gsoc
# July.11.2024
# Note: Build the Bayesian spatiotemporal model with dynamic optical class.
#       I. Parallel Computation of STC

library(bmstdr)
library(spTDyn)
library(sp)
library(ggplot2)
library(raster)
library(oceanmap)
library(dplyr)
library(iterators)
library(foreach)
library(doParallel)
library(SimDesign)
library(hydroGOF) #nrmse

rm(list=ls())
setwd("/home/dzhai/reduce_res/stc_42")
# notes: i is # of thread, j is time of iteration.
doParallelIteration <- function(i){
  # impart dataframe
  filename <- paste0("/home/dzhai/reduce_res/task_partition_42/",i,"_dynamic_res_input.Rdata")
  load(filename)
  
  print(i)
  stc <- chl~time+sp(time)+as.factor(month)+as.vector(ocfix)
  model <- Bsptime(package="sptDyn", model="GP", formula=stc, data=sptmodel_df.sub,
                   coordtype="lonlat", coords=2:3, scale.transform = "LOG", n.report=2,
                   rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),prior.beta0 = Norm(0,10^2),
                   N=5000,burn.in = 0,cov.model = "exponential",tol.dist = 0,
                   spatial.decay = spT.decay(distribution = "FIXED",value = 3/500),
                   distance.method="geodetic:km",g_size = 1)
  
  out <- model$fit
  # dim(out$betasp)
  sub_site <- sptmodel_df.sub$s.index # count # of site index
  sub_site <- sub_site[!duplicated(sptmodel_df.sub$s.index)]
  betap <- out$betap
  betasp <- out$betasp
  fitted <- out$fitted
  # fitted <- exp(out$fitted[,1]) # scale transform from log to normal
  model_input <- out$data
  phip <- out$phip
  sig2ep <- out$sig2ep
  sig2etap <- out$sig2etap
  comp <- out$computation.time
  PMCC <- out$PMCC
  
  savename <- paste0(i,"_res_stc_df.Rdata")
  save(fitted,betap,sub_site,betasp,
       model_input,phip,sig2ep,sig2etap,comp,PMCC,
       file=savename)
  
  # check trace plot (take the first point as example)
  savename <- paste0("/home/dzhai/reduce_res/stc_42/trace_plots/",i,"_trace.png")
  png(savename)
  plot(betap[1,],type='l',main=paste0("trace for class=",i),
       xlab="iteration",ylab="raw trend")
  dev.off()
}

##### set multiple cores doing parallel computation
# no_cores <- detectCores()
##### run
registerDoParallel(13)
foreach(i=iter(c(64, 60, 55, 51, 56, 18, 19, 14, 37, 47, 15, 48, 13),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}

registerDoParallel(12)
foreach(i=iter(c(32, 34, 42, 44, 43, 11, 45, 16,  9,  8, 22, 25),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}

registerDoParallel(11)
foreach(i=iter(c(27, 28,  3,  5,  6,  4, 21, 24,210,2329,3358),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}


#scp -r dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/stc_28 ~/Coding/Dynamic_SPT/reduce_res
########## Part II. Iterations Combination ##########
########## stc model ##########
library(boot) # use bootstrap to obtain CI due to non-normal distribution

rm(list=ls())
setwd("/home/dzhai/reduce_res/stc_42")
# setwd("~/Coding/Dynamic_SPT/reduce_res/stc_28")
# CI Bootstrapt function to obtain the mean
Bmean <- function(data, indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

results_tab <- array(NA,dim=c(2592,11)) 
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")
task_class <-  c(64, 60, 55, 51, 56, 18, 19,
                 14, 37, 47, 15, 48, 13,
                 32, 34, 42, 44, 43, 11,
                 45, 16,  9,  8, 22, 25,
                 27, 28,  3,  5,  6,  4, 21,
                 24, 210, 2329,3358)
s=1
for(i in task_class){
  filename <- paste0(i,"_res_stc_df.Rdata")
  load(filename)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  ### Parameters of current subset dataframe
  tick=1
  for(j in sub_site){
    print(s)
    subtemp <- model_input[which(model_input$s.index==j),]
    ### true CHL (average ts)
    chltemp <- mean(subtemp$chl)
    ### fitted CHL
    fittedtemp <- mean(subtemp$fitted)
    ### trends (average betasp)
    # trsp <- mean(betasp[tick,])
    betasp[tick,] <- betasp[tick,]*100 # percent per month
    trtemp <- mean(betasp[tick,])
    bounds <- c(trtemp-2*sd(betasp[tick,]),trtemp+2*sd(betasp[tick,]))
    ## Two: another way for confident intervals
    results <- boot(data=betasp[tick,], statistic=Bmean, R=1000) #tick
    citemp <- boot.ci(results, type="norm")
    l.bound <- citemp$normal[2]
    u.bound <- citemp$normal[3]
    # determine if distribution does not contain 0
    if((bounds[1]>0 & bounds[2]>0) | (bounds[1]<0 & bounds[2]<0)){
      trtemp <- trtemp
    }else{
      trtemp <- NA
    }
    ### RMSE & NRMSE & Bias
    RMSE <- sqrt(mean((chltemp -fittedtemp)^2))
    # nrmse.sd <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/sd(subtemp$chl)
    nrmse.maxmin <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/diff(range(subtemp$chl))
    Bias <- bias(fittedtemp,chltemp,type = "bias")
    ### results table
    results_tab[s,] <- c(j,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
    s <- s+1
    tick <- tick+1
    
  }
}
# results_tab <- results_tab[c(1:875),]
save(results_tab,file="oc_res_stc_spT_ocfix42resultTab.Rdata")

# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/stc_42/oc_res_stc_spT_ocfix42resultTab.Rdata ~/Coding/Dynamic_SPT/reduce_res

