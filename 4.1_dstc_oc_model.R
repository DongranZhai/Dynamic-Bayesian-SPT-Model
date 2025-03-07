# Note: Build the Bayesian spatiotemporal model with dynamic optical class.
#       Parallel Computation of DSTC

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
setwd("/home/dzhai/reduce_res/dstc_42")
# task_class <-  c(64, 60, 55, 51, 56, 18, 19,
#                  14, 37, 47, 15, 48, 13, 33,
#                  32, 34, 38, 42, 44, 43, 11,
#                  45, 16, 35,  9,  8, 22, 25,
#                  27, 28,  3,  5,  6,  4, 21,
#                  2, 24, 23, 49, 46, 10, 29)
# setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/dstc")
# notes: i is # of thread, j is time of iteration.
doParallelIteration <- function(i){
  # impart dataframe
  filename <- paste0("/home/dzhai/reduce_res/task_partition_42/",i,"_dynamic_res_input.Rdata")
  load(filename)

  print(i)
  # dstc <- chl~time+as.factor(month)+oc+sp(oc)
  dstc <- chl~time+sp(time)+as.factor(month)+tp(oc)
  model <- Bsptime(package="sptDyn", model="GP", formula=dstc, data=sptmodel_df.sub,
                     coordtype="lonlat", coords=2:3, scale.transform = "LOG", n.report=2,
                     rhotp = Norm(0,10^2),prior.sigma.eta=Gamm(a=2,b=1),prior.beta0 = Norm(0,10^2),
                     N=5001,burn.in = 1,cov.model = "exponential",tol.dist = 0,
                     spatial.decay = spT.decay(distribution = "FIXED",value = 3/500),
                     distance.method="geodetic:km",g_size = 1)
  
  out <- model$fit
  # dim(out$betasp)
  sub_site <- sptmodel_df.sub$s.index # count # of site index
  sub_site <- sub_site[!duplicated(sptmodel_df.sub$s.index)]
  # betasp <- c(t(out$betasp*12)) # per year
  # betasp_d <- data.frame(site=as.factor(sub_site), betasp = betasp)
  betatp <- out$betatp
  betat0p <- out$betat0p
  betasp <- out$betasp
  betap <- out$betap
  fitted <- out$fitted
  # fitted <- exp(out$fitted[,1]) # scale transform from log to normal
  model_input <- out$data
  phip <- out$phip
  sig2ep <- out$sig2ep
  sig2etap <- out$sig2etap
  sig2betap <- out$sig2betap
  sig2deltap <- out$sig2deltap
  comp <- out$computation.time
  PMCC <- out$PMCC
  
  savename <- paste0(i,"_res_dstc_df.Rdata")
  save(fitted,betasp,betap,betatp,betat0p,sub_site,
       model_input,phip,sig2ep,sig2etap,sig2betap,sig2deltap,
       comp,PMCC,file=savename)
  
  # check trace plot (take the first point as example)
  savename <- paste0("/home/dzhai/reduce_res/dstc_42/trace_plots/",i,"_trace.png")
  png(savename)
  plot(betasp[1,],type='l',main=paste0("trace for class=",i),
         xlab="iteration",ylab="raw trend")
  dev.off()
}

##### set multiple cores doing parallel computation
# no_cores <- detectCores()
##### run
registerDoParallel(15)
foreach(i=iter(task_class,.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}

########## Part II. Iterations Combination ##########
library(boot) # use bootstrap to obtain CI due to non-normal distribution

rm(list=ls())
setwd("/home/dzhai/reduce_res/dstc_42")
# setwd("~/Coding/Dynamic_SPT/reduce_res/dstc")
# CI Bootstrapt function to obtain the mean
Bmean <- function(data,indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

results_tab <- array(NA,dim=c(2592,11)) # 72*36
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")
task_class <-  c(64, 60, 55, 51, 56, 18, 19,
                 14, 37, 47, 15, 48, 13,
                 32, 34, 42, 44, 43, 11,
                 45, 16,  9,  8, 22, 25,
                 27, 28,  3,  5,  6,  4, 21,
                 24, 210, 2329,3358)
s=1
for(i in task_class){
  filename <- paste0(i,"_res_dstc_df.Rdata")
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

    betasp[tick,] <- betasp[tick,]*100 # percent per month
    trtemp <- mean(betasp[tick,])

    bounds <- c(trtemp-2*sd(betasp[tick,]),trtemp+2*sd(betasp[tick,]))

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
    RMSE <- sqrt(mean((chltemp-fittedtemp)^2))
    nrmse.maxmin <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/diff(range(subtemp$chl))
    Bias <- bias(fittedtemp,chltemp,type = "bias")
    ### results table
    results_tab[s,] <- c(j,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
    s <- s+1
    tick <- tick+1
    
  }
}

save(results_tab,file="oc_res_dstc_spT_tpoc_42resultTab.Rdata")

####
# dstc
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_sptp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
results_tab$Trend_Value <- results_tab$Trend_Value*12
save(results_tab,file="oc_res_dstc_sptp_resultTab.Rdata")
# long
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_dstc_sptp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
results_tab$Trend_Value <- results_tab$Trend_Value*12
save(results_tab,file="long_res_dstc_sptp_resultTab.Rdata")
# stc
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_stc_tp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
results_tab$Trend_Value <- results_tab$Trend_Value # already contain *100
save(results_tab,file="oc_res_stc_tp_resultTab.Rdata")

