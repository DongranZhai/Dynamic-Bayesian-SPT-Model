# Aug.21
# Using OC fix partition, take Longhurst as a vector in model formula
# July.11.2024
# Note: Build the Bayesian spatiotemporal model with dynamic optical class.
#       I. Parallel Computation of DSTC

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
setwd("/home/dzhai/reduce_res/long_42")
# notes: i is # of thread, j is time of iteration.
doParallelIteration <- function(i){
  # impart dataframe
  # filename <- paste0("/home/dzhai/reduce_res/long_partition/",i,"_long_res_input.Rdata")
  filename <- paste0("/home/dzhai/reduce_res/task_partition_42/",i,"_dynamic_res_input.Rdata")
  load(filename)
  sptmodel_df.sub = sptmodel_df.sub %>% mutate_if(is.character, as.factor)
  print(i)
  dstc <- chl~time+sp(time)+as.factor(month)+as.vector(long)
  model <- Bsptime(package="sptDyn", model="GP", formula=dstc, data=sptmodel_df.sub,
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
  
  savename <- paste0(i,"_long_dstc_df.Rdata")
  save(fitted,betap,sub_site,betasp,
       model_input,phip,sig2ep,sig2etap,comp,PMCC,
       file=savename)
  # check trace plot (take the first point as example)
  savename <- paste0("/home/dzhai/reduce_res/long_42/trace_plots/",i,"_trace.png")
  png(savename)
  plot(betasp[1,],type='l',main=paste0("trace for class=",i),
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

registerDoParallel(4)
foreach(i=iter(c(5,8,44),.packages="bmstdr")) %dopar% {
  doParallelIteration(i)
}
# 18,37,47,27,5,8,44
#scp -r ~/Coding/Dynamic_SPT/reduce_res/ dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/
#scp -r ~/Coding/Dynamic_SPT/reduce_res/long_partition dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/
#scp -r dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/dstc_long ~/Coding/Dynamic_SPT/reduce_res
########## Part II. Iterations Combination ##########
########## oc coefficient under dstc model ##########
library(boot) # use bootstrap to obtain CI due to non-normal distribution

rm(list=ls())
setwd("/home/dzhai/reduce_res/long_42")
# setwd("~/Coding/Dynamic_SPT/reduce_res/stc_long")
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
  filename <- paste0(i,"_long_dstc_df.Rdata")
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
    # trtptemp <- vector('numeric',length=304)
    # for(k in c(1:304)){
    #   trtptemp[k] <- mean(betatp[k,])
    # }
    # trtp <- mean(trtptemp)
    betasp[tick,] <- betasp[tick,]*100 # percent per month
    trtemp <- mean(betasp[tick,])
    # trtemp <- mean(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    # trtemp <- mean(betasp_d$betasp[which(as.vector(betasp_d$site)==j)])
    ### 95% credibility intervals for trend
    # bounds <- c(trtemp-2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)]),
    #             trtemp+2*sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])) # Z=1.96
    bounds <- c(trtemp-2*sd(betasp[tick,]),trtemp+2*sd(betasp[tick,]))
    ## One: another way for confident intervals
    # ntemp <- length(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])
    # setemp <- sd(betasp_d$betasp[which(as.numeric(betasp_d$site)==tick)])/sqrt(ntemp)
    # t.scoretemp <- qt(p=0.05/2,df=ntemp-1,lower.tail = F)
    # margin.error <- t.scoretemp*setemp
    # l.bound <- trtemp-margin.error
    # u.bound <- trtemp+margin.error
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
    # if((l.bound>0 & u.bound>0) | (l.bound<0 & u.bound<0)){
    #   trtemp <- trtemp
    # }else{
    #   trtemp <- NA
    # }
    ### RMSE & NRMSE & Bias
    RMSE <- sqrt(mean((chltemp-fittedtemp)^2))
    # nrmse.sd <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/sd(subtemp$chl)
    nrmse.maxmin <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/diff(range(subtemp$chl))
    Bias <- bias(fittedtemp,chltemp,type = "bias")
    ### results table
    results_tab[s,] <- c(j,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
    s <- s+1
    tick <- tick+1
    
  }
}
# results_tab <- results_tab[c(1:980),]
save(results_tab,file="long_res_stc_spT_42resultTab.Rdata")
# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/long_42/long_res_stc_spT_42resultTab.Rdata ~/Coding/Dynamic_SPT/reduce_res
# results_tab <- results_tab[c(1:867),]
# save(results_tab,file="long_res_dstc_resultTab.Rdata")

########## Part III. Iterations Combination (not use) ##########
########## time trend under dstc model ##########
library(boot) # use bootstrap to obtain CI due to non-normal distribution

rm(list=ls())
# setwd("/home/dzhai/reduce_res/dstc_long")
setwd("~/Coding/Dynamic_SPT/reduce_res/dstc_long")
# CI Bootstrapt function to obtain the mean
Bmean <- function(data,indices) {
  d <- data[indices] # allows boot to select sample 
  return(mean(d))
} 

results_tab <- array(NA,dim=c(23,11)) 
colnames(results_tab) <- c("Longhurst","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")
obs_fit <- array(NA,dim=c(2592,4))
colnames(obs_fit) <- c('Site','Obs','Fitted','Longhurst')

s=1
for(i in c(1:23)){
  filename <- paste0(i,"_long_dstc_df.Rdata")
  load(filename)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  
  chltemp <- mean(model_input$chl)
  fittedtemp <- mean(model_input$fitted)
  
  betap <- betap[2,c(1000:4000)]*100*12
  trtemp <- mean(betap)
  
  ### 95% credibility intervals for trend
  bounds <- c(trtemp-2*sd(betap),
              trtemp+2*sd(betap))
  
  ## Two: another way for confident intervals
  results <- boot(data=betap, statistic=Bmean, R=1000) #tick
  citemp <- boot.ci(results, type="norm")
  l.bound <- citemp$normal[2]
  u.bound <- citemp$normal[3]
  if((l.bound>0 & u.bound>0) | (l.bound<0 & u.bound<0)){
    trtemp <- trtemp
  }else{
    trtemp <- NA
  }
  ### RMSE & NRMSE & Bias
  RMSE <- sqrt(mean((chltemp -fittedtemp)^2))
  # nrmse.sd <- sqrt(mean(subtemp$fitted-subtemp$chl)^2)/sd(subtemp$chl)
  nrmse.maxmin <- sqrt(mean(model_input$fitted-model_input$chl)^2)/diff(range(model_input$chl))
  Bias <- bias(fittedtemp,chltemp,type = "bias")
  ### results table
  results_tab[i,] <- c(i,trtemp,chltemp,fittedtemp,bounds[1],bounds[2],l.bound,u.bound,RMSE,nrmse.maxmin,Bias)
  ### Parameters of current subset dataframe
  tick=1
  for(j in sub_site){
    print(j)
    subtemp <- model_input[which(model_input$s.index==j),]
    obs_fit[s,] <- c(j,subtemp$chl[tick],subtemp$fitted[tick],i)
    s <- s+1
    tick <- tick+1
  }
}
obs_fit <- obs_fit[c(1:867),]
save(results_tab,obs_fit,file="long_res_dstc_tr_resultTab.Rdata")
# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/dstc_long/long_res_dstc_tr_resultTab.Rdata ~/Coding/Dynamic_SPT/reduce_res
# results_tab <- results_tab[c(1:867),]
# save(results_tab,file="long_res_dstc_resultTab.Rdata")
