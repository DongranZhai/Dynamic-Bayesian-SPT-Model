# Aug.2.2024
# 26 OC; trends on individual grid cells
### July.14.2024
#         I. Group the trend estimates from DSTC and STC (the results from dynamic SPT model)
#         according to fixed optical classes (24 groups).
#         II. Group the trend estimates from DSTC and Longhurst (the results from dynamic SPT model)
#         according to fixed optical classes (24 groups).

############ Part I. DSTC + STC #######################
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("~/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_nna_df.Rdata")

site_ind <- global_df[,c(1:3,9)]
site_ind <- site_ind[!duplicated(site_ind), ]
colnames(site_ind) <- c("Site","Longitude","Latitude","GSOC")

##### I. Merge trends and optical class groups
# DSTC model
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_spT_tpoc_27resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
dstc_tr_df <- merge(site_ind,results_tab,by="Site")

# STC-O model
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/ocfix_res_stc_spT_27resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
stco_tr_df <- merge(site_ind,results_tab,by="Site")

# STC model
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_stc_spT_27resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
stcl_tr_df <- merge(site_ind,results_tab,by="Site")

save(dstc_tr_df,stco_tr_df,stcl_tr_df,file=paste0("3models_res_trends_df.Rdata"))


##### II. Group trend estimates according to ocean basin
# rm(list=ls())
# # load("~/Coding/Dynamic_SPT/dynamic/global_chl_oc_df.Rdata")
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/res_esTrends_2models_df.Rdata")
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/gsoc24_df_res.Rdata")
# 
# basin_ind <- oc_result[,c(1,2,4)]
# temp <- trunc(basin_ind[,c(1,2)])
# basin_ind <- cbind(temp,basin_ind[,3])
# colnames(basin_ind) <- c("Longitude","Latitude","Basin")
# # dstc
# temp <- trunc(dstc_tr_df[,c(2,3)])
# dstc_tr_df[,c(2,3)] <- temp
# 
# dstc_tr_df$Basin <- NA
# for(i in 1:nrow(dstc_tr_df)){
#   tempind <- basin_ind$Basin[which(basin_ind$Longitude == dstc_tr_df$Longitude[i] & basin_ind$Latitude == dstc_tr_df$Latitude[i])]
#   dstc_tr_df$Basin[i] <- tempind
# }
# 
# # stc
# temp <- trunc(stc_tr_df[,c(2,3)])
# stc_tr_df[,c(2,3)] <- temp
# 
# stc_tr_df$Basin <- NA
# for(i in 1:nrow(stc_tr_df)){
#   tempind <- basin_ind$Basin[which(basin_ind$Longitude == stc_tr_df$Longitude[i] & basin_ind$Latitude == stc_tr_df$Latitude[i])]
#   stc_tr_df$Basin[i] <- tempind
# }
# save(stc_tr_df,dstc_tr_df,file=paste0("res_esTrends_2models_df.Rdata"))
# this version with ocean basin label in data frame


##### III. Table of results #####
rm(list=ls())
setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/3models_res_trends_df.Rdata")
tr_3model_summary <- array(NA,dim=c(81,7))
for (i in c(1:27)){
  temp_dstc <- dstc_tr_df[which(dstc_tr_df$GSOC==i),]
  temp_stco <- stco_tr_df[which(stco_tr_df$GSOC==i),]
  temp_stcl <- stcl_tr_df[which(stcl_tr_df$GSOC==i),]
  tick <- 2
  for(j in c(5,8,9,13,14)){
    avrg_dstc <- round(mean(temp_dstc[,j],na.rm=T),digits = 5)
    avrg_stco <- round(mean(temp_stco[,j],na.rm=T),digits = 5)
    avrg_stcl <- round(mean(temp_stcl[,j],na.rm=T),digits = 5)
    
    tr_3model_summary[i,tick] <- avrg_dstc
    tr_3model_summary[i+27,tick] <- avrg_stco
    tr_3model_summary[i+27,tick] <- avrg_stcl
    tick <- tick+1
  }
  tr_3model_summary[i,1] <- i
  tr_3model_summary[i+27,1] <- i
  tr_3model_summary[i+54,1] <- i
}
tr_3model_summary[,7] <- rep(c("DSTC","STC-O","STC-L"),each=27) 
tr_3model_summary <- as.data.frame(tr_3model_summary)
colnames(tr_3model_summary) <- c("Class","Trends","Lower CI","Upper CI","NRMSE","Bias","Model")
save(tr_3model_summary,stco_tr_df,stcl_tr_df,dstc_tr_df,file=paste0("3models_res_trends_df.Rdata"))

############ Part II. DSTC + Longhurst (not use) #######################
rm(list=ls())
load("~/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_df.Rdata")
# use global_no_na
site_ind <- global_df[,c(1:3,9,10)]
site_ind <- site_ind[!duplicated(site_ind), ]
colnames(site_ind) <- c("Site","Longitude","Latitude","OCfix","Longhurst")

##### I. Merge trends and optical class groups #####
# DSTC model
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_sptp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
dstc_tr_df <- merge(site_ind,results_tab,by="Site")

# Longhurst model
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_dstc_sptp_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
long_tr_df <- merge(site_ind,results_tab,by="Site")

save(dstc_tr_df,long_tr_df,file=paste0("res_esTrends_oclong_df.Rdata"))

##### III. Table of results #####
rm(list=ls())
setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/res_esTrends_oclong_df.Rdata")
tr_oclong_summary <- array(NA,dim=c(48,7))
for (i in c(1:24)){
  temp_dstc <- dstc_tr_df[which(dstc_tr_df$OCfix==i),]
  temp_long <- long_tr_df[which(long_tr_df$Longhurst==i),]
  tick <- 2
  for(j in c(6,9,10,14,15)){
    avrg_dstc <- round(mean(temp_dstc[,j],na.rm=T),digits = 5)
    avrg_long <- round(mean(temp_long[,j],na.rm=T),digits = 5)
    
    tr_oclong_summary[i,tick] <- avrg_dstc
    tr_oclong_summary[i+24,tick] <- avrg_long
    tick <- tick+1
  }
  tr_oclong_summary[i,1] <- i
  tr_oclong_summary[i+24,1] <- i
}
tr_oclong_summary[,7] <- rep(c("DSTC","Longhurst"),each=24) 
tr_oclong_summary <- as.data.frame(tr_oclong_summary)
tr_oclong_summary <- tr_oclong_summary[-1,]
colnames(tr_oclong_summary) <- c("Class","Trends","Lower CI","Upper CI","NRMSE","Bias","Model")
save(tr_oclong_summary,long_tr_df,dstc_tr_df,file=paste0("res_esTrends_oclong_df.Rdata"))

##################################################
# ##### Merge gsoc
# load("~/Coding/Dynamic_SPT/dynamic/global_chl_oc_df.Rdata")
# # use global_no_na
# site_ind <- global_no_na[,c(1:3,9,10)]
# site_ind <- site_ind[!duplicated(site_ind), ]
# colnames(site_ind) <- c("Site","Longitude","Latitude","OCfix","Longhurst")
# 
# # merge(basin_ind,dstc_tr_df,by="Site")
# tempind <- dstc_tr_df[i,c(2,3)]
# which(basin_ind$Longitude == tempind$Longitude )
# # Not solved yet
# basin_tr_df <- merge(basin_ind,dstc_tr_df,by = c("Longitude","Latitude"))
# basin_tr_df <- left_join(dstc_tr_df,basin_ind,by = c("Longitude","Latitude"))
# all_tr_df <- rbind(dstc_tr_df,stc_tr_df)
# tempdstc <- basin_ind %>%
#   filter(basin_ind[,c(1,2)] %in% dstc_tr_df[,c(2,3)])
