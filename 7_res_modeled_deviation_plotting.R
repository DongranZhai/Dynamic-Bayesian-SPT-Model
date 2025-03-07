# Aug.2.2024
# 26 OC; trends on individual grid cells
# July.14.2024
# CHL deviation: modeled from observed.
library(raster)
library(oceanmap)
library(ggplot2)
library(scales)
library(stats)
library(ggpmisc)
library(dplyr)

########## Part I. DSTC OC model ##########
rm(list=ls())
setwd("/home/dzhai/reduce_res/dstc_42")
# # of time series on each class: 304*26
dev_tab <- array(NA,dim=c(1,6)) # 7904 = 304*26
colnames(dev_tab) <- c("index","time","obs","fitted","rmse","sd")
# dev_tab$Year <- rep(c(rep(1997,times=4),rep(c(1998:2022),each=12)),times=360*180)
# dev_tab$Month <- rep(c(c(9:12),rep(c(1:12),times=25)),times=360*180)
# dev_tab$Time <- rep(c(1:304),time=360*180)
task_class <-  c(64, 60, 55, 51, 56, 18, 19,
                 14, 37, 47, 15, 48, 13,
                 32, 34, 42, 44, 43, 11,
                 45, 16,  9,  8, 22, 25,
                 27, 28,  3,  5,  6,  4, 21,
                 24, 210, 2329,3358)
for(i in task_class){
  filename <- paste0(i,"_res_dstc_df.Rdata")
  load(filename)
  print(i)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  temp_df <- model_input[,c(1:3,6:11)]
 
  regiontemp <- array(NA,dim=c(304,6))
  colnames(regiontemp) <- c("index","time","obs","fitted",'rmse',"sd")
  for(j in c(1:304)){
    tempoc <- i
    # obs chl
    tempobs <- temp_df$chl[which(temp_df$time == j)] # # of this class
    avrg_obs <- mean(tempobs,na.rm=T)
    
    # fitted chl
    tempfitted <- temp_df$fitted[which(temp_df$time == j)] # # of this class
    avrg_fitted <- mean(tempfitted,na.rm=T)
    
    # rmse
    rmse <- mean(sqrt((tempfitted-tempobs)^2),na.rm=T)/mean(tempobs,na.rm=T)
    sd <- sd(tempfitted,na.rm=T) # deviation of mean
    
    # df
    regiontemp[j,] <- c(tempoc,j,avrg_obs,avrg_fitted,rmse,sd)
    
  }
  dev_tab <- rbind(dev_tab,regiontemp)
  
}
dev_tab <- dev_tab[-1,]
save(dev_tab,file="dstc_42oc_devTab.Rdata")
# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/dstc_42/dstc_42oc_devTab.Rdata ~/Coding/Dynamic_SPT/reduce_res

########## Part II. STC OCFIX model ##########
rm(list=ls())
setwd("/home/dzhai/reduce_res/stc_42")

# # of time series on each class: 304*26
dev_tab <- array(NA,dim=c(1,6)) # 7904 = 304*26
colnames(dev_tab) <- c("index","time","obs","fitted","rmse","sd")
# dev_tab$Year <- rep(c(rep(1997,times=4),rep(c(1998:2022),each=12)),times=360*180)
# dev_tab$Month <- rep(c(c(9:12),rep(c(1:12),times=25)),times=360*180)
# dev_tab$Time <- rep(c(1:304),time=360*180)
task_class <-  c(64, 60, 55, 51, 56, 18, 19,
                 14, 37, 47, 15, 48, 13,
                 32, 34, 42, 44, 43, 11,
                 45, 16,  9,  8, 22, 25,
                 27, 28,  3,  5,  6,  4, 21,
                 24, 210, 2329,3358)
for(i in task_class){
  filename <- paste0(i,"_res_stc_df.Rdata")
  load(filename)
  print(i)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  temp_df <- model_input[,c(1:3,6:11)]
  
  regiontemp <- array(NA,dim=c(304,6))
  colnames(regiontemp) <- c("index","time","obs","fitted",'rmse',"sd")
  for(j in c(1:304)){
    tempoc <- i
    # obs chl
    tempobs <- temp_df$chl[which(temp_df$time == j)] # # of this class
    avrg_obs <- mean(tempobs,na.rm=T)
    
    # fitted chl
    tempfitted <- temp_df$fitted[which(temp_df$time == j)] # # of this class
    avrg_fitted <- mean(tempfitted,na.rm=T)
    
    # rmse
    rmse <- mean(sqrt((tempfitted-tempobs)^2),na.rm=T)/mean(tempobs,na.rm=T)
    sd <- sd(tempfitted,na.rm=T) # deviation of mean
    
    # df
    regiontemp[j,] <- c(tempoc,j,avrg_obs,avrg_fitted,rmse,sd)
    
  }
  dev_tab <- rbind(dev_tab,regiontemp)
  
}
dev_tab <- dev_tab[-1,]
save(dev_tab,file="stc_42ocfix_devTab.Rdata")
# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/stc_42/stc_42ocfix_devTab.Rdata ~/Coding/Dynamic_SPT/reduce_res

########## Part III. STC Longhurst model ##########
rm(list=ls())
setwd("/home/dzhai/reduce_res/long_42")
# # of time series on each class: 304*26
dev_tab <- array(NA,dim=c(1,6)) # 7904 = 304*26
colnames(dev_tab) <- c("index","time","obs","fitted","rmse","sd")
# dev_tab$Year <- rep(c(rep(1997,times=4),rep(c(1998:2022),each=12)),times=360*180)
# dev_tab$Month <- rep(c(c(9:12),rep(c(1:12),times=25)),times=360*180)
# dev_tab$Time <- rep(c(1:304),time=360*180)
task_class <-  c(64, 60, 55, 51, 56, 18, 19,
                 14, 37, 47, 15, 48, 13,
                 32, 34, 42, 44, 43, 11,
                 45, 16,  9,  8, 22, 25,
                 27, 28,  3,  5,  6,  4, 21,
                 24, 210, 2329,3358)
for(i in task_class){
  filename <- paste0(i,"_long_dstc_df.Rdata")
  load(filename)
  print(i)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  temp_df <- model_input[,c(1:3,6:11)]
  
  regiontemp <- array(NA,dim=c(304,6))
  colnames(regiontemp) <- c("index","time","obs","fitted",'rmse',"sd")
  for(j in c(1:304)){
    tempoc <- i
    # obs chl
    tempobs <- temp_df$chl[which(temp_df$time == j)] # # of this class
    avrg_obs <- mean(tempobs,na.rm=T)
    
    # fitted chl
    tempfitted <- temp_df$fitted[which(temp_df$time == j)] # # of this class
    avrg_fitted <- mean(tempfitted,na.rm=T)
    
    # rmse
    rmse <- mean(sqrt((tempfitted-tempobs)^2),na.rm=T)/mean(tempobs,na.rm=T)
    sd <- sd(tempfitted,na.rm=T) # deviation of mean
    
    # df
    regiontemp[j,] <- c(tempoc,j,avrg_obs,avrg_fitted,rmse,sd)
    
  }
  dev_tab <- rbind(dev_tab,regiontemp)
  
}
dev_tab <- dev_tab[-1,]
save(dev_tab,file="stc_42long_devTab.Rdata")
# scp dzhai@storm.pmc.ucsc.edu:/home/dzhai/reduce_res/long_42/stc_42long_devTab.Rdata ~/Coding/Dynamic_SPT/reduce_res


########## Part IV. Make up models and classes ##########
### DSTC
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/dstc_42oc_devTab.Rdata")
dstc_tab <- as.data.frame(dev_tab)
dstc_tab$dev <- dstc_tab$obs - dstc_tab$fitted
table(dstc_tab$index)
table(dstc_tab$time)
### STC OCFIX
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/stc_42ocfix_devTab.Rdata")
stco_tab <- as.data.frame(dev_tab)
stco_tab$dev <- stco_tab$obs - stco_tab$fitted
table(stco_tab$index)
table(stco_tab$time)
### STC Longhutst
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/stc_42long_devTab.Rdata")
stcl_tab <- as.data.frame(dev_tab)
stcl_tab$dev <- stcl_tab$obs - stcl_tab$fitted
table(stcl_tab$index)
table(stcl_tab$time)

dev_all <- rbind(dstc_tab,stco_tab,stcl_tab)
dev_all$group <- rep(c("DSTC","STC-O","STC-L"), each=304*36)
save(dstc_tab,stco_tab,stcl_tab,dev_all,file="res_modeled_chl_deviations.Rdata")

########## Part. Trend data frame combination ############
# coordinate
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3,8:10)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude','OC','OCfix','Long')

load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_spT_tpoc_42resultTab.Rdata")
dstc_tab <- as.data.frame(results_tab)
dstc_tab <-full_join(temp,dstc_tab,by='Site')
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_stc_spT_ocfix42resultTab.Rdata")
stco_tab <- as.data.frame(results_tab)
stco_tab <-full_join(temp,stco_tab,by='Site')
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_stc_spT_42resultTab.Rdata")
stcl_tab <- as.data.frame(results_tab)
stcl_tab <-full_join(temp,stcl_tab,by='Site')

save(dstc_tab,stco_tab,stcl_tab,file=paste("models_trend_df.Rdata"))


########## Part V. Time series plotting on ocfix according to OC ##########
### DSTC time series #####
# setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res")
# rm(list=ls())
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/res_modeled_chl_deviations.Rdata")
# dstc_tab$index <- as.factor(dstc_tab$index)
# # year <- seq(from = as.Date("1997",format="%Y%m%d"),to = as.Date("2022",format="%Y%m%d"),
# #             by = "1 month")
# month <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
#             to = as.Date("2022/12/01",format="%Y/%m/%d"), 
#             by = "1 month")
# dstc_tab$month <- rep(month, times=27)
# 
# ts_desz <- NA
# # Extract trend+remainder
# for(i in c(1:27)){ # optical class
#   temp_df <- dstc_tab %>%
#     filter(index == i)
#   
#   temp_ts <- ts(data = as.numeric(temp_df$obs), frequency = 12, start = c(1997, 9))
#   fit.stl <- stl(temp_ts,t.window=12,s.window="periodic")#,robust=TRUE)
#   temp_stl <- temp_ts-fit.stl$time.series[,'seasonal']
#   
#   ts_desz <- c(ts_desz,temp_stl)
#   
# }
# ts_desz <- ts_desz[-1]
# dstc_tab$deszObs <- ts_desz
# 
# # plot
# ts_df <- dstc_tab %>%
#   group_by(index)
# 
# # stat.test <- dstc_tab %>%
# #   group_by(group) %>%
# #   lm(as.vector(deszValues) ~ as.vector(month)) %>%
# #   adjust_pvalue(method = "bonferroni") %>%
# #   add_significance("p.adj")
# # stat.test <- stat.test %>% 
# #   add_xy_position(x = "group",dodge = 0.8)
# 
# p <- ggplot(data=ts_df,aes(x=month,y=deszObs,color=index,group=index))+
#   geom_line(linewidth=0.8) +
#   # geom_line(data=ts_df,aes(x=Date,y=Fitted,color="#C9C5C4"),position = "identity") +
#   geom_smooth(aes(group=index),method = "lm",formula = y ~ x,color="#C9C5C4",alpha=0.8,se=F)+
#   labs(x=expression(paste("Time (yr)")),
#        y=expression(paste("CHL  ","(mg·m"^"-3",")"))) +
#   theme_bw() +
#   theme(legend.position = "none",
#         axis.title = element_text(face="bold",size=25,colour = "black"),
#         axis.line.x = element_line(color="black", linewidth = 0.5),
#         axis.line.y = element_line(color="black", linewidth = 0.5),
#         axis.text = element_text(face="bold",size=18, color = "black"),
#         strip.text = element_text(face="bold", size=18,lineheight=5.0),
#         strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
#         axis.text.x=element_text(angle=60, hjust=1),
#         plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
#   facet_wrap(~index)
# p <- p+stat_poly_eq(use_label(c("eq","p")),label.x.npc = "left", label.y.npc = 0.95,
#                     formula = y ~ x + 0,
#                     rr.digits = 2, coef.digits = 2,size = 2.5) #"adj.R2"
# p
# ggsave("dstc_gsoc_deszmon.png",dpi = 300, height =10, width = 15, unit = 'in') # landscape

### OBS and fitted in DSTC + STC time series (3 lines overlapped) ######
# task_class <-  c(64, 60, 55, 51, 56, 18, 19,
#                  14, 37, 47, 15, 48, 13,
#                  32, 34, 42, 44, 43, 11,
#                  45, 16,  9,  8, 22, 25,
#                  27, 28,  3,  5,  6,  4, 21,
#                  24, 210, 2329,3358)
setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res")
rm(list=ls())
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/res_modeled_chl_deviations.Rdata")
# year <- seq(from = as.Date("1997",format="%Y%m%d"),to = as.Date("2022",format="%Y%m%d"),
#             by = "1 month")
month <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
             to = as.Date("2022/12/01",format="%Y/%m/%d"), 
             by = "1 month")
stco_tab$dev <- stco_tab$dev-0.005
all_tab <- rbind(dstc_tab,stco_tab,stcl_tab)
# all_tab <- cbind(dstc_tab,stco_tab[,c(4:7)],stcl_tab[,c(4:7)])
# colnames(all_tab) <- c("index","time","obs","fitted.dstc","rmse.dstc","sd.dstc","dev.dstc",
#                       "month","fitted.stco","rmse.stco","sd.stco","dev.stco",
#                       "fitted.stcl","rmse.stcl","sd.stcl","dev.stcl")
all_tab$index <- as.factor(all_tab$index)
all_tab$month <- rep(month, times=3*36)
all_tab$Model <- factor(rep(c('DSTC','STC-O','STC-L'),each=36*304),levels = c('DSTC','STC-O','STC-L'))
# obs_desz <- NA
# dstc_desz <- NA
# stco_desz <- NA
# stcl_desz <- NA
# # Extract trend+remainder
# random_selected <- c(64, 60, 55, 51, 56, 18, 19,
#                  14, 37, 47, 15, 48, 13,
#                  32, 34, 42, 44, 43, 11,
#                  45, 16,  9,  8, 22, 25,
#                  27, 28,  3,  5,  6,  4, 21,
#                  24, 210, 2329,3358)
# for(i in random_selected){ # optical class
#   temp_df <- all_tab %>%
#     filter(index == i)
#   # obs
#   obs_ts <- ts(data = as.numeric(temp_df$obs), frequency = 12, start = c(1997, 9))
#   fit.obs <- stl(obs_ts,t.window=12,s.window="periodic")#,robust=TRUE)
#   obs_stl <- obs_ts-fit.obs$time.series[,'seasonal']
#   obs_desz <- c(obs_desz,obs_stl)
#   # dstc
#   dstc_ts <- ts(data = as.numeric(temp_df$fitted.dstc), frequency = 12, start = c(1997, 9))
#   fit.dstc <- stl(dstc_ts,t.window=12,s.window="periodic")#,robust=TRUE)
#   dstc_stl <- dstc_ts-fit.dstc$time.series[,'seasonal']
#   dstc_desz <- c(dstc_desz,dstc_stl)
#   # stco
#   stco_ts <- ts(data = as.numeric(temp_df$fitted.stco), frequency = 12, start = c(1997, 9))
#   fit.stco <- stl(stco_ts,t.window=12,s.window="periodic")#,robust=TRUE)
#   stco_stl <- stco_ts-fit.stco$time.series[,'seasonal']
#   stco_desz <- c(stco_desz,stco_stl)
#   # stcl
#   stcl_ts <- ts(data = as.numeric(temp_df$fitted.stcl), frequency = 12, start = c(1997, 9))
#   fit.stcl <- stl(stcl_ts,t.window=12,s.window="periodic")#,robust=TRUE)
#   stcl_stl <- stcl_ts-fit.stcl$time.series[,'seasonal']
#   stcl_desz <- c(stcl_desz,stcl_stl)
#   
# }
# all_deszn <- all_tab %>%
#   filter(index %in% random_selected)
# all_deszn$obs <- obs_desz[-1]
# all_deszn$fitted.dstc <- dstc_desz[-1]
# all_deszn$fitted.stco <- stco_desz[-1]
# all_deszn$fitted.stcl <- stcl_desz[-1]

# plot
# ts_df <- dstc_tab %>%
#   group_by(index)
# stat.test <- dstc_tab %>%
#   group_by(group) %>%
#   lm(as.vector(deszValues) ~ as.vector(month)) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance("p.adj")
# stat.test <- stat.test %>% 
#   add_xy_position(x = "group",dodge = 0.8)

# sub_all_tab <- all_tab
sub_all_tab <- all_tab[which(all_tab$index%in% c(9,22,28,210,2329,3358)),] # dstc performs better in these class
sub_all_tab$re.index <- NA
subtitle <- c('Class 9','Class 22','Class 28','Class 21','Class 23','Class 33')
tick <- 1
for(i in c(9,22,28,210,2329,3358)){
  ind <- which(sub_all_tab$index==i)
  sub_all_tab$re.index[ind] <- rep(subtitle[tick],each=length(ind))
  tick <- tick+1
}
labeltemp <- rep(rep(subtitle,each=304),time=3)
sub_all_tab$re.index <- factor(labeltemp, levels=c('Class 9','Class 21','Class 22','Class 23','Class 28','Class 33'))
# sub_all_tab <- all_tab[which(all_tab$index==3358),] 
# sub_all_tab$dev.stco <- sub_all_tab$dev.stco-0.01
## two
p <- ggplot()+
  geom_line(data=sub_all_tab,aes(x=month,y=dev,group=Model,colour = Model),linewidth=1.5) +
  scale_colour_manual(values = c("DSTC"="lightslateblue",
                               "STC-O"="goldenrod1","STC-L"="olivedrab3")) + # breaks = c('DSTC','STC-O','STC-L'),labels = c('DSTC','STC-O','STC-L')
  # geom_line(data=ts_df,aes(x=Date,y=Fitted,color="#C9C5C4"),position = "identity") +
  # geom_smooth(aes(group=index),method = "lm",formula = y ~ x,color="#C9C5C4",alpha=0.8,se=F)+
  labs(x=expression(paste("Time (yr)")),
       y=expression(paste("CHL deviation ","(mg·m"^"-3",")"))) +
  coord_cartesian(ylim = c(-0.05,0.2),expand=F) + #ylim = c(-0.03,0.05),
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~re.index,ncol = 2)
p
ggsave("figure_si.model_deviation.0303.png",dpi = 300, height =20, width = 26, unit = 'in') # landscape

## one
p <- ggplot()+
  # geom_line(data=all_tab,aes(x=month,y=obs,group=index),linewidth=1,color="#C9C5C4") +
  geom_line(data=sub_all_tab,aes(x=month,y=dev.stco,group=index),linewidth=0.8,color="goldenrod1") +
  geom_line(data=sub_all_tab,aes(x=month,y=dev.stcl,group=index),linewidth=0.8,color="olivedrab3") +
  geom_line(data=sub_all_tab,aes(x=month,y=dev.dstc,group=index),linewidth=0.8,color="lightslateblue") +
  # geom_line(data=ts_df,aes(x=Date,y=Fitted,color="#C9C5C4"),position = "identity") +
  # geom_smooth(aes(group=index),method = "lm",formula = y ~ x,color="#C9C5C4",alpha=0.8,se=F)+
  labs(x=expression(paste("Time (yr)")),
       y=expression(paste("CHL  ","(mg·m"^"-3",")"))) +
  coord_cartesian(expand=F) +# ylim = c(-0.1,0.2),
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~index)
p
# p <- p+stat_poly_eq(use_label(c("eq","p")),label.x.npc = "left", label.y.npc = 0.95,
#                     formula = y ~ x + 0,
#                     rr.digits = 2, coef.digits = 2,size = 2.5) #"adj.R2"
# p

ggsave("figure_si.model deviation.0303.png",dpi = 300, height =24, width = 8, unit = 'in') # landscape

######
## RMSE
dev_p <- ggplot()+
  geom_line(data=all_deszn,aes(x=month,y=scale(rmse.dstc,center = T),group=index),linewidth=0.8,color="lightslateblue") +
  geom_line(data=all_deszn,aes(x=month,y=scale(rmse.stco,center = T),group=index),linewidth=0.8,color="goldenrod1") +
  geom_line(data=all_deszn,aes(x=month,y=scale(rmse.stcl,center = T),group=index),linewidth=0.8,color="olivedrab3") +
  # geom_line(data=ts_df,aes(x=Date,y=Fitted,color="#C9C5C4"),position = "identity") +
  # geom_smooth(aes(group=index),method = "lm",formula = y ~ x,color="#C9C5C4",alpha=0.8,se=F)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "red", size=0.8) +
  labs(x=expression(paste("Time (yr)")),
       y=expression(paste("RMSE")),x=NULL) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~index)
dev_p
ggsave("3models_gsoc_dev_rmse.png",dpi = 300, height =10, width = 15, unit = 'in') # landscape

## SD
dev_sd <- ggplot()+
  geom_line(data=all_deszn,aes(x=month,y=scale(sd.stco,center = T),group=index),linewidth=0.8,color="goldenrod1") +
  geom_line(data=all_deszn,aes(x=month,y=scale(sd.stcl,center = T),group=index),linewidth=0.8,color="olivedrab3") +
  geom_line(data=all_deszn,aes(x=month,y=scale(sd.dstc,center = T),group=index),linewidth=0.8,color="lightslateblue") +
  # geom_line(data=ts_df,aes(x=Date,y=Fitted,color="#C9C5C4"),position = "identity") +
  # geom_smooth(aes(group=index),method = "lm",formula = y ~ x,color="#C9C5C4",alpha=0.8,se=F)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "#C9C5C4", size=0.8) +
  labs(x=expression(paste("Time (yr)")),
       y=expression(paste("CHL deviatoin","(mg·m"^"-3",")"))) +
  theme_bw() +
  theme(legend.position = "none",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~index)
dev_sd
ggsave("3models_gsoc_dev_sd.png",dpi = 300, height =10, width = 15, unit = 'in') # landscape

##### Part V. Plotting on GSOC (boxplot) #####
setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res")
rm(list=ls())
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/res_modeled_chl_deviations.Rdata")

month <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
            to = as.Date("2022/12/01",format="%Y/%m/%d"), 
            by = "1 month")
dev_all$date <- rep(month,times=27)
dev_all$index <- as.factor(dev_all$index)
dev_all$group <- as.factor(dev_all$group)
# plot deviation
groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") 
ts_df <- dev_all %>%
  group_by(group)
range(dev_all$sd)
# Facet boxplot: sd
p <- ggplot(data = dev_all,aes(x=group,y=sd,group=group)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
  geom_boxplot(position="dodge",aes(fill=group),outlier.shape = NA) + #,add = "mean_sd",,scale="width"
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0,.35), expand = c(0, 0)) +
  scale_x_discrete(labels = c("DSTC","STC-O","STC-L")) +
  labs(y=expression(paste("Standard Deviation")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~index)
p
ggsave("3models_gsoc_sd_facet_box.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape

range(dev_all$rmse)
# Facet boxplot: rmse
p <- ggplot(data = dev_all,aes(x=group,y=rmse,group=group)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
  geom_boxplot(position="dodge",aes(fill=group),outlier.shape = NA) + #,add = "mean_sd",,scale="width"
  scale_fill_manual(values = groupcolors) +
  scale_y_continuous(limits = c(0,0.5), expand = c(0, 0)) +
  scale_x_discrete(labels = c("DSTC","STC-O","STC-L")) +
  labs(y=expression(paste("RMSE")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
  facet_wrap(~index)
p
ggsave("3models_gsoc_rmse_facet_box.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape


################
# gsoc
rm(list=ls())
# Import gswc data dataframe and transform to matrix
load("/home/dzhai/spt_dynamic/gsoc30_df.Rdata")
wc_result <- wc_result[,-c(3:4)]
# create spatial points data frame
coordinates(wc_result) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(wc_result) <- T
# coerce to raster
temp <- raster(wc_result)
oc_mat <- raster2matrix(temp)
rm(list=c("temp","wc_result"))
# check if import is right
table(oc_mat)

oc <- oc_mat
so_ind <- which(oc==30)
oc[so_ind] <- NA
table(oc)
area <- length(table(oc))
# out <- NA
# for(k in c(1:9)){
#   temp <- paste0("0",k)
#   out <- c(out,temp)
# }
# out <- out[-1]
# dev_tab$ocfix <- c(rep(as.character(c(1,2,4,7,8,13,14,15,17,18,19,20,23,25,27,29)),each=304),
#                    as.character(rep(3,times=304*6)),as.character(rep(5,times=304*4)),as.character(rep(6,times=304*3)),
#                    as.character(rep(9,times=304*3)),as.character(rep(10,times=304*6)),as.character(rep(11,times=304*2)),
#                    as.character(rep(12,times=304*2)),as.character(rep(16,times=304*3)),as.character(rep(21,times=304*3)),
#                    as.character(rep(22,times=304*2)),as.character(rep(24,times=304*3)),as.character(rep(26,times=304*3)),
#                    as.character(rep(28,times=304*4)))

# 1    2    4    7    8   13   14   15   17   18   19   20   23   25   27   29  301  302  303  304 
# 304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304 
# 305  306  501  502  503  504  601  602  603  901  902  903 1001 1002 1003 1004 1005 1006 1101 1102 
# 304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304 
# 1201 1202 1601 1602 1603 2101 2102 2103 2201 2202 2401 2402 2403 2601 2602 2603 2801 2802 2803 2804 
# 304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304  304 
