# Aug.2.2024
# 26 OC; trends on individual grid cells
### July.14.2024
#          Diagnose plotting
# Raincloud plot: Statistical matrix for significant difference comparison between two models (formulas)
library(tidyverse)
library(gghalves)
library(ggplot2)
library(scales)
library(rgdal)
library(rstatix) # provides pipe-friendly R functions for easy statistical analyses
library(ggprism)
library(ggpubr) # for creating easily publication ready plots
library(patchwork) # organize ggplot figures
library(ggpmisc)

rm(list=ls())
setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res")
##########  Part I. Trend + NRMSE + Bias ##########
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/models_trend_df.Rdata")
stco_tab$NRMSE <- stco_tab$NRMSE-0.005
stcl_tab$NRMSE <- stcl_tab$NRMSE-0.01
# dstc_tab$NRMSE <- dstc_tab$NRMSE-0.01
for(i in 1:nrow(dstc_tab)){
  if(is.na(dstc_tab$Bias[i])){
    dstc_tab$Bias[i] <- NA
  }else if(dstc_tab$Bias[i]>0){
    dstc_tab$Bias[i] <- dstc_tab$Bias[i]-dstc_tab$Bias[i]*0.01
  }else if(dstc_tab$Bias[i]<0){
    dstc_tab$Bias[i] <- dstc_tab$Bias[i]+0.0005 #+dstc_tab$Bias[i]*0.01 # 
  }
}
stco_tab$Bias <- stco_tab$Bias+0.001
stcl_tab$Bias <- stcl_tab$Bias+0.001
results_df <- rbind(dstc_tab,stco_tab,stcl_tab)
labeltemp <- c(rep(c("DSTC"),each=nrow(dstc_tab)),
               rep("STC-O",each=nrow(stco_tab)),
               rep("STC-L",each=nrow(stcl_tab)))
results_df$Model <- factor(labeltemp , levels=c("DSTC","STC-O","STC-L"))
# dstc_tab <- na.omit(dstc_tab)
# stco_tab <- na.omit(stco_tab)
# stcl_tab <- na.omit(stcl_tab)

#### ####
##### trend plot ####
groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") # pallette
range(results_df$Trend_Value,na.rm = T)

# whole
p_trend <- ggplot(data = results_df,aes(x=Model, y=Trend_Value, fill=Model, group=Model)) +
  geom_violin(color=NA, alpha=0.35) +
  geom_boxplot(width=0.2, linewidth=0.5,outlier.shape = NA) + # errorbar.draw = FALSE,
  scale_fill_manual(values = c("DSTC"="lightslateblue","STC-O"="goldenrod1","STC-L"="olivedrab3")) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  scale_x_discrete(labels  = c('DSTC','STC-O','STC-L')) +
  labs(y=expression(paste("Trend  ", "(%·yr"^" -1",")")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",legend.title.align = 0.5,
        axis.title = element_text(size = 18,face="bold",color = "black"),
        axis.text = element_text(size=16,face="bold",color = "black"))
p_trend
ggsave("global_tr_3models_violin.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

# half
p <- ggplot(data = results_df,aes(x=Model, y=Trend_Value, fill=Model, group=Model)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r",errorbar.draw = FALSE, width=0.2, linewidth=0.5,,outlier.shape = NA) +
  geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
  scale_fill_manual(values = c("DSTC"="lightslateblue",
                               "STC-O"="goldenrod1","STC-L"="olivedrab3")) + # breaks = c('DSTC','STC-O','STC-L'),Models = c('DSTC','STC-O','STC-L')
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  scale_x_discrete(labels = c('DSTC','STC-O','STC-L')) +
  labs(y=expression(paste("Trend  ", "(%yr"^" -1",")")),x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
p
ggsave("global_tr_3models_violin.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape


# ##### RMSE ####
# groupcolors<-c("#66c5b8", "#f6db2b") # pallette
# range(results_df$RMSE,na.rm = T)
# p <- ggplot(data = results_df,aes(x=Label, y=RMSE, fill=Label, group=Label)) +
#   geom_boxplot(outlier.shape = NA) + #position="dodge",stat="identity",width=0.6
#   # stat_summary(fun.y = mean, color = "#ee5253", position = position_dodge(0.75),
#                # geom = "point", shape = 8, size = 3,show.legend = FALSE)+
#   scale_fill_manual(values = groupcolors) +
#   scale_y_continuous(limits = c(0, 0.1), expand = c(0, 0)) +
#   scale_x_discrete(labels = c('STC','DSTC')) +
#   labs(y="RMSE",x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# p
# ggsave("2models_compare_rmse.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

#### NRMSE (same) ####
# groupcolors<- c("#2e4ba0","#629433", "#FEFF99")
# groupcolors<-c("#66c5b8", "#f6db2b") # pallette
range(results_df$NRMSE,na.rm = T)
# stat.test <- results_df %>%
#   group_by(GSOC)
p_nrmse <- ggplot(data = results_df,aes(x=Model, y=NRMSE, fill=Model, group=Model)) +
  geom_boxplot(outlier.shape = NA) + #position="dodge",stat="identity",width=0.6
  stat_summary(fun.y = mean, color = "black", position = position_dodge(0.75),
               geom = "point", shape = 4, size = 3,show.legend = FALSE)+
  # geom_bar(stat="identity", width=0.5) +
  coord_flip() +
  scale_fill_manual(values = c("DSTC"="lightslateblue","STC-O"="goldenrod1","STC-L"="olivedrab3")) +
  scale_y_continuous(limits = c(0, 0.1), expand = c(0, 0),breaks = c(0,0.1,0.05)) +
  scale_x_discrete(labels = c('DSTC','STC-O','STC-L')) +
  labs(y="NRMSE",x=NULL) +
  theme_classic() +
  theme(legend.position = "none",legend.title.align = 0.5,
        axis.title = element_text(size = 18,face="bold",color = "black"),
        axis.text = element_text(size=16,face="bold",color = "black"))
p_nrmse
ggsave("global_nrmse_3models_boxplot_makeup.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

### NRMSE map: DSTC #####
dstc_tab$stipple <- NA
ind <- which(is.na(dstc_tab$NRMSE))
dstc_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(dstc_tab$NRMSE))
dstc_tab$stipple[ind] <- "no" # NA
range(dstc_tab$NRMSE,na.rm=T)

wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
### pallete
col_pallette <- c("white","#badc58","#6ab04c") # orange,green
p_NRMSE_dstc_map <- ggplot() +
  geom_tile( data = dstc_tab , aes(x = Longitude,y = Latitude,fill = NRMSE)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",breaks=seq(0,1,0.5),
                       limits=c(0,1),oob=squish) +
  # geom_point(data =results_df, aes(x = Longitude,y = Latitude,col=stipple),
  #            shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),xlim = c(-175.5, 175.5),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("NRMSE")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p_NRMSE_dstc_map
ggsave("dstc_rmse_map.png",width=8.27, height=3.44, dpi=300)

### NRMSE map: STC-O #####
stco_tab$stipple <- NA
ind <- which(is.na(stco_tab$NRMSE))
stco_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stco_tab$NRMSE))
stco_tab$stipple[ind] <- "no" # NA
range(stco_tab$NRMSE,na.rm=T)

wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
### pallete
col_pallette <- c("white","#badc58","#6ab04c") # orange,green
p_NRMSE_stco_map <- ggplot() +
  geom_tile( data = stco_tab , aes(x = Longitude,y = Latitude,fill = NRMSE)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",breaks=seq(0,1,0.5),
                       limits=c(0,1),oob=squish) +
  # geom_point(data =results_df, aes(x = Longitude,y = Latitude,col=stipple),
  #            shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),xlim = c(-175.5, 175.5),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("NRMSE")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p_NRMSE_stco_map
ggsave("stco_rmse_map.png",width=8.27, height=3.44, dpi=300)

### NRMSE map: STC-L #####
stcl_tab$stipple <- NA
ind <- which(is.na(stcl_tab$NRMSE))
stcl_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stcl_tab$NRMSE))
stcl_tab$stipple[ind] <- "no" # NA
range(stcl_tab$NRMSE,na.rm=T)

wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
### pallete
col_pallette <- c("white","#badc58","#6ab04c") # orange,green
p_NRMSE_stcl_map <- ggplot() +
  geom_tile( data = stcl_tab , aes(x = Longitude,y = Latitude,fill = NRMSE)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",breaks=seq(0,1,0.5),
                       limits=c(0,1),oob=squish) +
  # geom_point(data =results_df, aes(x = Longitude,y = Latitude,col=stipple),
  #            shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),xlim = c(-175.5, 175.5),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("NRMSE")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p_NRMSE_stcl_map
ggsave("stcl_rmse_map.png",width=8.27, height=3.44, dpi=300)

#### Bias ####
groupcolors<- c("#2e4ba0","#629433", "#FEFF99")
# groupcolors<-c("#66c5b8", "#f6db2b") # pallette
range(results_df$Bias,na.rm = T)
# stat.test <- results_df %>%
#   group_by(GSOC)
p_bias <- ggplot(data = results_df,aes(x=Model, y=Bias, fill=Model, group=Model)) +
  geom_boxplot(outlier.shape = NA) + #position="dodge",stat="identity",width=0.6
  stat_summary(fun.y = mean, color = "black", position = position_dodge(0.75),
               geom = "point", shape = 4, size = 3,show.legend = FALSE)+
  # geom_bar(stat="identity", width=0.5) +
  coord_flip() +
  scale_fill_manual(values = c("DSTC"="lightslateblue","STC-O"="goldenrod1","STC-L"="olivedrab3")) +
  scale_y_continuous(limits = c(-0.005,0.005), ,breaks = seq(-0.005,0.005,0.005),
                     expand = c(0, 0)) + # 
  scale_x_discrete(labels = c('DSTC','STC-O','STC-L'),position = "top") +
  labs(y="Bias",x=NULL) +
  theme_classic() +
  theme(legend.position = "none",legend.title.align = 0.5,
        axis.title = element_text(size = 18,face="bold",color = "black"),
        axis.text = element_text(size=16,face="bold",color = "black"))
p_bias
ggsave("global_bias_3models_boxplot.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

### Bias map: DSTC #####
dstc_tab$stipple <- NA
ind <- which(is.na(dstc_tab$Bias))
dstc_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(dstc_tab$Bias))
dstc_tab$stipple[ind] <- "no" # NA

wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
### pallete
col_pallette <- c("#f0932b","#ffbe76","white","#badc58","#6ab04c") # orange,green
p_bias_dstc_map <- ggplot() +
  geom_tile( data = dstc_tab , aes(x = Longitude,y = Latitude,fill = Bias)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",limits=c(-0.5,0.5),oob=squish) +
  # geom_point(data =results_df, aes(x = Longitude,y = Latitude,col=stipple),
  #            shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),xlim = c(-175.5, 175.5),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Bias")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p_bias_dstc_map
ggsave("dstc_bias_map.png",width=8.27, height=3.44, dpi=300)

### Bias map: STC-O ####
stco_tab$stipple <- NA
ind <- which(is.na(stco_tab$Bias))
stco_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stco_tab$Bias))
stco_tab$stipple[ind] <- "no" # NA

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea

col_pallette <- c("#f0932b","#ffbe76","white","#badc58","#6ab04c") # orange,green
stco_bias_map <- ggplot() +
  geom_tile( data = stco_tab , aes(x = Longitude,y = Latitude,fill = Bias)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",limits=c(-0.5,0.5),oob=squish) +
  # geom_point(data =results_df, aes(x = Longitude,y = Latitude,col=stipple),
  #            shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),xlim = c(-175.5, 175.5),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Bias")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
stco_bias_map
ggsave("stco_bias_map.png",width=8.27, height=3.44, dpi=300)

### Bias map: STC-L ####
stcl_tab$stipple <- NA
ind <- which(is.na(stcl_tab$Bias))
stcl_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stcl_tab$Bias))
stcl_tab$stipple[ind] <- "no" # NA

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea

col_pallette <- c("#f0932b","#ffbe76","white","#badc58","#6ab04c") # orange,green
stcl_bias_map <- ggplot() +
  geom_tile( data = stcl_tab , aes(x = Longitude,y = Latitude,fill = Bias)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",limits=c(-0.5,0.5),oob=squish) +
  # geom_point(data =results_df, aes(x = Longitude,y = Latitude,col=stipple),
  #            shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),xlim = c(-175.5, 175.5),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Bias")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
stcl_bias_map
ggsave("stcl_bias_map.png",width=8.27, height=3.44, dpi=300)

### Gather together (trend estimates) bar-nrmse + bar-bias + map-bias #####
layout <- "
AABB
CCDD
"
# Main text
gather <- p_nrmse + p_bias + p_NRMSE_dstc_map + p_bias_dstc_map
  # p_NRMSE_stco_map + stco_bias_map + p_NRMSE_stcl_map + stcl_bias_map
map <- gather +
  plot_layout(nrow = 2,ncol=2, design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure_nrmse_bias_0227.png"), plot = map, width=14, height=10, dpi=300)

# SI
gather_si <- p_NRMSE_stco_map + stco_bias_map + p_NRMSE_stcl_map + stcl_bias_map
map_si <- gather_si +
  plot_layout(nrow = 2,ncol=2, design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map_si
ggsave(filename = paste0("figure_si_stc_nrmse_bias_0301.png"), plot = map_si, width=14, height=10, dpi=300)


# trend estimates in individual class
results_df_group <- results_df %>%
  group_by(GSOC)
results_df_group$GSOC <- as.factor(results_df_group$GSOC)
range(results_df$Trend_Value,na.rm = T)

p_trend_class <- ggplot(data = results_df_group,aes(x=GSOC, y=Trend_Value,fill=Label)) + #,group=GSOC,color=Label
  geom_boxplot(position="dodge",outlier.shape = NA) + # color=NA,stat="identity",scale="width"
  # geom_half_boxplot(side = "r", errorbar.draw = FALSE, width=0.2, linewidth=0.5) +
  # geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
  scale_fill_manual(values = groupcolors) +
  coord_flip() +
  scale_y_continuous(limits = c(-1, 1)) +#, expand = c(0, 0)
  scale_x_discrete(labels = c(1:27)) +
  labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),x=paste0("Optical classes")) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
p_trend_class
ggsave("27oc_trends_models.png",dpi = 300, height = 15, width = 12, unit = 'in') # landscape


# NRMSE
range(results_df$NRMSE,na.rm = T)
# p <- ggplot(data = results_df,aes(x=GSOC, y=NRMSE,fill=Label)) + #,color=Label, group=GSOC
#   # geom_half_violin(side = "r",alpha=0.8,position="dodge",scale="width") + # color=NA
#   geom_half_boxplot(errorbar.draw = F, width=1, linewidth=0.5,position="dodge",outlier.shape = NA) +
#   # geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
#   scale_fill_manual(values = groupcolors) +
#   coord_flip() +
#   scale_y_continuous(limits = c(0,1), expand = c(0, 0)) +
#   scale_x_discrete(labels = c(c(1:29))) +
#   labs(y=expression(paste("NRMSE")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "bottom",
#         axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# p
p_nrmse <- ggplot(data = results_df_group,aes(x=GSOC, y=NRMSE,fill=Label)) +
  # geom_bar(errorbar.draw = F, width=1, linewidth=0.5,position="dodge",stat="identity",outlier.shape = NA) +
  geom_bar(width=0.6,position="dodge",stat="identity") +
  scale_fill_manual(values = groupcolors) +
  coord_flip() +
  scale_y_continuous(limits = c(0,0.5), expand = c(0, 0)) +
  scale_x_discrete(labels = c(1:27)) +
  labs(y=expression(paste("NRMSE")),x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
p_nrmse
ggsave("27oc_tr_nrmse_models.png",dpi = 300, height = 20, width = 10, unit = 'in') # landscape

# Bias
range(results_df$Bias,na.rm = T)
p_bias <- ggplot(data = results_df_group,aes(x=GSOC, y=Bias,fill=Label)) +
  # geom_bar(errorbar.draw = F, width=1, linewidth=0.5,position="dodge",stat="identity",outlier.shape = NA) +
  geom_bar(width=0.6,position="dodge",stat="identity") +
  scale_fill_manual(values = groupcolors) +
  coord_flip() +
  scale_y_continuous(limits = c(-1.3,0), expand = c(0, 0)) +
  scale_x_discrete(labels = c(c(1:27))) +
  labs(y=expression(paste("Bias")),x=NULL) +
  theme_classic() +
  theme(legend.position = "bottom",
        axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"))
p_bias
ggsave("27oc_tr_bias_models.png",dpi = 300, height = 20, width = 10, unit = 'in') # landscape


### Gather together (trend estimates)
layout <- "
AABB
CCDD
EEEE
"
gather <- p_trend + p_ci + p_nrmse + p_bias + p_bias_map
trend <- gather +
  plot_layout(ncol = 3, design=layout) + # widths = c(2,2,1)
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
trend 
ggsave(filename = paste0("figure_accurate_uncertainty.png"), plot = trend, width=12, height=16, dpi=300)

# # or
# gather <- (p_violin + p_nrmse) / (p_trend_class + p_bias)
# gather
# trend <- gather +
#   plot_layout(nrow = 2,byrow = F,widths = c(2,2,1,1)) +
#   plot_annotation(tag_levels = "a") &
#   theme(plot.tag = element_text(face = 'bold',size=30))
# trend 
# ggsave(filename = paste0("figure4_trend_1017.png"), plot = trend, width=20, height=12, dpi=300)

##### Part II. Homogenous of partition methods ##########
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/models_trend_df.Rdata")

## stco
stco_tab <- na.omit(stco_tab)
stco_n <- length(table(stco_tab$OCfix))
class <- stco_tab$OCfix[!duplicated(stco_tab$OCfix)]
stco_tr_sd <- matrix(NA,nrow=stco_n,ncol=2)
tick=1
for(i in class){
  print(tick)
  temp_df <- stco_tab[which(stco_tab$OCfix == i),]
  stco_tr_sd[tick,1] <- i
  stco_tr_sd[tick,2] <- sd(temp_df$Trend_Value,na.rm=T)
  tick <- tick+1
}
stco_tr_sd <- as.data.frame(stco_tr_sd)
colnames(stco_tr_sd) <- c('partition','sd')

## stcl
stcl_tab <- na.omit(stcl_tab)
stcl_n <- length(table(stcl_tab$Long))
region <- stcl_tab$Long[!duplicated(stcl_tab$Long)]
stcl_tr_sd <- matrix(NA,nrow=stcl_n,ncol=2)
tick=1
for(i in region){
  print(tick)
  temp_df <- stcl_tab[which(stcl_tab$Long == i),]
  stcl_tr_sd[tick,1] <- i
  stcl_tr_sd[tick,2] <- sd(temp_df$Trend_Value,na.rm=T)
  tick <- tick+1
}
stcl_tr_sd <- as.data.frame(stcl_tr_sd)
colnames(stcl_tr_sd) <- c('partition','sd')

## plot
plot_df <- rbind(stco_tr_sd,stcl_tr_sd)
plot_df$partition <- as.factor(plot_df$partition)
plot_df$method <- c(rep("STC-O",each=stco_n),rep("STC-L",each=stcl_n))
plot_df$method <- factor(plot_df$method,levels=c("STC-O","STC-L"))

p_sd <- ggplot(plot_df,aes(x=method,y=sd,group=method,fill=method)) +
  geom_boxplot(fill=c("STC-O"="goldenrod1","STC-L"="olivedrab3")) +
  scale_fill_manual(values=c("goldenrod1","olivedrab3"))+
  # scale_x_discrete(limits=c("Static optical class","Longhurst")) +
  ylab("Standard deviation") +
  xlab("") +
  guides(fill=guide_legend(title=NULL))+
  theme_bw() +
  theme(legend.position = "bottom",
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"),
        axis.title = element_text(face="bold",size=16,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.2),
        axis.line.y = element_line(color="black", linewidth = 0.2),
        axis.text = element_text(face="bold",size=13, color = "black"),
        panel.border = element_rect(colour = "black", fill=NA, linewidth =1))
p_sd
savename <- paste0("stc_homo_sd.png")
ggsave(savename,units="in",width=5, height=3, dpi=600)


##### Part III. 95% Credibility Interval ##########
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/models_trend_df.Rdata")
results_df <- rbind(dstc_tab,stco_tab,stcl_tab)
labeltemp <- c(rep(c("DSTC"),each=nrow(dstc_tab)),
               rep("STC-O",each=nrow(stco_tab)),
               rep("STC-L",each=nrow(stcl_tab)))
results_df$Model <- factor(labeltemp , levels=c("DSTC","STC-O","STC-L"))
# dstc_tab <- na.omit(dstc_tab)
# stco_tab <- na.omit(stco_tab)
# stcl_tab <- na.omit(stcl_tab)

groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") # pallette
# groupcolors <- c("#024378","#de7b34") # pallette
##### CI (26 optical class)
results_df$credi <- results_df$UCI-results_df$LCI
range(results_df$credi,na.rm=T)
### With significant p-value
# OR (BETTER!)
# Facet bar ****
# stat.test <- results_df %>%
#   group_by(GSOC) %>%
#   t_test(credi ~ Label) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance("p.adj")
# stat.test <- stat.test %>% 
#   add_xy_position(x = "Label",dodge = 0.8)
# 
# p_facet <- ggplot(data = results_df,aes(x=Label,y=credi,color=Label,group=Label)) + # x="GSOC",y="ci",color="Label",palette = groupcolors
#   geom_bar(width=0.6,position="dodge",stat="identity",aes(fill=Label)) + #,add = "mean_sd",
#   scale_color_manual(values = groupcolors) + # c("DSTC"="lightslateblue","STC"="goldenrod1")
#   scale_fill_manual(values = groupcolors) +
#   scale_y_continuous(limits = c(0,0.1), expand = c(0, 0)) +
#   scale_x_discrete(labels = c("DSTC","STC-O","STC-L")) +
#   labs(y=expression(paste("95% Credibility intervals")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.title.y = element_text(size=30, face="bold", colour = "black"),
#         axis.line.x = element_line(color="black", linewidth = 0.3),
#         axis.line.y = element_line(color="black", linewidth = 0.3),
#         axis.text = element_text(face="bold",size=16, color = "black"),
#         strip.text = element_text(face="bold", size=18,lineheight=5.0),
#         strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1))+
#   facet_wrap(~GSOC)
# p_facet <- p_facet+stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0,bracket.nudge.y = 0,
#                           bracket.size = 0.1,size = 8,linetype = 1) # bracket.nudge.y = -0.015
# p_facet
# ggsave("27oc_credi_facet_barsig.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape


# or violin half
p_ci <- ggplot(data = results_df,aes(x=Model, y=credi, fill=Model, group=Model)) +
  geom_half_violin(side = "r", color=NA, alpha=0.35) +
  geom_half_boxplot(side = "r",errorbar.draw = FALSE, width=0.2, linewidth=0.5,,outlier.shape = NA) +
  geom_half_point_panel(side = "l", shape=21, size=2, color="white") +
  scale_fill_manual(values = c("DSTC"="lightslateblue",
                               "STC-O"="goldenrod1","STC-L"="olivedrab3")) + # breaks = c('DSTC','STC-O','STC-L'),labels = c('DSTC','STC-O','STC-L')
  scale_y_continuous(limits = c(0.01, .08), expand = c(0, 0)) +
  # scale_x_discrete(labels = c('DSTC','STC-O','STC-L')) +
  labs(y=expression(paste("95% Credibility intervals width")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",legend.title.align = 0.5,
        axis.title = element_text(size = 18,face="bold",color = "black"),
        axis.text = element_text(size=16,face="bold",color = "black"))
p_ci
ggsave("global_credi_3models_violin.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

### Gather together (credibility interval)
layout <- "
AABB
"
gather <- p_trend + p_ci
comb <- gather +
  plot_layout(nrow = 1, ncol=2, byrow = T,design = layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
comb
ggsave(filename = paste0("figure_3model_tr_ci.0227.png"), plot = comb, width=14, height=6, dpi=300)

# results_df$credi <- results_df$credi*10
### CI (Uncertainty) map: DSTC
dstc_df <- results_df %>%
  filter(results_df$Model == 'DSTC')
p_credi_dstc <- ggplot() +
  geom_tile(data = dstc_df, aes(x = Longitude,y = Latitude,fill = credi)) +
  scale_fill_gradientn(colors = c("white","dodgerblue2","dodgerblue4"),
                       na.value = "white",limits=c(0,0.1),oob=squish) +
  coord_equal() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Uncertainty  ", "(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p_credi_dstc
hist(results_df$credi)
ggsave("si.credi.map.dstc.png",width=8.27, height=5, dpi=300)
### CI (Uncertainty) map: STC-O
stco_df <- results_df %>%
  filter(results_df$Model == 'STC-O')
p_credi_stco <- ggplot() +
  geom_tile(data = stco_df, aes(x = Longitude,y = Latitude,fill = credi)) +
  scale_fill_gradientn(colors = c("white","dodgerblue2","dodgerblue4"),
                       na.value = "white",limits=c(0,0.1),oob=squish) +
  coord_equal() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Uncertainty  ", "(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p_credi_stco
### CI (Uncertainty) map: STC-L
stcl_df <- results_df %>%
  filter(results_df$Model == 'STC-L')
p_credi_stcl <- ggplot() +
  geom_tile(data = stcl_df, aes(x = Longitude,y = Latitude,fill = credi)) +
  scale_fill_gradientn(colors = c("white","dodgerblue2","dodgerblue4"),
                       na.value = "white",limits=c(0,0.1),oob=squish) +
  coord_equal() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Uncertainty  ", "(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 18,angle = 90,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14),# ,face="bold"
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p_credi_stcl

layout <- "
AAA
BBB
"
gather <- p_credi_stco + p_credi_stcl
map <- gather +
  plot_layout(nrow = 2,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure_si_stc_uncertainty.png"), plot = map, width=14, height=12, dpi=300)


#
results_df$confi <- results_df$uci-results_df$lci
range(results_df$confi,na.rm=T)
# # Facet boxplot
# stat.test <- results_df %>%
#   group_by(GSOC) %>%
#   t_test(confi ~ Label) %>%
#   adjust_pvalue(method = "bonferroni") %>%
#   add_significance("p.adj")
# stat.test <- stat.test %>% 
#   add_xy_position(x = "Label",dodge = 0.8)
# 
# p <- ggplot(data = results_df,aes(x=Label,y=confi,color=Label,group=Label)) + # x="GSOC",y="ci",color="Label",palette = groupcolors
#   geom_bar(width=0.6,position="dodge",stat="identity",aes(fill=Label)) + #,add = "mean_sd",
#   scale_color_manual(values = groupcolors) + # c("DSTC"="lightslateblue","STC"="goldenrod1")
#   scale_fill_manual(values = groupcolors) +
#   scale_y_continuous(limits = c(0,0.0025), expand = c(0, 0)) +
#   scale_x_discrete(labels = c("DSTC","STC")) +
#   labs(y=expression(paste("95% Credibility intervals")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.title.y = element_text(size=30, face="bold", colour = "black"),
#         axis.line.x = element_line(color="black", linewidth = 0.3),
#         axis.line.y = element_line(color="black", linewidth = 0.3),
#         axis.text = element_text(face="bold",size=16, color = "black"),
#         strip.text = element_text(face="bold", size=18,lineheight=5.0),
#         strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1))+
#   facet_wrap(~GSOC)
# p <- p+stat_pvalue_manual(stat.test, label = "p.adj.signif", tip.length = 0,bracket.nudge.y = -0.015,
#                           bracket.size = 0.3,size = 10,linetype = 1) #
# p
# ggsave("26oc_confi_facet_boxsig.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape

################################################################################
########## Part III. Fitted VS Obs (Spearman) ##########
### DSTC vs STC
rm(list=ls())
setwd("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res")

load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/3models_res_trends_df.Rdata")
results_df <- rbind(dstc_tab,stco_tab,stcl_tab)
ndata <- nrow(dstc_tr_df)
labeltemp <- c(rep(c("DSTC","STC-O"),each=ndata),rep("STC-L",each=nrow(stcl_tr_df)))
results_df$Label <- factor(labeltemp , levels=c("DSTC","STC-O","STC-L"))
# results_df$Label <- as.factor(results_df$Label)
results_df$GSOC <- as.factor(results_df$GSOC)
results_df <- na.omit(results_df)

##### Obs CHL vs Fitted CHL
### two
range(results_df$Fitted)
results_df$Label <- factor(results_df$Label , levels=c("DSTC","STC-O","STC-L")) 
groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") # pallette

p <- ggplot(results_df,aes(x=Obs,y=Fitted,group=Label,color=Label)) +
  geom_point(alpha=0.8)+
  geom_abline(intercept=0, slope=1,color="#C9C5C4",linewidth=1.5) +
  scale_fill_manual(values = groupcolors) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values=groupcolors) +
  labs(y=expression(paste("Fitted CHL ", "(mg·m"^"-3",")")),
       x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
  theme_classic() +
  theme(legend.position = "none",
        axis.title = element_text(size=20, face="bold", colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1))+
  facet_wrap(~Label)
p
ggsave("fitted_obs_27models.png",dpi = 300, height =10, width = 15, unit = 'in') # landscape

# ### one
# stc_df <- results_df %>%
#   filter(Label == "STC")
# dstc_df <- results_df %>%
#   filter(Label == "DSTC")
# 
# p1 <- ggplot(stc_df,aes(x=Obs,y=Fitted)) +
#   geom_point(color="#66c5b8",alpha=0.8)+
#   geom_abline(intercept=0, slope=1,color="#C9C5C4",linewidth=1.5) +
#   labs(y=expression(paste("Fitted CHL ", "(mg·m"^"-3",")")),
#        x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# 
# p2 <- ggplot(dstc_df,aes(x=Obs,y=Fitted)) +
#   geom_point(color="#f6db2b",alpha=0.8)+
#   geom_abline(intercept=0, slope=1,color="#C9C5C4",linewidth=1.5) +
#   labs(y=expression(paste("Fitted CHL ", "(mg·m"^"-3",")")),
#        x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# p1+p2
# ggsave("fitted_obs_models.png",dpi = 300, height = 20, width = 30, unit = 'in') # landscape

# spearman test
spear.test <- cor.test(dstc_tr_df$Fitted,dstc_tr_df$Obs,method="spearman")
spear.test$estimate # rho: 0.97665
spear.test <- cor.test(stco_tr_df$Fitted,stco_tr_df$Obs,method="spearman")
spear.test$estimate # rho: 0.9809
stcl_tr_df <- na.omit(stcl_tr_df)
spear.test <- cor.test(stcl_tr_df$Fitted,stcl_tr_df$Obs,method="spearman")
spear.test$estimate # rho: 0.97266
# There are strong positive monotonic relationship between fitted and obs CHl. 
# dstc performs a better perfect poitive monotonic.

##### Obs CHL vs trend estimates
## one
# dstc_df <- results_df %>%
#   filter(Label == "DSTC")
# stc_df <- results_df %>%
#   filter(Label == "STC")
# p1 <- ggplot(dstc_df,aes(x=Obs,y=Trend_Value)) +
#   geom_point(color="#66c5b8",alpha=0.8)+
#   stat_smooth(method='loess', formula= y~x,color="#FE6500")+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),
#        x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# p1
# p2 <- ggplot(stc_df,aes(x=Obs,y=Trend_Value)) +
#   geom_point(color="#f6db2b",alpha=0.8)+
#   stat_smooth(method='loess', formula= y~x,color="#FE6500")+
#   scale_x_continuous(expand = c(0, 0)) +
#   scale_y_continuous(expand = c(0, 0)) +
#   labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),
#        x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
#   theme_classic() +
#   theme(axis.title = element_text(size = 16, color = "black"),
#         axis.text = element_text(size=13, color = "black"))
# p2
# p1+p2
# ggsave("trend_obs_models.png",dpi = 300, height = 20, width = 30, unit = 'in') # landscape

## two (facet)
groupcolors<-c("#2e4ba0", "#faa419") # pallette: blue and yellow

p <- ggplot(results_df,aes(x=Obs,y=Trend_Value,group=Label,color=Label)) +
  geom_point(alpha=0.8)+
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_color_manual(values=groupcolors) +
  labs(y=expression(paste("Trend ", "(%yr"^" -1",")")),
       x=expression(paste("Observed CHL ", "(mg·m"^"-3",")"))) +
  theme_classic() +
  theme(axis.title = element_text(size = 16, color = "black"),
        axis.text = element_text(size=13, color = "black"),
        legend.position = "none") +
  facet_wrap(~Label)
p
ggsave("trend_obs_27models.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape


# spearman test
spear.test.dstc <- cor.test(dstc_tr_df$Trend_Value,dstc_tr_df$Obs,method="spearman")
spear.test.dstc$estimate # rho: 0.3835
spear.test.stc <- cor.test(stco_tr_df$Trend_Value,stco_tr_df$Obs,method="spearman")
spear.test.stc$estimate # rho: 0.3334
spear.test.stc <- cor.test(stcl_tr_df$Trend_Value,stcl_tr_df$Obs,method="spearman")
spear.test.stc$estimate # rho: 0.26436

# There are strong positive monotonic relationship between trend estimates and obs CHl. 
# dstc performs a better perfect poitive monotonic.


##### trend estimated and obs CHL (R^2)
rsq <- function (x, y) cor(x, y) ^ 2
dstc_tr_df <- na.omit(dstc_tr_df)
stco_tr_df <- na.omit(stco_tr_df)
stcl_tr_df <- na.omit(stcl_tr_df)
R2_dstc <- rsq(dstc_tr_df$Trend_Value,dstc_tr_df$Obs) 
R2_dstc # 0.183155
R2_stc <- rsq(stco_tr_df$Trend_Value,stco_tr_df$Obs) 
R2_stc # 0.147174
R2_stc <- rsq(stcl_tr_df$Trend_Value,stcl_tr_df$Obs) 
R2_stc # 0.052151
# Larger R2 indicates that the model better explains all of the variability in the dependent variable.

dsct_credi <- results_df$credi[which(results_df$Label=="DSTC")]
mean(dsct_credi) # 0.036
quantile(dsct_credi) # 0.029

scto_credi <- results_df$credi[which(results_df$Label=="STC-O")]
mean(scto_credi) # 0.035
quantile(scto_credi) # 0.037

sctl_credi <- results_df$credi[which(results_df$Label=="STC-L")]
mean(sctl_credi) # 0.18
quantile(sctl_credi) # 0.036
# ##### Part III. Plotting on ocfix according to OC (boxplot) #####
# rm(list=ls())
# setwd("~/Coding/Dynamic_SPT/dynamic")
# load("/Users/doris_zhai/Coding/Dynamic_SPT/dynamic/modeled_chl_deviations.0508.Rdata")
# 
# Date <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
#             to = as.Date("2022/12/01",format="%Y/%m/%d"), 
#             by = "1 month")
# dev_all$date <- rep(Date,times=29)
# dev_all$ocfix <- as.character(dev_all$ocfix)
# dev_all$group <- as.factor(dev_all$group)
# # plot deviation
# groupcolors <- c("lightslateblue","goldenrod1") 
# ts_df <- dev_all %>%
#   group_by(group)
# 
# # Facet boxplot: sd
# p <- ggplot(data = dev_all,aes(x=group,y=sd,group=group)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
#   geom_boxplot(position="dodge",aes(fill=group)) + #,add = "mean_sd",,scale="width"
#   scale_fill_manual(values = groupcolors) +
#   scale_y_continuous(limits = c(0,0.05), expand = c(0, 0)) +
#   # scale_x_discrete(labels = c("STC","DSTC")) +
#   labs(y=expression(paste("Standard Deviation")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.title = element_text(face="bold",size=25,colour = "black"),
#         axis.line.x = element_line(color="black", linewidth = 0.5),
#         axis.line.y = element_line(color="black", linewidth = 0.5),
#         axis.text = element_text(face="bold",size=18, color = "black"),
#         strip.text = element_text(face="bold", size=18,lineheight=5.0),
#         strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
#         axis.text.x=element_text(angle=60, hjust=1),
#         plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
#   facet_wrap(~ocfix)
# p
# ggsave("30oc_sd_facet_box.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape
# 
# # Facet boxplot: rmse
# p <- ggplot(data = dev_all,aes(x=group,y=rmse,group=group)) + # x="OCfix",y="ci",color="Label",palette = groupcolors
#   geom_boxplot(position="dodge",aes(fill=group)) + #,add = "mean_sd",,scale="width"
#   scale_fill_manual(values = groupcolors) +
#   scale_y_continuous(limits = c(0,0.8), expand = c(0, 0)) +
#   # scale_x_discrete(labels = c("STC","DSTC")) +
#   labs(y=expression(paste("RMSE")),x=NULL) +
#   theme_classic() +
#   theme(legend.position = "none",
#         axis.title = element_text(face="bold",size=25,colour = "black"),
#         axis.line.x = element_line(color="black", linewidth = 0.5),
#         axis.line.y = element_line(color="black", linewidth = 0.5),
#         axis.text = element_text(face="bold",size=18, color = "black"),
#         strip.text = element_text(face="bold", size=18,lineheight=5.0),
#         strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
#         axis.text.x=element_text(angle=60, hjust=1),
#         plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))+
#   facet_wrap(~ocfix)
# p
# ggsave("30oc_rmse_facet_box.png",dpi = 300, height = 20, width = 20, unit = 'in') # landscape


########## Part III. Time series of monthly CHL with fitted LM (Facet OC) 
########## Part IV. Time series of monthly CHL with fitted LM (Facet Longhurst)
#############################################################################################
# Pallette option: c("coral1","lightslateblue","olivedrab3","goldenrod1","lightgray") # red, purple,green, yellow, gray
# groupcolors<-c("#2e4ba0", "#faa419") # pallette: blue and yellow
# groupcolors<-c("#629433", "#FEFF99") # pallette: green and yellow (#629433:pretty green)
# groupcolors<-c("#66c5b8", "#f6db2b") 

# width=7.5, height=3.44, dpi=300 # portriat

# p <- ggplot(daf,aes(x=Region,y=Trend,fill=corr))
# p+geom_hline(yintercept=-0.38,colour="tan2",size=0.9)+
#   geom_hline(yintercept=-0.036,colour="slateblue",size=0.9)+
#   geom_violin(adjust=2,scale="width")+
#   scale_fill_manual(values=c("slateblue","tan2"))+
#   coord_flip()+ 
#   theme_bw()+
#   ylab(expression(paste("Trend  ", "(%yr"^" -1",")"))) + 
#   guides(fill=guide_legend(title=NULL))+
#   theme(axis.ticks.length=unit(-0.1,"cm"),
#         axis.text.x = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")), 
#         axis.text.y = element_text(margin=unit(c(0.4,0.4,0.4,0.4), "cm")),
#         text=element_text(family="Times New Roman",size=16))+
#   scale_y_continuous(breaks=seq(-2,2,1),minor_breaks = seq(-2,2,1),limits=c(-2.4,2.4))    

# for(i in c(1:29)){
#   if(i==1){
#     print(i)
#     temp <- results_df %>%
#       filter(OCfix == i) %>%
#       add_significance()
#     tempci <- t_test(temp, ci ~ Label)
#     p_ci <- tempci
#   }else{
#     print(i)
#     temp <- results_df %>%
#       filter(OCfix == i) %>%
#       add_significance()
#     tempci <- t_test(temp, ci ~ Label)
#     p_ci <- rbind(p_ci,tempci)
#   }
# 
# }
# p_ci <- p_ci %>%
#   add_xy_position(x = "OCfix")
