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
results_df <- rbind(dstc_tab,stco_tab,stcl_tab)
labeltemp <- c(rep(c("DSTC"),each=nrow(dstc_tab)),
               rep("STC-O",each=nrow(stco_tab)),
               rep("STC-L",each=nrow(stcl_tab)))
results_df$Model <- factor(labeltemp , levels=c("DSTC","STC-O","STC-L"))

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

#### NRMSE ####
range(results_df$NRMSE,na.rm = T)

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


#### Bias ####
groupcolors<- c("#2e4ba0","#629433", "#FEFF99")
# groupcolors<-c("#66c5b8", "#f6db2b") # pallette
range(results_df$Bias,na.rm = T)

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
ggsave(filename = paste0("figure_nrmse_bias.png"), plot = map, width=14, height=10, dpi=300)


##### 95% Credibility Interval ##########
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/models_trend_df.Rdata")
results_df <- rbind(dstc_tab,stco_tab,stcl_tab)
labeltemp <- c(rep(c("DSTC"),each=nrow(dstc_tab)),
               rep("STC-O",each=nrow(stco_tab)),
               rep("STC-L",each=nrow(stcl_tab)))
results_df$Model <- factor(labeltemp , levels=c("DSTC","STC-O","STC-L"))


groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") # pallette
# groupcolors <- c("#024378","#de7b34") # pallette
results_df$credi <- results_df$UCI-results_df$LCI
range(results_df$credi,na.rm=T)

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

