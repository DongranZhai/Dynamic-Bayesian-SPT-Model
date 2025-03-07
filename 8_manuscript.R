library(dplyr)
library(oceanmap)
library(ggplot2)
library(rgdal)
library(mapproj)
library(scales)
library(viridis)
library(ggpubr) # for creating easily publication ready plots
library(patchwork) # organize ggplot figures
library(ggpmisc)
library(gghalves)
library(RColorBrewer)

#### Number of class presented on each grid cell #####
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_dynamic_fix_5res.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/gsoc66_df_res.Rdata")
# oc_result <- oc_result[which(oc_result$month == 223),]
# oc_result <- oc_result[,-c(3:5)]
# # create spatial points data frame
# coordinates(oc_result) <- ~ lon + lat
# # coerce to SpatialPixelsDataFrame
# gridded(oc_result) <- T
# # coerce to raster
# temp <- raster(oc_result)
# oc_mat <- raster2matrix(temp)
# rm(list=c("temp","oc_result"))
# # check if import is right
# table(oc_mat)
# ind <- which(is.na(oc_mat))
# coast_ind <- which(oc_glts ==11 | oc_glts ==12 | oc_glts ==13 | oc_glts ==14)
# oc_glts[coast_ind] <- NA

# Keep same NA regions with dominant optical class
# for(i in 1:304){
#   tempts <- oc_glts[,,i]
#   tempts[ind] <- NA
#   oc_glts[,,i] <- tempts
# }

class_count <- array(NA,dim=c(72,36))
for(i in c(1:72)){
  for(j in c(1:36)){
    tempts <- oc_glts[i,j,]
    class_count[i,j] <- length(table(tempts))
  }
}
# class_count <- as.numeric(class_count)
# Plot
lon <- seq(-179.5,179.5,5)
lat <- seq(-89.5,89.5,5)
class_number_raster <- matrix2raster(class_count , x = lon, y = lat, layer = 1)
class_number_df <- as.data.frame(class_number_raster,xy = T)
colnames(class_number_df) <-c('lon','lat','count')
rm(list=c('class_number_raster'))
# otl_df$count <- as.numeric(as.character(otl_df$count))
class_number_df$count <- as.factor(class_number_df$count)
class_number_df$count[which(class_number_df$count==0)] <- NA
lat_ind <- which(class_number_df$lat >=60 | class_number_df$lat <= -60)
class_number_df$count[lat_ind] <- NA
class_number_df <- oc_result %>%
  left_join(class_number_df,by =c("lon","lat"))
class_number_df <- na.omit(class_number_df)

wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea

# ##### create oc defination map plot
# optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
#                      '7'='#0fb9b1','8'='#badc58','9'='#32ff7e','10'='#FFCCBC','11'='#FFEB3B','12'='#FFA000')
# optical_palette <- c('0'='black','1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
#                      '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#ffbe76','11'='#FFA000','12'='#eb4d4b')
# optical_palette <- c('1'='#82589F','2'='#82589F','3'='#82589F','4'='#82589F','5'='#82589F','6'='#82589F',
                     # '7'='#D6A2E8','8'='#D6A2E8','9'='#D6A2E8','10'='#D6A2E8','11'='#D6A2E8','12'='#D6A2E8')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12')
# turbo_pal <- RColorBrewer::RColorBrewer(n = 12,option = "H")
# colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))
# figure 1b
plot_number <- ggplot() +
  geom_raster(data=class_number_df,aes(x=lon,y=lat,group=count,fill=count)) +
  # scale_fill_manual(values = optical_palette , na.value = 'black') +
  # scale_fill_gradientn(colors=c(low = "#82589F",high = "#D6A2E8",midpoint = 13)) + #  mid = "white",
  # scale_fill_viridis_c(option = "magma") +
  scale_fill_viridis(discrete = TRUE,option = "C") +
  coord_equal() + 
  geom_polygon(colour="black",size=0.25) +
  geom_polygon(data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.4) +
  guides(fill = guide_legend(title = 'Number of classes',title.position = "bottom",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(-175, 175), ylim = c(-75, 75), expand=F) + # ,ylim = c(-60, 60)
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") + # Globally Dominant Optical optical Class
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
plot_number
ggsave("class_counts.0301.png",width=8.5, height=5.4,dpi=300)

# figure 1a
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/gsoc66_df_res.Rdata")
oc_result$gsoc <- as.factor(oc_result$gsoc)
oc_result[which(oc_result$Regions=="Southern Ocean"),] <- NA
oc_result$gsoc[which(oc_result$gsoc==50)] <- 51
for(i in c(51:57)){
  oc_result$gsoc[which(oc_result$gsoc==i)] <- i-1
}
oc_result <- na.omit(oc_result)

turbo_pal <- viridis::viridis(n = 66,option = "H")
plot <- ggplot() +
  geom_raster(data=oc_result,aes(x=lon,y=lat,group=Regions,fill=gsoc)) +
  scale_fill_manual(values = turbo_pal,na.value = 'black') + # 
  coord_equal() + 
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.4 ) +
  guides(fill = guide_legend(title = 'Dominant optical class',title.position = "bottom",
                             title.hjust = .5, show.limits = T,label.position = 'bottom', 
                             direction = 'horizontal',nrow = 2, byrow = T)) + #color = 'legend'
  coord_cartesian(xlim = c(-175, 175),ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") + # Geographically Separated Optical optical Class
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
plot
# combine figures
layout <- "
AAA
BBB
"
gather <- plot + plot_number
map <- gather +
  plot_layout(nrow = 2,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure_gsoc_number_0227.1.png"), plot = map, width=16, height=20, dpi=300)

##### Pick specific grid cells, TS of CHL with colored background of number of classes (SI) ####
### CHL deseasonality
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_nna_df.Rdata")

## Setting date formate
Date <- seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
            to = as.Date("2022/12/01",format="%Y/%m/%d"), 
            by = "1 month")
n <- length(table(global_no_na$s.index))
global_no_na$date <- rep(Date,times=n)
## Select grid cells
site <- global_no_na$s.index[!duplicated(global_no_na$s.index)]
select_grids <- vector(mode="logical",length=304)
for(i in c(1:304)){
  ind <- global_no_na$oc[which(global_no_na$s.index==site[i])]
  select_grids[i] <- sum(is.na(ind))
}
site <- site[which(select_grids==0)] # Pick from these sites
# tt <- global_no_na$s.index[!duplicated(global_no_na$s.index[which(global_no_na$ocfix==1)])]

####### example 1: site 579
## chl decompose ts
chl_df <- global_no_na[which(global_no_na$s.index==579),]
chl <- chl_df$chl
chl_decompose <- stl(ts(chl,start = c(1997,9),end = c(2022,12),frequency = 12),s.window="periodic")
chl_df$chl <- as.numeric(chl_decompose$time.series[,2])
range(chl_df$chl)
# plot(temp$time.series[,2]) # trend
## oc data frame
oc_df <- data.frame(start=seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
                                      to = as.Date("2022/12/01",format="%Y/%m/%d"), 
                                      by = "1 month"),
                    end=seq(from = as.Date("1997/10/01",format="%Y/%m/%d"), 
                              to = as.Date("2023/01/01",format="%Y/%m/%d"), 
                              by = "1 month"),
                    oc=as.factor(chl_df$oc))
# plot
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#ffbe76','11'='#FFA000','12'='#eb4d4b')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12')
p_site_579 <- ggplot(data=chl_df)+
  geom_line(data=chl_df,aes(x=Date,y=chl),linewidth=0.8) +
  geom_rect(data=oc_df,aes(ymin=0.2,ymax=0.45,
                           xmin=start,xmax=end,
                         fill=oc), alpha =0.5) +
  scale_fill_manual(values = optical_palette) +
  guides(fill = guide_legend(title = 'optical class',title.position = "left",title.vjust = 0.8, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(as.Date("1997/09/01",format="%Y/%m/%d"),as.Date("2023/01/01",format="%Y/%m/%d")),
                  ylim = c(0.2,0.45), expand=F) + 
  labs(x=expression(paste("Time (month)")),
       y=expression(paste("CHL  ","(mg·m"^"-3",")"))) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
p_site_579
ggsave("site_577_ts_oc.png",width=8.2, height=4.3,dpi=300)

####### example 2: site 633
## chl decompose ts
chl_df <- global_no_na[which(global_no_na$s.index==633),]
chl <- chl_df$chl
chl_decompose <- stl(ts(chl,start = c(1997,9),end = c(2022,12),frequency = 12),s.window="periodic")
chl_df$chl <- as.numeric(chl_decompose$time.series[,2])
range(chl_df$chl)
# plot(temp$time.series[,2]) # trend
## oc data frame
oc_df <- data.frame(start=seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
                              to = as.Date("2022/12/01",format="%Y/%m/%d"), 
                              by = "1 month"),
                    end=seq(from = as.Date("1997/10/01",format="%Y/%m/%d"), 
                            to = as.Date("2023/01/01",format="%Y/%m/%d"), 
                            by = "1 month"),
                    oc=as.factor(chl_df$oc))
# plot
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#ffbe76','11'='#FFA000','12'='#eb4d4b')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12')
p_site_633 <- ggplot(data=chl_df)+
  geom_line(data=chl_df,aes(x=Date,y=chl),linewidth=0.8) +
  geom_rect(data=oc_df,aes(ymin=0.15,ymax=0.25,
                           xmin=start,xmax=end,
                           fill=oc), alpha =0.5) +
  scale_fill_manual(values = optical_palette) +
  guides(fill = guide_legend(title = 'optical class',title.position = "left",title.vjust = 0.8, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(as.Date("1997/09/01",format="%Y/%m/%d"),as.Date("2023/01/01",format="%Y/%m/%d")),
                  ylim = c(0.15,0.25), expand=F) + 
  labs(x=expression(paste("Time (month)")),
       y=expression(paste("CHL  ","(mg·m"^"-3",")"))) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
p_site_633
ggsave("site_633_ts_oc.png",width=8.2, height=4.3,dpi=300)

####### example 3: site 799
## chl decompose ts
chl_df <- global_no_na[which(global_no_na$s.index==799),]
chl <- chl_df$chl
chl_decompose <- stl(ts(chl,start = c(1997,9),end = c(2022,12),frequency = 12),s.window="periodic")
chl_df$chl <- as.numeric(chl_decompose$time.series[,2])
range(chl_df$chl)
# plot(temp$time.series[,2]) # trend
## oc data frame
oc_df <- data.frame(start=seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
                              to = as.Date("2022/12/01",format="%Y/%m/%d"), 
                              by = "1 month"),
                    end=seq(from = as.Date("1997/10/01",format="%Y/%m/%d"), 
                            to = as.Date("2023/01/01",format="%Y/%m/%d"), 
                            by = "1 month"),
                    oc=as.factor(chl_df$oc))
# plot
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#ffbe76','11'='#FFA000','12'='#eb4d4b')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12')
p_site_799 <- ggplot(data=chl_df)+
  geom_line(data=chl_df,aes(x=Date,y=chl),linewidth=0.8) +
  geom_rect(data=oc_df,aes(ymin=0.04,ymax=0.085,
                           xmin=start,xmax=end,
                           fill=oc), alpha =0.5) +
  scale_fill_manual(values = optical_palette) +
  guides(fill = guide_legend(title = 'optical class',title.position = "left",title.vjust = 0.8, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(as.Date("1997/09/01",format="%Y/%m/%d"),as.Date("2023/01/01",format="%Y/%m/%d")),
                  ylim = c(0.04,0.085), expand=F) + 
  labs(x=expression(paste("Time (month)")),
       y=expression(paste("CHL  ","(mg·m"^"-3",")"))) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
p_site_799
ggsave("site_799_ts_oc.png",width=8.2, height=4.3,dpi=300)

####### example 4: site 794
## chl decompose ts
chl_df <- global_no_na[which(global_no_na$s.index==794),]
chl <- chl_df$chl
chl_decompose <- stl(ts(chl,start = c(1997,9),end = c(2022,12),frequency = 12),s.window="periodic")
chl_df$chl <- as.numeric(chl_decompose$time.series[,2])
range(chl_df$chl)
# plot(temp$time.series[,2]) # trend
## oc data frame
oc_df <- data.frame(start=seq(from = as.Date("1997/09/01",format="%Y/%m/%d"), 
                              to = as.Date("2022/12/01",format="%Y/%m/%d"), 
                              by = "1 month"),
                    end=seq(from = as.Date("1997/10/01",format="%Y/%m/%d"), 
                            to = as.Date("2023/01/01",format="%Y/%m/%d"), 
                            by = "1 month"),
                    oc=as.factor(chl_df$oc))
# plot
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#ffbe76','11'='#FFA000','12'='#eb4d4b')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12')
p_site_794 <- ggplot(data=chl_df)+
  geom_line(data=chl_df,aes(x=Date,y=chl),linewidth=0.8) +
  geom_rect(data=oc_df,aes(ymin=0.045,ymax=0.15,
                           xmin=start,xmax=end,
                           fill=oc), alpha =0.5) +
  scale_fill_manual(values = optical_palette) +
  guides(fill = guide_legend(title = 'optical class',title.position = "left",title.vjust = 0.8, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(as.Date("1997/09/01",format="%Y/%m/%d"),as.Date("2023/01/01",format="%Y/%m/%d")),
                  ylim = c(0.045,0.15), expand=F) + 
  labs(x=expression(paste("Time (month)")),
       y=expression(paste("CHL  ","(mg·m"^"-3",")"))) +
  theme_bw() +
  theme(legend.position = 'bottom',
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
p_site_794
ggsave("site_794_ts_oc.png",width=8.2, height=4.3,dpi=300)

## combine
layout <- "
AAAA
BBCC
DDEE
"
gather <- plot_number + p_site_579 + p_site_633 + p_site_794 + p_site_799
map_5 <- gather +
  plot_layout(nrow = 3,ncol=2,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map_5
ggsave(filename = paste0("figure_4ts+counted_oc.0301.png"), plot = map_5, width=16, height=20, dpi=300)

##### Table of global integral ######
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/models_trend_df.Rdata")
temptr <- 2 * ((stcl_tab[,4] - min(stcl_tab[,4],na.rm=T)) / 
               (-max(stcl_tab[,4],na.rm=T) + min(stcl_tab[,4],na.rm=T))) + 1
stcl_tab[,4] <- temptr
templci <- 2 * ((stcl_tab[,7] - min(stcl_tab[,7],na.rm=T)) / 
                 (-max(stcl_tab[,7],na.rm=T) + min(stcl_tab[,7],na.rm=T))) + 1
stcl_tab[,7] <- templci
tempuci <- 2 * ((stcl_tab[,8] - min(stcl_tab[,8],na.rm=T)) / 
                  (-max(stcl_tab[,8],na.rm=T) + min(stcl_tab[,8],na.rm=T))) + 1
stcl_tab[,8] <- tempuci
# range(temptr,na.rm=T)
dstc_tab <- dstc_tab[,-c(1:3,9:11)]
stco_tab <- stco_tab[,-c(1:3,9:11)]
stcl_tab <- stcl_tab[,-c(1:3,9:11)]

table_sum <- array(NA,dim=c(3,8))
colnames(table_sum) <- c('Model',	'Trend_Value', 'Obs', 'Fitted', 'Lower_CI',
                         'Upper_CI',	'NRMSE',	'Bias')
for(j in c(2:8)){
  
  print(j)
  table_sum[1,j] <- round(mean(dstc_tab[,j-1],na.rm=T),digits = 2)
  table_sum[2,j] <- round(mean(stco_tab[,j-1],na.rm=T),digits = 2)
  table_sum[3,j] <- round(mean(stcl_tab[,j-1],na.rm=T),digits = 2)
  
}
table_sum <- as.data.frame(table_sum)
table_sum$Model <- c('DSTC','STC-O','STC-L')
table_sum$CI <- table_sum$Upper_CI - table_sum$Lower_CI
write.csv(table_sum,
          file="manuscript_global_integral_table.csv")

##### Chlorophyll monthly raster #####
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/occci_chl_5res.Rdata")

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea

chl_raster  <- chl_res[,,289] # chl_npsg
chl_raster <- matrix2raster(chl_raster, x = lon, y = lat, layer = 1)
chl_df <- as.data.frame(chl_raster ,xy = T)
colnames(chl_df) <-c('lon','lat','values')

p <- ggplot() +
  geom_raster( data = chl_df , aes(x = lon,y = lat,fill = values)) +
  # scale_fill_gradientn(colours = optical_palette , na.value = 'black') +
  scale_fill_gradientn(colours = oce::oce.colorsJet(120), na.value = "white",limits = c(0,3.2))+  
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.5) +
  coord_cartesian(ylim = c(-75, 75),xlim = c(-170, 170),expand=F) +
  # scale_x_continuous(breaks = seq(-180,180,60)) + # npsg
  scale_x_continuous(breaks = seq(-180,180,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title = expression(Chlorophyll-a~(mg~m^{-3})),
                               title.position = "right",
                               title.theme = element_text(angle = 90),
                               barwidth = unit(.5, "cm"),barheight = unit(7.5, "cm"),title.hjust = .5))+
  labs(x = "Longitude", y = "Latitude") + # title = paste0('Chl ', class_number),title.hjust=.5,
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,linewidth =0.5),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
p
ggsave(filename="chlorophyll_concentration_raster_202109.png",width=8.27, height=4.44, dpi=300)

##### Optical class monthly raster #####
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/gsoc66_df_mat_res.Rdata")
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_dynamic_fix_5res.Rdata")
# 1, 13, 25, 289, 301

##### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data/", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]
turbo_pal <- viridis::viridis(n = 66,option = "H")
# optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
#                      '7'='#32ff7e','8'='#badc58','9'='#FFEB3B') # ,'10'='#FFA000'
# label <- c('1','2','3','4','5','6','7','8','9') # ,'10','11','12'

##### oc count and filled
lat <- seq(-89.5,89.5,by = 5)
lon <- seq(-179.5,179.5,by = 5)

otl <- oc_mat[,,301]
otl_raster <- matrix2raster(otl, x = lon, y = lat, layer = 1)
otl_df <- as.data.frame(otl_raster,xy = T)
colnames(otl_df) <-c('longitude','latitude','count')
rm(list=c('otl_raster'))

##### create oc defination map plot
plot <- ggplot() +
  geom_raster(data=otl_df,aes(x=longitude,y=latitude,group=count,fill=factor(count))) +
  scale_fill_manual(values = turbo_pal,na.value = 'black') + # 
  coord_equal() + 
  geom_polygon(colour="black",size=0.25) +
  geom_polygon(data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.5 ) +
  guides(fill = guide_legend(title = 'class number',title.position = "bottom",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(-170, 170),ylim = c(-75, 75), expand=F) + # 
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(x = "Longitude", y = "Latitude") + # Globally Dominant Optical optical Class
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,linewidth =0.5),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
plot
ggsave("optical_class_raster_gsoc_202209.png",width=8.2, height=4.3,dpi=300)


##### flowchart plot #####
#### gamma
k <- c(1, 2, 2)
mu <- c(1, 1, 2)
theta <- mu/k
png("gamma_dist.png")
plot(0, 0, xlim = c(0, 10), ylim = c(0, 1), type = "n",xaxt='n',yaxt='n',ann=FALSE)
curve(dgamma(x, shape = k[2], scale = theta[2]), from = 0, to = 10, col = 'orange', add = TRUE,lwd=7)
dev.off()

### normal
x <- seq(-4, 4, length=100)
y <- dnorm(x)
png("normal_dist.png")
plot(0, 0, xlim = c(-4, 4), ylim = c(0, 0.5), type = "n",xaxt='n',yaxt='n',ann=FALSE)
curve(dnorm(x), from = -4, to = 4, col = 'orange', add = TRUE,lwd=7, axes = FALSE,type = "l")
dev.off()

### fixed
png("fixed_dist.png")
plot(0, 0, xlim = c(-4, 4), ylim = c(0, 1), type = "n",xaxt='n',yaxt='n',ann=FALSE)
lines(c(0, 0), c(0.05, 0.95),col = 'orange',lwd=7, type = "l")
dev.off()

#### posterior
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/3models_res_trends_df.Rdata")
# hist(betasp[20,]*100, breaks = 30, col = "lightgreen") #  xlim = c(0, 1),
tick=3 # sub_site[20]
model <- c('OCCCI')
bounds_df <- matrix(NA, nrow=1,ncol=2)
betasp_df <- matrix(NA, nrow=1,ncol=5000)
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/dstc/dstc_42/3_res_dstc_df.Rdata")
betasp_df[1,] <- betasp[tick,]*100

plot_df <- data.frame(trend=NA,model=NA)
for(i in c(1)){
  bounds_df[i,] <- c(mean(betasp_df[i,])-2*sd(betasp_df[i,]),mean(betasp_df[i,])+2*sd(betasp_df[i,]))
  temp_betasp <- data.frame(trend=betasp_df[i,],model=model[i])
  plot_df <- rbind(plot_df,temp_betasp)
}
plot_df <- plot_df[-1,]
labeltemp <- rep(c("OCCCI"),each=5000)
plot_df$model <- factor(labeltemp, levels=c("OCCCI"))

# plot
# groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") # "OCCCI"="#0652DD","CMEMS"="#D980FA"
plot <- ggplot(plot_df, aes(trend,color=model)) + 
  geom_density(alpha = 0.2,linewidth=1.3) +
  coord_cartesian(expand=F) +# xlim = c(-0.04, 0.02),ylim = c(0, 80),
  scale_color_manual(values = c("OCCCI"="#a29bfe")) +
  geom_vline(xintercept = bounds_df[1,1],linewidth=1.3,
             color = "#a29bfe", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[1,2],linewidth=1.3,
             color = "#a29bfe", linetype = "dashed") +
  labs(x=expression(paste("Trend  ", "(%·yr"^" -1",")")),y='Probability density') +
  theme_bw() +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,linewidth =0.5),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
plot
ggsave("posterior_density_occci_1363.png",width=6, height=4.3,dpi=300)

# tick=3 # sub_site[20]
# model <- c('OCCCI','CMEMS')
# bounds_df <- matrix(NA, nrow=2,ncol=2)
# betasp_df <- matrix(NA, nrow=2,ncol=5000)
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/dstc/dstc_42/3_res_dstc_df.Rdata")
# betasp_df[1,] <- betasp[tick,]*100
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_cmems/dstc_42/3_res_dstc_df_cmems.Rdata")
# betasp_df[2,] <- betasp[tick,]*100
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/stc_long/formula-spT-long27/3_long_dstc_df.Rdata")
# betasp_df[3,] <- betasp[tick,]*100

# plot <- ggplot(plot_df, aes(trend,color=model)) + 
#   geom_density(alpha = 0.2,linewidth=1.3) +
#   coord_cartesian(expand=F) +# xlim = c(-0.04, 0.02),ylim = c(0, 80),
#   scale_color_manual(values = c("OCCCI"="#0652DD","CMEMS"="#D980FA")) +
#   geom_vline(xintercept = bounds_df[1,1],linewidth=1.3,
#              color = "#0652DD", linetype = "dashed") +
#   geom_vline(xintercept = bounds_df[1,2],linewidth=1.3,
#            color = "#0652DD", linetype = "dashed") +
#   geom_vline(xintercept = bounds_df[2,1], linewidth=1.3,
#              color = "#D980FA", linetype = "dashed") +
#   geom_vline(xintercept = bounds_df[2,2], linewidth=1.3,
#              color = "#D980FA", linetype = "dashed") +
#   labs(x=expression(paste("Trend  ", "(%·yr"^" -1",")")),y='Probability density') +
#   theme_bw() +
#   theme(legend.position = 'none',
#         panel.background = element_blank(),
#         panel.border = element_rect(colour = "black", fill=NA,linewidth =0.5),
#         plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
# plot
# ggsave("posterior_density_2models_1363.png",width=6, height=4.3,dpi=300)
# # stco
# stco_df <- data.frame(trend=betasp[20,]*100,model="STC-O")
# hist(betasp[20,]*100, breaks = 30, col = "lightgreen") #  xlim = c(0, 1),
# # stcl
# stcl_df <- data.frame(trend=betasp[20,]*100,model="STC-L")
# hist(betasp[20,]*100, breaks = 30, col = "lightgreen") #  xlim = c(0, 1),
# 
# plot_df <- rbind(dstc_df,stco_df)

# dstc_tr_df[which(dstc_tr_df$Longitude== -119.5 & dstc_tr_df$Latitude == 5.5),]
# stco_tr_df[which(stco_tr_df$Longitude== -119.5 & stco_tr_df$Latitude == 5.5),]
# stcl_tr_df[which(stcl_tr_df$Longitude== -119.5 & stcl_tr_df$Latitude == 5.5),]

##### Difference Comparison of Two DSTC trends (OCCCI vs CMEMS) ONE #####
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_cmems/oc_res_dstc_spT_tpoc_42resultTab_cmems.Rdata")
results_cmems <- as.data.frame(results_tab)
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_spT_tpoc_42resultTab.Rdata")
results_occci <- as.data.frame(results_tab)

test.t <- t.test(results_occci$Trend_Value,results_cmems$Trend_Value, paired = F)
test.u <- wilcox.test(results_occci$Trend_Value,results_cmems$Trend_Value, paired = F)
# test.chi <- chisq.test(table(na.omit(results_occci$Trend_Value), na.omit(results_cmems$Trend_Value)))
print(test.t) # p-value = 0.931
print(test.u)

# data frame
labeltemp <- rep(c("OCCCI","GlobColour"),each=72*36)
comp_df <-data.frame(Trend_Value=c(as.vector(results_occci$Trend_Value),as.vector(results_cmems$Trend_Value)),
                      Dataset=factor(labeltemp , levels=c("OCCCI","GlobColour")))

# plot
# whole violin plot
p_violin <- ggplot(data = comp_df,aes(x=Dataset, y=Trend_Value, fill=Dataset, group=Dataset)) +
  geom_violin(color=NA, alpha=0.35) +
  geom_boxplot(width=0.2, linewidth=0.5,outlier.shape = NA) + # errorbar.draw = FALSE,
  scale_fill_manual(values = c("OCCCI"="#0652DD","GlobColour"="#D980FA")) +
  scale_y_continuous(limits = c(-1, 1), expand = c(0, 0)) +
  scale_x_discrete(labels = as.factor(c('OCCCI','GlobColour'))) +
  # stat_summary(fun = mean, geom = "point", shape = 20, size = 3, color = "red") +
  labs(y=expression(paste("Trend  ", "(%·yr"^" -1",")")),x=NULL) +
  theme_classic() +
  theme(legend.position = "none",legend.title.align = 0.5,
        axis.title = element_text(size = 18,face="bold",color = "black"),
        axis.text = element_text(size=16,face="bold",color = "black"))
p_violin
ggsave("robust_comparison.png",dpi = 300, height = 5, width = 5, unit = 'in') # landscape

# TWO
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")
#### coordinate
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')

load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_cmems/oc_res_dstc_spT_tpoc_42resultTab_cmems.Rdata")
results_cmems <- as.data.frame(results_tab)
results_cmems <- left_join(temp,results_cmems,by='Site')
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_spT_tpoc_42resultTab.Rdata")
results_occci <- as.data.frame(results_tab)
results_occci <- left_join(temp,results_occci,by='Site')
site <- 1:2592
n <- length(results_occci$Trend_Value)
diff_value <- results_occci$Trend_Value - results_cmems$Trend_Value
diff_sign <- vector(mode = "logical", length = n)
for(i in site){
  # tick <- which(results_occci$Site == 1)
  #  results_occci$Trend_Value[which(results_occci$Site == 306)]
  tr_occci <- results_occci$Trend_Value[which(results_occci$Site == i)]
  tr_cmems <- results_cmems$Trend_Value[which(results_cmems$Site == i)]
  if(is.na(tr_occci) | is.na(tr_cmems)){
    diff_sign[i] <- "miss"
  }else if(is.na(tr_occci) & !is.na(tr_cmems)){
    diff_sign[i] <- "miss"
  }else if(!is.na(tr_occci) & is.na(tr_cmems)){
    diff_sign[i] <- "miss"
  }else if(tr_occci > 0 & tr_cmems < 0){
    diff_sign[i] <- "opposite"
  }else if(tr_occci < 0 & tr_cmems > 0){
    diff_sign[i] <- "opposite"
  }else{
    diff_sign[i] <- "same"
  }
}
diff_df <- data.frame(Site=results_occci$Site,Value=diff_value,Sign=as.factor(diff_sign))

# lon and lat have already been centred
comb_df <- full_join(temp,diff_df,by='Site')
comb_df$Sign <- as.factor(comb_df$Sign)
# set NA
ind.na <- which(comb_df$Latitude > 60 | comb_df$Latitude < -60)
comb_df$Value[ind.na] <- NA
comb_df$Sign[ind.na] <- "miss"

# ## Stipple grid cells that exclude zero
# comb_df$stipple <- NA
# ind <- which(is.na(comb_df$Value))
# comb_df$stipple[ind] <- "no" # NA
# ind <- which(comb_df$Sign == "big")
# comb_df$stipple[ind] <- "yes" # big

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# col_pallete <- c("#f0932b","#ffbe76","#badc58","#6ab04c") # orange,green
col_pallete <- brewer.pal(8,'PiYG')

plot_diff <- ggplot() +
  geom_tile(data = comb_df , aes(x = Longitude,y = Latitude,fill = Value)) +
  scale_fill_gradientn(colors = c("magenta", "white", "green"),na.value = "white",limits=c(-1,1),
                       values = scales::rescale(c(-2, -1, 1, 2)),oob=squish) +
  # scale_fill_gradientn(colors = c("magenta", "white", "green"),na.value = "white",
  #   values = rescale(c(min(comb_df$Value,na.rm=T), -1, 1, max(comb_df$Value,na.rm=T))),  
  #   limits = c(min(comb_df$Value,na.rm=T), max(comb_df$Value,na.rm=T)),oob = squish) +
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=Sign,shape=Sign,size=Sign),show.legend = F) +
  scale_size_manual(values=c(miss=1.2,opposite=2,same=1.2)) +
  scale_shape_manual(values=c(miss=4,opposite=17,same=1)) +
  scale_color_manual(values = c(same = alpha("white", 0),opposite = alpha("#353b48", 0.8),
                                miss = alpha("#353b48", 0.5))) + # 
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend difference ", "(%yr"^" -1",")")),
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
plot_diff
ggsave("diff_products.png",width=8.27, height=3.44, dpi=300)

### Gather together (trend estimates) trend + map
layout <- "
#A#
BBB
"
gather <- p_violin + plot_diff
map <- gather +
  plot_layout(nrow = 2,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure_dstc_products_robust_0227.png"), plot = map, width=14, height=12, dpi=300)

##### Longhurst regions #####
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
# Longhurst outline
long<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="Longhurst_world_v4_2010")
long_df <- fortify(long)
# land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]
#centrepoints for region labelling
long_df$lonc <- NA
long_df$latc <- NA
centreps_df <- as.data.frame(coordinates(long))
centreps_df[5,] <- c(-55.3,40.7) #small number of regions centrepoints not optimal
centreps_df[33,] <- c(149.3,39) #small number of regions centrepoints not optimal
centreps_df[36,] <- c(160.6,-36.3)
centreps_df[51,] <- c(75,-36.2)

for(i in c(4,5,6,7,8,9,10,18,22,23,31,32,33,34,35,36,37,38,39,40,41,51,52)){
  long_df$lonc[which(long_df$id==i)] <- centreps_df$V1[i]
  long_df$latc[which(long_df$id==i)] <- centreps_df$V2[i] 
}

# correct labels
long_df$trueid <- NA
provs_order <- c(NA,NA,NA,13,11,15,4,10,1,8,NA,NA,NA,NA,NA,NA,NA,14,NA,NA,NA,2,3,NA,NA,NA,NA,NA,NA,NA,18,19,12,16,17,23,20,6,5,7,9,NA,NA,NA,NA,NA,NA,NA,NA,NA,21,22,NA,NA)#slightly different due to null region
for(i in 1:54){
  long_df$trueid[which(long_df$id==i)] <- provs_order[i]
}
plot <- ggplot(data=long_df,aes(x=long,y=lat,group=group)) +
  # scale_fill_gradientn(colors = colorPalette,na.value = "black",limits=c(-3,3),oob=squish) +
  geom_polygon(colour="black",fill="lightblue",size=0.25) +
  geom_text(data=long_df,aes(x=lonc,y=latc,label=trueid,family="Times"),size=4) +
  geom_polygon(data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  coord_equal() +
  guides(fill=guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  annotate("text",x=-133,y=-38.1, label="21",size=4,colour='black',family="Times") +
  theme_bw() +
  theme(legend.position = 'bottom',
        legend.title = element_text(size = 18,face="bold"),
        panel.background = element_blank(),
        plot.title = element_text(size = 18, face = "bold"),
        legend.text=element_text(size=14,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
plot
ggsave("long23_outline.0301.png",width=8.27, height=4.6, dpi=300)

##### Probability distribution #####
tick=3 # sub_site[20]
Dataset <- c('OCCCI','GlobColour')
bounds_df <- matrix(NA, nrow=2,ncol=2)
betasp_df <- matrix(NA, nrow=2,ncol=5000)
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/dstc/dstc_42/13_res_dstc_df.Rdata")
betasp_df[1,] <- betasp[tick,]*100
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_cmems/dstc_42/13_res_dstc_df_cmems.Rdata")
betasp_df[2,] <- betasp[tick,]*100
# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/stc_long/formula-spT-long27/3_long_dstc_df.Rdata")
# betasp_df[3,] <- betasp[tick,]*100

plot_df <- data.frame(trend=NA,Dataset=NA)
for(i in c(1,2)){
  bounds_df[i,] <- c(mean(betasp_df[i,])-2*sd(betasp_df[i,]),mean(betasp_df[i,])+2*sd(betasp_df[i,]))
  temp_betasp <- data.frame(trend=betasp_df[i,],Dataset=Dataset[i])
  plot_df <- rbind(plot_df,temp_betasp)
}
plot_df <- plot_df[-1,]
labeltemp <- rep(c("OCCCI","GlobColour"),each=5000)
plot_df$Dataset <- factor(labeltemp, levels=c("OCCCI","GlobColour"))
# with label
plot_13oc_3cell <- ggplot(plot_df, aes(trend,color=Dataset)) +
  geom_density(alpha = 0.2,linewidth=1.3) +
  coord_cartesian(expand=F) +# xlim = c(-0.04, 0.02),ylim = c(0, 80),
  scale_color_manual(values = c("OCCCI"="#0652DD","GlobColour"="#D980FA")) +
 geom_vline(xintercept = bounds_df[1,1],linewidth=1.3,
             color = "#0652DD", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[1,2],linewidth=1.3,
           color = "#0652DD", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[2,1], linewidth=1.3,
               color = "#D980FA", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[2,2], linewidth=1.3,
             color = "#D980FA", linetype = "dashed") +
  labs(x=expression(paste("Trend  ", "(%·yr"^" -1",")")),y='Probability density',
       title = 'Antarctic Circumpolar Current') +
  theme_bw() +
  theme(legend.position = 'bottom',
      panel.background = element_blank(),
      plot.title = element_text(size = 18, face = "bold",hjust = 0.5),
      legend.text=element_text(size=14,face="bold"),
      legend.title=element_text(size=18,face="bold"),
      axis.text=element_text(size=16,face="bold"),
      axis.title=element_text(size=18,face="bold"),
      panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
      plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
plot_13oc_3cell

# no label
plot_3oc_3cell <- ggplot(plot_df, aes(trend,color=Dataset)) +
  geom_density(alpha = 0.2,linewidth=1.3) +
  coord_cartesian(expand=F) +# xlim = c(-0.04, 0.02),ylim = c(0, 80),
  scale_color_manual(values = c("OCCCI"="#0652DD","GlobColour"="#D980FA")) +
  geom_vline(xintercept = bounds_df[1,1],linewidth=1.3,
             color = "#0652DD", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[1,2],linewidth=1.3,
             color = "#0652DD", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[2,1], linewidth=1.3,
             color = "#D980FA", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[2,2], linewidth=1.3,
             color = "#D980FA", linetype = "dashed") +
  labs(x=expression(paste("Trend  ", "(%·yr"^" -1",")")),y='Probability density') +
  theme_bw() +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,linewidth =0.5),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
plot_3oc_3cell

# combine
layout <- "
AABB
CCDD
"
gather <- plot_3oc_3cell + plot_3oc_5cell + plot_13oc_3cell + plot_47oc_3cell
map <- gather +
  plot_layout(nrow = 2,ncol=2,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure_posterior_density_2dataset.0302.png"), plot = map, width=14, height=12, dpi=300)


# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/3models_res_trends_df.Rdata")
# hist(betasp[20,]*100, breaks = 30, col = "lightgreen") #  xlim = c(0, 1),
tick=3 # sub_site[20]
model <- c('OCCCI')
bounds_df <- matrix(NA, nrow=1,ncol=2)
betasp_df <- matrix(NA, nrow=1,ncol=5000)
# dstc
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/dstc/dstc_42/3_res_dstc_df.Rdata")
betasp_df[1,] <- betasp[tick,]*100

plot_df <- data.frame(trend=NA,model=NA)
for(i in c(1)){
  bounds_df[i,] <- c(mean(betasp_df[i,])-2*sd(betasp_df[i,]),mean(betasp_df[i,])+2*sd(betasp_df[i,]))
  temp_betasp <- data.frame(trend=betasp_df[i,],model=model[i])
  plot_df <- rbind(plot_df,temp_betasp)
}
plot_df <- plot_df[-1,]
labeltemp <- rep(c("OCCCI","GlobColour"),each=5000)
plot_df$model <- factor(labeltemp, levels=c("OCCCI","GlobColour"))

# plot
# groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") # "OCCCI"="#0652DD","CMEMS"="#D980FA"
plot <- ggplot(plot_df, aes(trend,color=model)) + 
  geom_density(alpha = 0.2,linewidth=1.3) +
  coord_cartesian(expand=F) +# xlim = c(-0.04, 0.02),ylim = c(0, 80),
  scale_color_manual(values = c("OCCCI"="#a29bfe")) +
  geom_vline(xintercept = bounds_df[1,1],linewidth=1.3,
             color = "#a29bfe", linetype = "dashed") +
  geom_vline(xintercept = bounds_df[1,2],linewidth=1.3,
             color = "#a29bfe", linetype = "dashed") +
  labs(x=expression(paste("Trend  ", "(%·yr"^" -1",")")),y='Probability density') +
  theme_bw() +
  theme(legend.position = 'none',
        panel.background = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA,linewidth =0.5),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
plot
ggsave("posterior_density_occci_1363.png",width=6, height=4.3,dpi=300)


########### Model deviation ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
# # of time series on each class: 304*26
# dev_tab <- array(NA,dim=c(1,6)) # 7904 = 304*26
# colnames(dev_tab) <- c("index","time","obs","fitted","rmse","sd")
# dev_tab$Year <- rep(c(rep(1997,times=4),rep(c(1998:2022),each=12)),times=360*180)
# dev_tab$Month <- rep(c(c(9:12),rep(c(1:12),times=25)),times=360*180)
# dev_tab$Time <- rep(c(1:304),time=360*180)

for(i in c(3)){
  # dstc
  filename <- paste0(i,"_res_dstc_df.Rdata")
  load(filename)
  print(i)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  dev_tab <- model_input[,c(1:3,6:11)]
  
  # stc-o
  filename <- paste0(i,"_res_stc_df.Rdata")
  load(filename)
  print(i)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  temp_df <- model_input[,c(1:3,6:11)]
  dev_tab <- rbind(dev_tab,temp_df)
  
  # stc-l
  filename <- paste0(i,"_long_dstc_df.Rdata")
  load(filename)
  print(i)
  model_input <- model_input[,c(1:10)]
  model_input$fitted <- exp(fitted[,1])
  temp_df <- model_input[,c(1:3,6:11)]
  dev_tab <- rbind(dev_tab,temp_df)
  
}
dev_tab$Model <- rep(c('DSTC','STC-O','STC-L'),each=nrow(temp_df))
dev_tab$Dev <- dev_tab$chl - dev_tab$fitted
col <- c("goldenrod1","olivedrab3","lightslateblue")
# [1] 1359 1360 1427 1448 1449 1450 1451 1452 1453 1460
sub_dev_tab <- dev_tab[which(dev_tab$s.index==1448),]
p <- ggplot()+
  geom_line(data=sub_dev_tab,aes(x=time,y=Dev,group=Model,colour = Model),linewidth=1) +
  # scale_colour_manual(values = c("DSTC"="lightslateblue",
  #                              "STC-O"="goldenrod1","STC-L"="olivedrab3")) + # breaks = c('DSTC','STC-O','STC-L'),labels = c('DSTC','STC-O','STC-L')
  # geom_line(data=ts_df,aes(x=Date,y=Fitted,color="#C9C5C4"),position = "identity") +
  # geom_smooth(aes(group=index),method = "lm",formula = y ~ x,color="#C9C5C4",alpha=0.8,se=F)+
  labs(x=expression(paste("Time (yr)")),
       y=expression(paste("CHL  ","(mg·m"^"-3",")"))) +
  coord_cartesian(expand=F) + #ylim = c(-0.03,0.05),
  theme_bw() +
  theme(legend.position = "bottom",
        axis.title = element_text(face="bold",size=25,colour = "black"),
        axis.line.x = element_line(color="black", linewidth = 0.5),
        axis.line.y = element_line(color="black", linewidth = 0.5),
        axis.text = element_text(face="bold",size=18, color = "black"),
        strip.text = element_text(face="bold", size=18,lineheight=5.0),
        strip.background = element_rect(fill="#C9C5C4", colour="black",linewidth=1),
        axis.text.x=element_text(angle=60, hjust=1),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p

# ANOVA test
anova_res <- aov(Dev ~ Model, data = sub_dev_tab)
summary(anova_res)

t.test(dev1, dev2, paired = TRUE)




save(dev_tab,file="dstc_42oc_devTab.Rdata")
regiontemp <- array(NA,dim=c(304,6))
colnames(regiontemp) <- c("index","time","obs","fitted",'rmse',"sd")
for(j in c(1:304)){
  tempsite <- i
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
# ##### Table of each class (not use) #####
# n <- seq(1,81,3)
# table_sum <- array(NA,dim=c(81,7))
# colnames(table_sum) <- c('Class','Model',	'Trend Value',	'Lower CI',
#                          'Upper CI',	'NRMSE',	'Bias')
# for(j in c(1:27)){
#   table_dstc <- dstc_tr_df[which(dstc_tr_df$GSOC == j),]
#   table_stco <- stco_tr_df[which(stco_tr_df$GSOC == j),]
#   table_stcl <- stcl_tr_df[which(stcl_tr_df$GSOC  == j),]
#   i <- n[j]
#   print(i)
#   # class
#   table_sum[i,1] <- paste0("\\multirow{3}{*}{",j,"}")
#   table_sum[i+1,1] <- paste0('')
#   table_sum[i+2,1] <- paste0('')
#   # model
#   table_sum[i,2] <- 'DSTC'
#   table_sum[i+1,2] <- 'STC-O'
#   table_sum[i+2,2] <- 'STC-L'
#   # mean
#   table_sum[i,3] <- round(mean(table_dstc$Trend_Value,na.rm=T),digits = 3)
#   table_sum[i+1,3] <- round(mean(table_stco$Trend_Value,na.rm=T),digits = 3)
#   table_sum[i+2,3] <- round(mean(table_stcl$Trend_Value,na.rm=T),digits = 3)
#   # Lower CI
#   table_sum[i,4] <- round(mean(table_dstc$LCI,na.rm=T),digits = 3)
#   table_sum[i+1,4] <- round(mean(table_stco$LCI,na.rm=T),digits = 3)
#   table_sum[i+2,4] <- round(mean(table_stcl$LCI,na.rm=T),digits = 3)
#   # Upper CI
#   table_sum[i,5] <- round(mean(table_dstc$UCI,na.rm=T),digits = 3)
#   table_sum[i+1,5] <- round(mean(table_stco$UCI,na.rm=T),digits = 3)
#   table_sum[i+2,5] <- round(mean(table_stcl$UCI,na.rm=T),digits = 3)
#   # NRMSE
#   table_sum[i,6] <- round(mean(table_dstc$NRMSE,na.rm=T),digits = 3)
#   table_sum[i+1,6] <- round(mean(table_stco$NRMSE,na.rm=T),digits = 3)
#   table_sum[i+2,6] <- round(mean(table_stcl$NRMSE,na.rm=T),digits = 3)
#   # Bias
#   table_sum[i,7] <- round(mean(table_dstc$Bias,na.rm=T),digits = 3)
#   table_sum[i+1,7] <- round(mean(table_stco$Bias,na.rm=T),digits = 3)
#   table_sum[i+2,7] <- round(mean(table_stcl$Bias,na.rm=T),digits = 3)
#   
# }
# 
# write.csv(table_sum,
#           file="manuscript_table.csv")
