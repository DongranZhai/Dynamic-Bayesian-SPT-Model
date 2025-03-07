# Note: Plotting dynamic model
# Update: July.12.2024: change plotting way
library(rgdal)
library(ggplot2)
library(mapproj)
library(munsell)
library(oceanmap)
library(dplyr)
library(scales)
library(patchwork) # organize ggplot figures
library(R.matlab)

# temp <- readMat("~/Coding/DA/Longhurst_180.mat") # used to identify longhursst regions
# Longhurst <- temp$Longhurst
# nlon <- 360
# nlat <- 180
# area <- sort(unique(Longhurst[!is.na(Longhurst)])) # list of longhurst areas to iterate through
# area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean
# 
# lon <- seq(-180,180,length.out=nlon)
# lat <- seq(-90,90,length.out=nlat)
# 
# for(i in 1:nlon){
#   for(j in 1:nlat){
#     if(Longhurst[i,j] %in% area){
#       Longhurst[i,j] <- Longhurst[i,j]
#     }else{
#       Longhurst[i,j] <- NA
#     }
#   }
# }
# ind.na <- which(is.na(Longhurst))

########## Part I. Time trend under OC DSTC model ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_dstc_spT_tpoc_42resultTab.Rdata")
results_tab <- as.data.frame(results_tab)

#### coordinate
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
# lon and lat have already been centred
comb_df <- full_join(temp,results_tab,by='Site')

## Stipple grid cells that exclude zero
comb_df$stipple <- NA
ind <- which(is.na(comb_df$Trend_Value))
comb_df$stipple[ind] <- "yes" # NA
ind <- which(!is.na(comb_df$Trend_Value))
comb_df$stipple[ind] <- "no" # NA

range(results_tab$Trend_Value,na.rm=T)
mean(results_tab$Trend_Value,na.rm=T)
sum(is.na(results_tab$Trend_Value))

range(results_tab$Obs,na.rm=T)
range(results_tab$Fitted,na.rm=T)
# range(exp(results_tab$Fitted),na.rm=T)

# set NA
ind.na <- which(comb_df$Latitude > 60 | comb_df$Latitude < -60)
comb_df$Trend_Value[ind.na] <- NA

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p_dstc <- ggplot() +
  geom_tile( data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-4,4),oob=squish) +
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=stipple),
             shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre 
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")),
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
p_dstc
ggsave("oc_dstc_spT_mon_42tpoc.png",width=8.27, height=3.44, dpi=300)

########## Part II. Time trend under OC STC model ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_stc_spT_ocfix42resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
# results_tab$Trend_Value <- results_tab$Trend_Value*12
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")

#### coordinate
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
# lon and lat have already been centred
comb_df <- full_join(temp,results_tab,by='Site')

## Stipple grid cells that exclude zero
comb_df$stipple <- NA
ind <- which(is.na(comb_df$Trend_Value))
comb_df$stipple[ind] <- "yes" # NA
ind <- which(!is.na(comb_df$Trend_Value))
comb_df$stipple[ind] <- "no" # NA

range(results_tab$Trend_Value,na.rm=T)
mean(results_tab$Trend_Value,na.rm=T)
sum(is.na(results_tab$Trend_Value))

range(results_tab$Obs,na.rm=T)
range(results_tab$Fitted,na.rm=T)
# range(exp(results_tab$Fitted),na.rm=T)

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p_stco <- ggplot() +
  geom_tile( data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-4,4),oob=squish) +
  coord_equal() +
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=stipple),
             shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")),
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
p_stco
ggsave("oc_stc_spT_mon_42.png",width=8.27, height=3.44, dpi=300)

########## Part III. Time trend under Longhurst STC model ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_stc_spT_42resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")

#### coordinate
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
# lon and lat have already been centred
comb_df <- full_join(temp,results_tab,by='Site')

## Stipple grid cells that exclude zero
comb_df$stipple <- NA
ind <- which(is.na(comb_df$Trend_Value))
comb_df$stipple[ind] <- "yes" # NA
ind <- which(!is.na(comb_df$Trend_Value))
comb_df$stipple[ind] <- "no" # NA

range(results_tab$Trend_Value,na.rm=T)
mean(results_tab$Trend_Value,na.rm=T)
sum(is.na(results_tab$Trend_Value))

range(results_tab$Obs,na.rm=T)
range(results_tab$Fitted,na.rm=T)
# range(exp(results_tab$Fitted),na.rm=T)

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p_stcl <- ggplot() +
  geom_tile( data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-4,4),oob=squish) +
  coord_equal() +
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=stipple),
             shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")),
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
p_stcl
ggsave("long_stc_spT_mon_42.png",width=8.27, height=3.44, dpi=300)

### Gather together (trend estimates)
# 1
layout <- "
AAA
BBB
CCC
"
gather <- p_dstc + p_stco + p_stcl
map <- gather +
  plot_layout(nrow = 3,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure5_map.png"), plot = map, width=14, height=16, dpi=300)

# 2
layout <- "
AAA
BBB
"
gather <- p_stco + p_stcl
map <- gather +
  plot_layout(nrow = 2,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure_si_stc_map.0301.png"), plot = map, width=14, height=12, dpi=300)


########## Part IV. OC coefficient under Longhurst DSTC model (not use) ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_dstc_spoc_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
# results_tab$Trend_Value <- results_tab$Trend_Value*12
#### coordinate
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
# lon and lat have already been centred
comb_df <- merge(results_tab,temp,by='Site')

range(results_tab$Trend_Value,na.rm=T)
mean(results_tab$Trend_Value,na.rm=T)
sum(is.na(results_tab$Trend_Value))

range(results_tab$Obs,na.rm=T)
range(results_tab$Fitted,na.rm=T)
# range(exp(results_tab$Fitted),na.rm=T)

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile( data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-5,5),oob=squish) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Coefficient of optical class")),
                               title.position = "left",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p
ggsave("long_dstc_spoc.png",width=8.27, height=3.44, dpi=300)

########## Part V. Time trend under Longhurst DSTC model (not use) ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
##### Import gsoc data dataframe and transform to matrix
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/gsoc24_df_res.Rdata")
oc_result <- oc_result[,-c(3:4)]
# create spatial points data frame
coordinates(oc_result) <- ~ lon + lat
# coerce to SpatialPixelsDataFrame
gridded(oc_result) <- T
# coerce to raster
temp <- raster(oc_result)
oc_mat <- raster2matrix(temp)
rm(list=c("temp","oc_result"))
# check if import is right
table(oc_mat)
oc <- oc_mat
rm("oc_mat")

class <- 1:24
time <- 1:304
n_c <- length(class)

##### Import chl initial data
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/occci_chl_5res.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_dstc_spoc_resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
trend <- results_tab[,2]

### Option 1: determine if 0 is contained in 95% credibility intervals
# for(i in class){
#   #print(i)
#   if((results_tab[i,7]>0 & results_tab[i,8]>0) | (results_tab[i,7]<0 & results_tab[i,8]<0)){
#     trend[i] <- trend[i]
#   }else{
#     trend[i] <- 0
#   }
# }

for(i in class){
  #print(i)
  if((results_tab[i,5]>0 & results_tab[i,6]>0) | (results_tab[i,5]<0 & results_tab[i,6]<0)){
    trend[i] <- trend[i]
  }else{
    trend[i] <- 0
  }
}

oc_df <- matrix2raster(oc,x=lon,y=lat,layer=1)
oc_df <- as.data.frame(oc_df,xy=T)
oc_df$trend <- NA
colnames(oc_df) <- c("longitude","latitude","class","trend")

for(i in class){
  oc_df$trend[which(oc_df$class==i)] <- trend[i]
}

range(results_tab$Trend_Value,na.rm=T)
mean(results_tab$Trend_Value,na.rm=T)
sum(is.na(results_tab$Trend_Value))

range(results_tab$Obs,na.rm=T)
range(results_tab$Fitted,na.rm=T)
# range(exp(results_tab$Fitted),na.rm=T)

### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]#remove caspian sea
# mapp_df <- fortify(map("world2",plot=FALSE,fill=TRUE))

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p <- ggplot() +
  geom_tile( data = oc_df , aes(x = longitude,y = latitude,fill = trend)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-5,5),oob=squish) +
  coord_equal() +
  # geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  # geom_polygon(data=map("world", wrap=c(0,360))) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
  # scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_colorbar(title=expression(paste("Trend  ", "(%yr"^" -1",")")),
                               title.position = "top",
                               barwidth = unit(.4, "cm"),barheight = unit(3.5, "cm"),title.vjust =1)) +
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = 'right',legend.direction = "vertical",
        legend.title = element_text(size = 10,angle = 90),
        plot.margin = margin(t = .3, r = .3, b = .3, l = .3, unit = "cm"))
p
ggsave("long_dstc_tr_yr_10percent.png",width=8.27, height=3.44, dpi=300)

