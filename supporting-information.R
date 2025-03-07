###### Notes: This script is about
######       analyzing and plotting the figures in the Surpporing Information.
library(ggplot2)
library(oceanmap)
library(rgdal)
library(mapproj)
library(munsell)
library(scales)
library(R.matlab)
library(dplyr)
library(Polychrome) # gsoc pallete
library(viridis) # pallete
library(ggpubr) # for creating easily publication ready plots
library(patchwork) # organize ggplot figures
library(ggpmisc)
library(gghalves)
library(RColorBrewer)

############ I. Dominant optical class without geographical separation ############
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
### revolution reduce: from 1 to 5 grid cells
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_rm_coastal_res.Rdata")
# create function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
oc_fix <- array(NA,dim=c(72,36))
for(i in 1:72){
  for(j in 1:36){
    temp <- oc_glts[i,j,]
    ind <- getmode(temp)
    oc_fix[i,j] <- ind
  }
}
lat <- seq(-89.5,89.5,by = 5)
lon <- seq(-179.5,179.5,by = 5)
savename <- paste0("oc_dynamic_fix_5res.Rdata")
save(oc_glts,monoc,oc_fix,lon,lat,file=savename)

##### land map
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data/", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]
#### optical class
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_dynamic_fix_5res.Rdata")
otl <- oc_fix
nlon <- length(lon)
nlat <- length(lat)

otl_raster <- matrix2raster(otl, x = lon, y = lat, layer = 1)
otl_df <- as.data.frame(otl_raster,xy = T)
colnames(otl_df) <-c('longitude','latitude','outline')
rm(list=c('otl_raster'))

optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#FFA000')
label <- c('1','2','3','4','5','6','7','8','9','10') # ,'11','12'

plot <- ggplot() +
  geom_raster(data=otl_df,aes(x=longitude,y=latitude,group=outline,fill=factor(outline))) +
  scale_fill_manual(values = optical_palette , na.value = 'black',limits=label) +
  coord_equal() + 
  geom_polygon(colour="black",size=0.25) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  guides(fill = guide_legend(title = 'class number',title.position = "bottom",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend') +
  coord_cartesian(xlim = c(-175, 175),ylim = c(-75, 75), expand=F) + # 
  scale_x_continuous(breaks = seq(-180,180,60)) +
  scale_y_continuous(breaks = seq(-60,60,30)) +
  labs(title = "", x = "Longitude", y = "Latitude") + # Globally Dominant Optical optical Class
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
ggsave("synna_10oc_res.png",width=8.2, height=5.6,dpi=300)

### II. Pick specific grid cells, TS of CHL with colored background of number of classes ####
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
### CHL de-seasonality
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_nna_df.Rdata")
## Setting date format
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

### example 1: site 579
## chl decompose ts
chl_df <- global_no_na[which(global_no_na$s.index==579),]
chl <- chl_df$chl
chl_decompose <- stl(ts(chl,start = c(1997,9),end = c(2022,12),frequency = 12),s.window="periodic")
chl_df$chl <- as.numeric(chl_decompose$time.series[,2])
range(chl_df$chl)
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
# ggsave("site_577_ts_oc.png",width=8.2, height=4.3,dpi=300)

## select other three grid cell and repeat the same process, then combine the plots
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
ggsave(filename = paste0("figure_4ts+counted_oc.png"), plot = map_5, width=16, height=20, dpi=300)

############ III. Longhurst regions ############
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
ggsave("long23_outline.png",width=8.27, height=4.6, dpi=300)

############ IV. STCs map plotting ############
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
# data import
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_res_stc_spT_ocfix42resultTab.Rdata")
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

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p_stco <- ggplot() +
  geom_tile( data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-4,4),oob=squish) +
  coord_equal() +
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=stipple),
             shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
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

load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/long_res_stc_spT_42resultTab.Rdata")
results_tab <- as.data.frame(results_tab)
colnames(results_tab) <- c("Site","Trend_Value","Obs","Fitted","LCI","UCI","lci","uci","RMSE","NRMSE","Bias")

#### coordinate
ind <- global_df$s.index
temp <- global_df[which(global_df$s.index==ind),c(1:3)]
temp <- temp[!duplicated(temp$s.index),]
colnames(temp) <- c('Site','Longitude','Latitude')
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
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]

### pallete
colorPalette <- rev(c("firebrick4","firebrick2","white","dodgerblue2","dodgerblue4"))#colour palette

p_stcl <- ggplot() +
  geom_tile( data = comb_df , aes(x = Longitude,y = Latitude,fill = Trend_Value)) +
  scale_fill_gradientn(colors = colorPalette,na.value = "white",limits=c(-4,4),oob=squish) +
  coord_equal() +
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=stipple),
             shape=4,size=1,show.legend = F) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
  ylab("Latitude") +
  xlab("Longitude") +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  scale_x_continuous(breaks = seq(-180,180,60)) +
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

## combine stco and stcl
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
ggsave(filename = paste0("figure_si_stc_map.png"), plot = map, width=14, height=12, dpi=300)

############ V. NRMSE and Bias of STCs models ############
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/models_trend_df.Rdata")

results_df <- rbind(dstc_tab,stco_tab,stcl_tab)
labeltemp <- c(rep(c("DSTC"),each=nrow(dstc_tab)),
               rep("STC-O",each=nrow(stco_tab)),
               rep("STC-L",each=nrow(stcl_tab)))
results_df$Model <- factor(labeltemp , levels=c("DSTC","STC-O","STC-L"))

## NRMSE: STC-O
stco_tab$stipple <- NA
ind <- which(is.na(stco_tab$NRMSE))
stco_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stco_tab$NRMSE))
stco_tab$stipple[ind] <- "no" # NA
range(stco_tab$NRMSE,na.rm=T)

wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]
### pallete
col_pallette <- c("white","#badc58","#6ab04c") # orange,green
p_NRMSE_stco_map <- ggplot() +
  geom_tile( data = stco_tab , aes(x = Longitude,y = Latitude,fill = NRMSE)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",breaks=seq(0,1,0.5),
                       limits=c(0,1),oob=squish) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
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
# ggsave("stco_rmse_map.png",width=8.27, height=3.44, dpi=300)

## NRMSE: STC-L
stcl_tab$stipple <- NA
ind <- which(is.na(stcl_tab$NRMSE))
stcl_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stcl_tab$NRMSE))
stcl_tab$stipple[ind] <- "no" # NA
range(stcl_tab$NRMSE,na.rm=T)

wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]
### pallete
col_pallette <- c("white","#badc58","#6ab04c") # orange,green
p_NRMSE_stcl_map <- ggplot() +
  geom_tile( data = stcl_tab , aes(x = Longitude,y = Latitude,fill = NRMSE)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",breaks=seq(0,1,0.5),
                       limits=c(0,1),oob=squish) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
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
# ggsave("stcl_rmse_map.png",width=8.27, height=3.44, dpi=300)

## Bias: STC-O
stco_tab$stipple <- NA
ind <- which(is.na(stco_tab$Bias))
stco_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stco_tab$Bias))
stco_tab$stipple[ind] <- "no" # NA

col_pallette <- c("#f0932b","#ffbe76","white","#badc58","#6ab04c") # orange,green
stco_bias_map <- ggplot() +
  geom_tile( data = stco_tab , aes(x = Longitude,y = Latitude,fill = Bias)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",limits=c(-0.5,0.5),oob=squish) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
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
# ggsave("stco_bias_map.png",width=8.27, height=3.44, dpi=300)

## Bias: STC-L
stcl_tab$stipple <- NA
ind <- which(is.na(stcl_tab$Bias))
stcl_tab$stipple[ind] <- "yes" # NA
ind <- which(!is.na(stcl_tab$Bias))
stcl_tab$stipple[ind] <- "no" # NA

col_pallette <- c("#f0932b","#ffbe76","white","#badc58","#6ab04c") # orange,green
stcl_bias_map <- ggplot() +
  geom_tile( data = stcl_tab , aes(x = Longitude,y = Latitude,fill = Bias)) +
  scale_fill_gradientn(colors = col_pallette,na.value = "white",limits=c(-0.5,0.5),oob=squish) +
  scale_color_manual(values = c(no = alpha("white", 0),yes = alpha("#353b48", 0.5))) +
  coord_equal() +
  geom_polygon( data=wmap_df, aes(x=long, y=lat, group = group),colour="black", fill="gainsboro",size=0.25 ) +
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
# ggsave("stcl_bias_map.png",width=8.27, height=3.44, dpi=300)

# gather 4 plots
layout <- "
AABB
CCDD
"

gather_si <- p_NRMSE_stco_map + stco_bias_map + p_NRMSE_stcl_map + stcl_bias_map
map_si <- gather_si +
  plot_layout(nrow = 2,ncol=2, design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map_si
ggsave(filename = paste0("figure_si_stc_nrmse_bias.png"), plot = map_si, width=14, height=10, dpi=300)

############ VI. Standard deviation of STCs models ############
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

##### VII. Probability distribution #####
tick=3 # sub_site[20]
Dataset <- c('OCCCI','GlobColour')
bounds_df <- matrix(NA, nrow=2,ncol=2)
betasp_df <- matrix(NA, nrow=2,ncol=5000)
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/dstc/dstc_42/13_res_dstc_df.Rdata")
betasp_df[1,] <- betasp[tick,]*100
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_cmems/dstc_42/13_res_dstc_df_cmems.Rdata")
betasp_df[2,] <- betasp[tick,]*100

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


##### VIII. 95% Credibility Interval #####
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/models_trend_df.Rdata")
results_df <- rbind(dstc_tab,stco_tab,stcl_tab)
labeltemp <- c(rep(c("DSTC"),each=nrow(dstc_tab)),
               rep("STC-O",each=nrow(stco_tab)),
               rep("STC-L",each=nrow(stcl_tab)))
results_df$Model <- factor(labeltemp , levels=c("DSTC","STC-O","STC-L"))

groupcolors <- c("lightslateblue","goldenrod1","olivedrab3") # pallette

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

##### VIIII. Model deviations #####
dev_tab <- array(NA,dim=c(1,6)) # 7904 = 304*26
colnames(dev_tab) <- c("index","time","obs","fitted","rmse","sd")
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
dstc_tab <- as.data.frame(dev_tab)
dstc_tab$dev <- dstc_tab$obs - dstc_tab$fitted
table(dstc_tab$index)
table(dstc_tab$time)


