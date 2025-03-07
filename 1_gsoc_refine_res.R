###################################### Script Notation #####################################
########## Part I: This script is mainly for remove the cloud, coastal and unprevalent regions:
#                  1) abandon 13 and 14 (coastal region) optical class;
#                  2) trim the latitude between -70 ~ 70, focus on the tropical and subtropical region,
#                  because the data at polar and subpolar are shadowed by the cloud
#                  3) keep the same NA regions with Longhurst
########## Part II: Visualization global geographicl separated optical class
#                    Simplify the global ocean and sea table,
#                    merging that just remain the five basins
############################################################################################
library(ggplot2)
library(oceanmap)
library(rgdal)
library(mapproj)
library(munsell)
library(scales)
library(R.matlab)
library(dplyr)
library(Polychrome) # gsoc pallete

rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
######################### diagnose plot oc map overlap outline 
### max class oc_glts
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

##### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data/", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]

##### oc outline and filled
# load("~/Coding/Dynamic_SPT/ind_max_og_filter.Rdata")
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_dynamic_fix_5res.Rdata")
otl <- oc_fix
nlon <- length(lon)
nlat <- length(lat)

otl_raster <- matrix2raster(otl, x = lon, y = lat, layer = 1)
otl_df <- as.data.frame(otl_raster,xy = T)
colnames(otl_df) <-c('longitude','latitude','outline')
rm(list=c('otl_raster'))

# ##### create oc defination map plot
# optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
#                      '7'='#0fb9b1','8'='#badc58','9'='#32ff7e','10'='#FFCCBC','11'='#FFEB3B','12'='#FFA000')
# optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
#                      '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#ffbe76','11'='#FFA000','12'='#eb4d4b')
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#FFA000')
label <- c('1','2','3','4','5','6','7','8','9','10') # ,'11','12'

plot <- ggplot() +
  geom_raster(data=otl_df,aes(x=longitude,y=latitude,group=outline,fill=factor(outline))) +
  scale_fill_manual(values = optical_palette , na.value = 'black',limits=label) +
  # stat_contour(data=otl_df,aes(x=longitude,y=latitude,z=outline),
  #              geom="contour",colour="black",size=0.25) +
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
ggsave("synna_10oc_res.0301.png",width=8.2, height=5.6,dpi=300)


##################################### Part III #############################################
############# Visualization global geographicl separated optical class
##### Notes: Simplify the global ocean and sea table,
#####        merging that just remain the five basins
library(dplyr)
library(rgdal) # real world map
library(ggplot2)
library(oceanmap)
library(raster)
library(sp)
#library(rgeos)
#library(sf)
library(secr)
library(viridis) # color pallete
library(R.matlab)

rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")

##### check which polygon(region) the picked pixel is in
ocean_tab <- read.csv(file='~/Coding/Dynamic_SPT/ocean_basin/ocean_basin_gsoc.csv',
                      header = T,fill=T,fileEncoding = "UTF-8")
# optical class data frame
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_dynamic_fix_5res.Rdata")
nlon <- length(lon)
nlat <- length(lat)
oc_df <- data.frame(lon=NA,lat=NA,oc=NA,month=NA)
for(i in c(1:304)){
  oc <- oc_glts[,,i]
  oc_raster <- matrix2raster(oc, x = lon, y = lat, layer = 1)
  oc_temp <- as.data.frame(oc_raster,xy = T)
  colnames(oc_temp) <-c('lon','lat','oc')
  oc_temp$month <- i
  oc_df <- rbind(oc_df,oc_temp)
}
oc_df <- oc_df[-1,]
rm(list=c('oc_raster'))
oc_df$Regions <- NA

basins_labels <- c("North Pacific Ocean","North Pacific Ocean","South Pacific Ocean",
                   "South Pacific Ocean","North Atlantic Ocean","South Atlantic Ocean",
                   "Indian Ocean","Arctic Ocean","Southern Ocean","Seas","Seas","ACC")

for(i in c(1:12)){
  print(i)
  tempoc <- oc_df[1:2]
  ocean_tab_temp <- ocean_tab%>%
    filter(Group==i)%>%
    dplyr::select(Longitude,Latitude)
  result <- pointsInPolygon(tempoc, ocean_tab_temp, logical = T)
  oc_df$Regions[result] <- basins_labels[i]
}

temp <- oc_df$oc[which(oc_df$Regions=="North Pacific Ocean")]
table(temp) # all 14 class
temp <- oc_df$oc[which(oc_df$Regions=="South Pacific Ocean")]
table(temp) # all 14 class
temp <- oc_df$oc[which(oc_df$Regions=="North Atlantic Ocean")]
table(temp) # no class 1
temp <- oc_df$oc[which(oc_df$Regions=="South Atlantic Ocean")]
table(temp) # all 14 class
temp <- oc_df$oc[which(oc_df$Regions=="Indian Ocean")]
table(temp) # all 14 class
temp <- oc_df$oc[which(oc_df$Regions=="Southern Ocean")]
table(temp) # no class 2
temp <- oc_df$oc[which(oc_df$Regions=="ACC")]
table(temp)# no class 1, 2
temp <- oc_df$oc[which(oc_df$Regions=="Arctic Ocean")]
table(temp) # no class 2, 3, 6
temp <- oc_df$oc[which(oc_df$Regions=="Seas")]
table(temp) # no class 1, 2

# remove class 11 - 14, and trim -70~70
coast_ind <- which(oc_df$oc ==11 | oc_df$oc ==12 | oc_df$oc ==13 | oc_df$oc ==14)
oc_df$oc[coast_ind] <- NA
# lat_ind <- which(oc_df$lat >=70 | oc_df$lat <= -70)
# otl[,lat_ind] <- NA
##### define the optical classes into basins
class <- 1:10
basins_label <- c("North Pacific Ocean","South Pacific Ocean",
                  "North Atlantic Ocean","South Atlantic Ocean",
                  "Indian Ocean","ACC","Southern Ocean","Arctic Ocean","Seas")

oc_result <- as.data.frame(array(data=NA, dim=c(1,6)))
colnames(oc_result) <- c("lon","lat","oc","month","Regions","gsoc")
for(i in 1:length(basins_label)){
  tempoc <- oc_df
  # pick one basin
  print(basins_label[i])
  temp <- tempoc %>%
    filter(Regions==basins_label[i])
  table <- table(temp$oc)
  print(table)
  # reorder optical class for labeling
  temp <- temp[with(temp, order(oc)),]
  
  # gsoc labeling
  temp$gsoc <- as.numeric(as.factor(rank(temp$oc,na.last = "keep")))
  for (j in 1:length(temp$gsoc)){
    if(temp$Regions[j]=="North Pacific Ocean"){
      temp$gsoc[j] <- temp$gsoc[j]
    }else if(temp$Regions[j]=="South Pacific Ocean"){
      temp$gsoc[j] <- temp$gsoc[j] + 10
    }else if(temp$Regions[j]=="North Atlantic Ocean"){
      temp$gsoc[j] <- temp$gsoc[j] + 20
    }else if(temp$Regions[j]=="South Atlantic Ocean"){
      temp$gsoc[j] <- temp$gsoc[j] + 29
    }else if(temp$Regions[j]=="Indian Ocean"){
      temp$gsoc[j] <- temp$gsoc[j] + 39
    }else if(temp$Regions[j]=="ACC"){
      temp$gsoc[j] <- temp$gsoc[j] + 49
    }else if(temp$Regions[j]=="Southern Ocean"){
      temp$gsoc[j] <- temp$gsoc[j] + 57
    }else if(temp$Regions[j]=="Arctic Ocean"){
      temp$gsoc[j] <- NA
    }else if(temp$Regions[j]=="Seas"){
      temp$gsoc[j] <- NA
    }
  }
  # corresponding to original optical class df
  oc_result <- rbind(oc_result,temp)
  
}
oc_result <- oc_result[-1,]

oc_result <- oc_df %>%
  left_join(oc_result,by =c("lon","lat","oc","month","Regions"))

filename <- paste0("gsoc66_df_res.Rdata")
save(oc_result,file =filename)

# region_temp <- oc_result[which(oc_result$Regions=="North Pacific Ocean"),]
# table(region_temp$oc)
# table(region_temp$gsoc)

### Dynamic
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/gsoc66_df_res.Rdata")
oc_mat <- array(NA, dim=c(72,36,304))
for(i in c(1:304)){ # 72*36
  print(i)
  temp_df <- oc_result[c((72*36*(i-1)+1):(72*36*i)),-c(3:5)]
  # create spatial points data frame
  coordinates(temp_df) <- ~ lon + lat
  # coerce to SpatialPixelsDataFrame
  gridded(temp_df) <- T
  # coerce to raster
  temp_r <- raster(temp_df)
  temp_mat <- raster2matrix(temp_r)
  # check if import is right
  # table(temp_mat)
  oc_mat[,,i] <- temp_mat
}
filename <- paste0("gsoc66_df_mat_res.Rdata")
save(oc_result,oc_mat,file =filename)

######################################### GS optical class ###########################
##### plot the geographical separated optical class
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/gsoc66_df_res.Rdata")
oc_result$gsoc <- as.factor(oc_result$gsoc)
oc_result[which(oc_result$Regions=="Southern Ocean"),] <- NA
oc_result$gsoc[which(oc_result$gsoc==50)] <- 51
for(i in c(51:57)){
  oc_result$gsoc[which(oc_result$gsoc==i)] <- i-1
}
oc_result <- na.omit(oc_result)
##### land
wmap<-readOGR(dsn="~/Coding/Dynamic_SPT/map_data/", layer="ne_110m_land")
wmap_df <- fortify(wmap)
wmap_df <-wmap_df[-(which(wmap_df$group==112.2)),]

# load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/oc_dynamic_fix_5res.Rdata")
# otl <- oc_fix
# nlon <- length(lon)
# nlat <- length(lat)
# otl_raster <- matrix2raster(otl, x = lon, y = lat, layer = 1)
# otl_df <- as.data.frame(otl_raster,xy = T)
# colnames(otl_df) <-c('longitude','latitude','outline')
# rm(list=c('otl_raster'))


# optical_palette <- c('1'='#9C27B0','2'='#3742fa','3'='#448AFF','4'='#00BCD4','5'='#B2EBF2',
#                      '6'='#badc58','7'='#32ff7e','8'='#FFEB3B','9'='#FFA000','10'='#512DA8',
#                      '11'='#3742fa','12'='#448AFF','13'='#00BCD4','14'='#B2EBF2','15'='#0fb9b1',
#                      '16'='#badc58','17'='#32ff7e','18'='#FFEB3B','19'='#FFA000','20'='#512DA8',
#                      '21'='#3742fa','22'='#448AFF','23'='#00BCD4','24'='#B2EBF2','25'='#badc58',
#                      '26'='#32ff7e','27'='#FFEB3B','28'='#FFA000','29'='#00BCD4','30'='#badc58',
#                      '31'='#FFCCBC','32'='#FFEB3B','33'='#FFppp0o9A000','34'='#448AFF','35'='#0fb9b1',
#                      '36'='#badc58','37'='#32ff7e','38'='#FFEB3B','39'='#FFA000')
turbo_pal <- viridis::viridis(n = 66,option = "H")

plot <- ggplot() +
  geom_raster(data=oc_result,aes(x=lon,y=lat,group=Regions,fill=gsoc)) +
  scale_fill_manual(values = turbo_pal,na.value = 'black') + # 
  # scale_fill_viridis(option = "H",discrete = TRUE) +
  # stat_contour(data=otl_df,aes(x=longitude,y=latitude,z=outline),
  #              geom="contour",colour="black",size=0.25) +
  coord_equal() + 
  # geom_polygon(colour="black",size=0.25) +
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
        legend.text=element_text(size=16,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
plot
ggsave("gsoc56.png",width=8.5, height=6, dpi=300) # 4.3

######################
# 27 class
# table(oc_df$oc[which(oc_df$Regions == "Arctic Ocean")])
# # 8:3
# table(oc_df$oc[which(oc_df$Regions == "Indian Ocean")])
# # 3  4  5  6  8  9 
# # 56  3 40 16 96 11 
# table(oc_df$oc[which(oc_df$Regions == "ACC")])
# # 4   8   9 
# # 26 147  38 
# table(oc_df$oc[which(oc_df$Regions == "Southern Ocean")])
# # 8 
# # 43 

#### merge small sample class i.e. 7->6, 10->11
tick <- which(oc_df$Regions == "North Pacific Ocean" & oc_df$oc == 2)
oc_df$oc[tick] <- 3
tick <- which(oc_df$Regions == "South Pacific Ocean" & oc_df$oc == 9)
oc_df$Regions[tick] <- "North Pacific Ocean"
tick <- which(oc_df$Regions == "North Atlantic Ocean" & oc_df$oc == 2)
oc_df$oc[tick] <- 3
tick <- which(oc_df$Regions == "North Atlantic Ocean" & oc_df$oc == 4)
oc_df$oc[tick] <- 3
tick <- which(oc_df$Regions == "North Atlantic Ocean" & oc_df$oc == 9)
oc_df$oc[tick] <- 8
tick <- which(oc_df$Regions == "South Atlantic Ocean" & oc_df$oc == 4)
oc_df$oc[tick] <- 3
tick <- which(oc_df$Regions == "South Atlantic Ocean" & oc_df$oc == 6)
oc_df$Regions[tick] <- "North Atlantic Ocean"
tick <- which(oc_df$Regions == "South Atlantic Ocean" & oc_df$oc == 9)
oc_df$oc[tick] <- 8
tick <- which(oc_df$Regions == "Indian Ocean" & oc_df$oc == 4)
oc_df$oc[tick] <- 3
tick <- which(oc_df$Regions == "Arctic Ocean" & oc_df$oc == 8)
oc_df$Regions[tick] <- 'North Atlantic Ocean'


# tick <- which(oc_df$Regions == "ACC" & oc_df$oc == 4 & oc_df$lon < 0)
# oc_df$Regions[tick] <- 'South Pacific Ocean'
# tick <- which(oc_df$Regions == "ACC" & oc_df$oc == 4 & oc_df$lon > 0)
# oc_df$Regions[tick] <- 'Indian Ocean'
# tick <- which(oc_df$Regions == "ACC" & oc_df$oc == 9)
# oc_df$Regions[tick] <- 'Indian Ocean'
# tick <- which(oc_df$Regions == "ACC" & oc_df$oc == 8 & oc_df$lon < -60 & oc_df$lon > 150)
# oc_df$Regions[tick] <- 'South Pacific Ocean'
# tick <- which(oc_df$Regions == "ACC" & oc_df$oc == 8 & oc_df$lon > -60 & oc_df$lon < 30)
# oc_df$Regions[tick] <- 'South Atlantic Ocean'
# tick <- which(oc_df$Regions == "ACC" & oc_df$oc == 8 & oc_df$lon > 30 & oc_df$lon < 150)
# oc_df$Regions[tick] <- 'Indian Ocean'
# tick <- which(oc_df$Regions == "Southern Ocean" & oc_df$lon < -60 & oc_df$lon > 150)
# oc_df$Regions[tick] <- 'South Pacific Ocean'
# tick <- which(oc_df$Regions == "Southern Ocean" & oc_df$oc == 8 & oc_df$lon > -60 & oc_df$lon < 30)
# oc_df$Regions[tick] <- 'South Atlantic Ocean'
# tick <- which(oc_df$Regions == "Southern Ocean" & oc_df$oc == 8 & oc_df$lon > 30 & oc_df$lon < 150)
# oc_df$Regions[tick] <- 'Indian Ocean'