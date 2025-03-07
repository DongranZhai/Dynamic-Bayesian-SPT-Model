###################################### Script Notation #####################################
#                    Visualization global geographicl separated optical class
#                    Simplify the global ocean and sea table,
#                    merging that just remain the five basins
############################################################################################
library(dplyr)
library(rgdal) # real world map
library(ggplot2)
library(oceanmap)
library(raster)
library(sp)
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

###### GS optical class #####
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
        legend.text=element_text(size=16,face="bold"),
        legend.title=element_text(size=18,face="bold"),
        axis.text=element_text(size=16,face="bold"),
        axis.title=element_text(size=18,face="bold"),
        panel.border = element_rect(colour = "black", fill=NA,linewidth=0.6),
        plot.margin = margin(t = .3, r = .5, b = .3, l = .3, unit = "cm"))
plot
ggsave("gsoc56.png",width=8.5, height=6, dpi=300) # 4.3

#### merge small sample class i.e. 7->6, 10->11
# tick <- which(oc_df$Regions == "North Pacific Ocean" & oc_df$oc == 2)
# oc_df$oc[tick] <- 3
# tick <- which(oc_df$Regions == "South Pacific Ocean" & oc_df$oc == 9)
# oc_df$Regions[tick] <- "North Pacific Ocean"
# tick <- which(oc_df$Regions == "North Atlantic Ocean" & oc_df$oc == 2)
# oc_df$oc[tick] <- 3
# tick <- which(oc_df$Regions == "North Atlantic Ocean" & oc_df$oc == 4)
# oc_df$oc[tick] <- 3
# tick <- which(oc_df$Regions == "North Atlantic Ocean" & oc_df$oc == 9)
# oc_df$oc[tick] <- 8
# tick <- which(oc_df$Regions == "South Atlantic Ocean" & oc_df$oc == 4)
# oc_df$oc[tick] <- 3
# tick <- which(oc_df$Regions == "South Atlantic Ocean" & oc_df$oc == 6)
# oc_df$Regions[tick] <- "North Atlantic Ocean"
# tick <- which(oc_df$Regions == "South Atlantic Ocean" & oc_df$oc == 9)
# oc_df$oc[tick] <- 8
# tick <- which(oc_df$Regions == "Indian Ocean" & oc_df$oc == 4)
# oc_df$oc[tick] <- 3
# tick <- which(oc_df$Regions == "Arctic Ocean" & oc_df$oc == 8)
# oc_df$Regions[tick] <- 'North Atlantic Ocean'

