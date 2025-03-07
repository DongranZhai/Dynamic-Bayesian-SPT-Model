library(sp)
library(raster)
library(oceanmap)
library(dplyr)
library(zoo)

########## Part I. Create data frame ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
### CHL
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/occci_chl_5res.Rdata")
chl <- chl_res

##### OCFIX + OC
load("~/Coding/Dynamic_SPT/reduce_res/gsoc66_df_mat_res.Rdata")
# Mode of each grid cell
get_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
oc_fix_mat <- matrix(NA, 72,36)
for(i in c(1:72)){
  for(j in c(1:36)){
    temp_ts <- oc_mat[i,j,]
    if(sum(!is.na(temp_ts))==0){
      oc_fix_mat[i,j] <- NA
    }else{
      oc_fix_mat[i,j] <- get_mode(temp_ts)
    }
  }  
}

oc_fix <- array(NA,dim=c(72,36,304))
for(i in c(1:304)){
  oc_fix[,,i] <- oc_fix_mat
}
#######################
# chl, oc, ocfix
chl.dim <- dim(chl)
time <- 1:304
coordtemp <- expand.grid(time,lon,lat)
chltemp <- as.vector(aperm(chl,c(3,1,2)))
octemp <- as.vector(aperm(oc_mat,c(3,1,2)))
ocfixtemp <- as.vector(aperm(oc_fix,c(3,1,2)))

# Longhurst
load("/Users/doris_zhai/Coding/Dynamic_SPT/reduce_res/res_Longhurst_54.Rdata")
longtemp <- array(data=NA, dim = c(72,36,chl.dim[3]))
for(i in 1:chl.dim[3]){
  print(i)
  longtemp[,,i] <- Longhurst
}
longtemp <- as.vector(aperm(longtemp,c(3,1,2)))
sitetemp <- rep(1:(chl.dim[1]*chl.dim[2]),each=chl.dim[3])
YY <- as.vector(rep(c(rep(1997,time=4),rep(c(1998:2022),time=12)),times=(chl.dim[1]*chl.dim[2])))
MM <- as.factor(rep(c(9:12,rep(1:12,times=25)),times=(chl.dim[1]*chl.dim[2])))
TT <- rep(time,times=(chl.dim[1]*chl.dim[2]))

global_df <- data.frame(s.index=sitetemp,longitude=coordtemp[[2]],latitude=coordtemp[[3]],
                        year=YY,month=MM,time=TT,chl=chltemp,oc=octemp,ocfix=ocfixtemp,long=longtemp)
# na.ind <- which(!is.na(global_df$long))
# global_no_na <- global_df[na.ind,]
## Interpolation of missing value
sptmodel_df <- global_df
s=1
for(i in c(1:2592)){
  # print(i)
  temp <- global_df[(304*(i-1)+1):(304*i),]
  tempchl <- temp$chl
  if(sum(is.na(tempchl)) <= 100 & sum(is.na(tempchl)) != 0){
    # interpolation
    interp.ind <- which(sptmodel_df$s.index==i)
    sptmodel_df$chl[interp.ind] <- na.approx(sptmodel_df$chl[interp.ind])
    print(s)
    s <- s+1
  }else if(sum(is.na(tempchl)) == 0){
  
  }else if(sum(is.na(tempchl)) > 100){
    # na.ind <- global_df$s.index[which(is.na(global_df$chl))]
    # na.ind <- na.ind[!duplicated(na.ind)]
    sptmodel_df <- 
      sptmodel_df %>%  filter(!s.index %in% i)
  }
}
global_no_na <- sptmodel_df
save(global_df,global_no_na,file="global_chl_oc_res_66_df.Rdata")

  
########## Part II. Partition SPTMODEL dataframe on Optical class ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res/task_partition_42")
load("~/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_66_df.Rdata")

# create function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
### Missing value
sptmodel_df <- global_no_na
na.ind.ocfix <- sptmodel_df$s.index[which(is.na(sptmodel_df$ocfix))]
na.ind.ocfix <- na.ind.ocfix[!duplicated(na.ind.ocfix)]
sptmodel_df <-
  sptmodel_df %>%  filter(!s.index %in% na.ind.ocfix)
# na.ind.oc <- sptmodel_df$s.index[which(is.na(sptmodel_df$oc))]
na.ind.oc <- which(is.na(sptmodel_df$oc))
# sptmodel_df$oc[na.ind.oc[j]]: optical class value
# na.ind.oc[j]: row in data frame
# j: index of site
for(j in 1:length(na.ind.oc)){
  print(j)
  sptmodel_df$oc[na.ind.oc[j]] <- getmode(sptmodel_df$oc[j:(j+9)])
}

task_class <- sptmodel_df$ocfix[!duplicated(sptmodel_df$ocfix)]
for(i in task_class){
  print(paste0("fixed oc: ",i))
  sptmodel_df.sub <- sptmodel_df[which(sptmodel_df$ocfix == i),]
  print(nrow(sptmodel_df.sub)/304)

  savename <- paste0(i,"_dynamic_res_input.Rdata")
  save(sptmodel_df.sub,file = savename)
}

########## Part II. Partition SPTMODEL dataframe on Longhurst ##########
rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res/long_partition")
load("~/Coding/Dynamic_SPT/reduce_res/global_chl_oc_res_nna_df.Rdata")
### remove rows containing NA
# Got: global_df,global_no_na
sptmodel_df <- global_df
#keep na with longhurst
na.ind <- sptmodel_df$s.index[which(is.na(sptmodel_df$long))]
na.ind <- na.ind[!duplicated(na.ind)]
sptmodel_df <-
  sptmodel_df %>%  filter(!s.index %in% na.ind)

na.ind.1 <- sptmodel_df$s.index[which(is.na(sptmodel_df$chl))]
na.ind.1 <- na.ind.1[!duplicated(na.ind.1)]
sptmodel_df <- 
  sptmodel_df %>%  filter(!s.index %in% na.ind.1)
na.ind.2 <- sptmodel_df$s.index[which(is.na(sptmodel_df$ocfix))]
na.ind.2 <- na.ind.2[!duplicated(na.ind.2)]
sptmodel_df <-
  sptmodel_df %>%  filter(!s.index %in% na.ind.2)
na.ind.3 <- sptmodel_df$s.index[which(is.na(sptmodel_df$oc))]
na.ind.3 <- na.ind.3[!duplicated(na.ind.3)]
sptmodel_df <-
  sptmodel_df %>%  filter(!s.index %in% na.ind.3)
sum(is.na(sptmodel_df))
rm(list=c("global_df",'global_no_na'))

for(i in 1:length(table(sptmodel_df$long))){
  print(paste0("Longhurst: ",i))
  
  sptmodel_df.sub <- sptmodel_df[which(sptmodel_df$long == i),]
  print(nrow(sptmodel_df.sub)/304)
  
  savename <- paste0(i,"_long_res_input.Rdata")
  save(sptmodel_df.sub,file = savename)
}

for(i in 1:length(table(sptmodel_df$ocfix))){
  print(paste0("fixed oc: ",i))
  
  sptmodel_df.sub <- sptmodel_df[which(sptmodel_df$ocfix == i),]
  print(nrow(sptmodel_df.sub)/304)
  
  savename <- paste0(i,"_dynamic_res_input.Rdata")
  save(sptmodel_df.sub,file = savename)
}

########## Part IV. Check plotting ##########
### Longhurst
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#f78fb3','11'='#ffbe76','12'='#FFA000',
                     '13'='#eb4d4b','14'='#d63031')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12','13','14')
lat <- seq(-89.5,89.5,by = 1) # npsg
lon <- seq(-179.5,179.5,by = 1) # npsg
# lat <- seq(-89.5,89.5,by = 1) # centre
# lon <- seq(0.5,359.5,by = 1) # centre

oc_raster  <- Longhurst
oc_raster <- matrix2raster(oc_raster, x = lon, y = lat, layer = 1)
oc_df <- as.data.frame(oc_raster ,xy = T)
colnames(oc_df) <-c('lon','lat','values')

p <- ggplot() +
  geom_raster( data = oc_df , aes(x = lon,y = lat,fill = values)) +
  scale_fill_gradientn(colours = optical_palette , na.value = 'black') +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  # scale_x_continuous(breaks = seq(-180,180,60)) + # npsg
  scale_x_continuous(breaks = seq(-180,180,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_legend(title = 'class number',title.position = "left",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend' ) +
  labs(title = paste0('Long '),title.hjust=.5,
       x = "Longitude", y = "Latitude") +
  theme(legend.position = 'bottom')

savename <- paste0('long_check.png')
ggsave(savename,width=8.27, height=3.44, dpi=300)

### optical class (example: second layer)
optical_palette <- c('1'='#9C27B0','2'='#512DA8','3'='#3742fa','4'='#448AFF','5'='#00BCD4','6'='#B2EBF2',
                     '7'='#32ff7e','8'='#badc58','9'='#FFEB3B','10'='#f78fb3','11'='#ffbe76','12'='#FFA000',
                     '13'='#eb4d4b','14'='#d63031')
label <- c('1','2','3','4','5','6','7','8','9','10','11','12','13','14')
lat <- seq(-89.5,89.5,by = 1) # npsg
lon <- seq(-179.5,179.5,by = 1) # npsg
# lat <- seq(-89.5,89.5,by = 1) # centre
# lon <- seq(0.5,359.5,by = 1) # centre

oc_raster  <- oc[,,2] # oc_npsg
oc_raster <- matrix2raster(oc_raster, x = lon, y = lat, layer = 1)
oc_df <- as.data.frame(oc_raster ,xy = T)
colnames(oc_df) <-c('lon','lat','values')

class_number <- 2
p <- ggplot() +
  geom_raster( data = oc_df , aes(x = lon,y = lat,fill = values)) +
  scale_fill_gradientn(colours = optical_palette , na.value = 'black') +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  # scale_x_continuous(breaks = seq(-180,180,60)) + # npsg
  scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_legend(title = 'class number',title.position = "left",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend' ) +
  labs(title = paste0('Class ', class_number),title.hjust=.5,
       x = "Longitude", y = "Latitude") +
  theme(legend.position = 'bottom')

savename <- paste0('oc',class_number,'.png') # npsg
ggsave(savename,width=8.27, height=3.44, dpi=300)

### chlorophyll (example: second layer)
library(palr)
pal <- chl_pal(palette = TRUE)
lat <- seq(-89.5,89.5,by = 1) # npsg
lon <- seq(-179.5,179.5,by = 1) # npsg
# lat <- seq(-89.5,89.5,by = 1) # centre
# lon <- seq(0.5,359.5,by = 1) # centre

oc_raster  <- chl[,,2] # chl_npsg
oc_raster <- matrix2raster(oc_raster, x = lon, y = lat, layer = 1)
oc_df <- as.data.frame(oc_raster ,xy = T)
colnames(oc_df) <-c('lon','lat','values')

class_number <- 2
p <- ggplot() +
  geom_raster( data = oc_df , aes(x = lon,y = lat,fill = values)) +
  scale_fill_gradientn(colours = optical_palette , na.value = 'black') +
  coord_cartesian(ylim = c(-75, 75),expand=F) +
  # scale_x_continuous(breaks = seq(-180,180,60)) + # npsg
  scale_x_continuous(breaks = seq(0,360,60)) + # centre
  scale_y_continuous(breaks = seq(-60,60,30)) +
  guides(fill = guide_legend(title = 'class number',title.position = "left",title.hjust = .5, show.limits = T,
                             label.position = 'bottom', direction = 'horizontal',nrow = 1, byrow = T,
                             barwidth = unit(10, "cm"),barheight = unit(.5, "cm")),color = 'legend' ) +
  labs(title = paste0('Chl ', class_number),title.hjust=.5,
       x = "Longitude", y = "Latitude") +
  theme(legend.position = 'bottom')

savename <- paste0('chl_',class_number,'.png') # npsg
ggsave(savename,width=8.27, height=3.44, dpi=300)
