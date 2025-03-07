### July.09.2024
# 1. Reduce the resolution of chlorophyll from 1 degree grid cell 
#   to 5 degree grid cell.
# 2. Reduce the resolution of optical class from 1 degree grid cell 
#   to 5 degree grid cell.
# 3. Fixed optical class reduce resolution
# 360/5=72
# 180/5=36

library(raster)
library(sp)
library(raster)
library(oceanmap)
library(R.matlab)

rm(list=ls())
setwd("~/Coding/Dynamic_SPT/reduce_res")
####################### Part I. Chlorophyll #######################
load("~/Coding/Dynamic_SPT/occci_chl_v6_new.Rdata")
dim.chl <- dim(chl)
chl_res <- array(NA,dim=c(72,36,304))
for(i in c(1:dim.chl[3])){
  ## Convert matrix to a raster with geographical coordinates
  r <- raster(chl[,,i]) 
  extent(r) <- extent(c(-180, 180, -90, 90))
  ## Create a raster with the desired dimensions, and resample into it
  s <- raster(nrow=72, ncol=36)
  s <- resample(r,s)
  ## Convert resampled raster back to a matrix
  chl_res[,,i] <- as.matrix(s)
}
lon <- seq(-179.5,179.5,5)
lat <- seq(-89.5,89.5,5)
save(chl_res,lon,lat,file="occci_chl_5res.Rdata")

####################### Part II. Optical class spt varying #######################
# load("~/Coding/Dynamic_SPT/dynamic/oc_rm_coastal.Rdata")
# rm("monoc") # monoc is after removing coastal region
# dim.oc <- dim(oc_glts)
# oc_res <- array(NA,dim=c(72,36,304))
# for(i in c(1:dim.oc[3])){
#   ## Convert matrix to a raster with geographical coordinates
#   r <- raster(oc_glts[,,i]) 
#   extent(r) <- extent(c(-180, 180, -90, 90))
#   ## Create a raster with the desired dimensions, and resample into it
#   s <- raster(nrow=72, ncol=36)
#   s <- resample(r,s)
#   ## Convert resampled raster back to a matrix
#   oc_res[,,i] <- as.matrix(s)
# }
# lon <- seq(-179.5,179.5,5)
# lat <- seq(-89.5,89.5,5)

####################### Part III. Fixed optical class #######################
# ##### Import gsoc data dataframe and transform to matrix
# load("~/Coding/Dynamic_SPT/dynamic/gsoc30_df.Rdata")
# oc_result <- oc_result[,-c(3:4)]
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
# 
# ## Convert matrix to a raster with geographical coordinates
# r <- raster(oc_mat) 
# extent(r) <- extent(c(-180, 180, -90, 90))
# ## Create a raster with the desired dimensions, and resample into it
# s <- raster(nrow=72, ncol=36)
# s <- resample(r,s)
# ## Convert resampled raster back to a matrix
# oc_fix_res <- as.matrix(s)
# 
# save(oc_res,oc_fix_res,lon,lat,file="oc_varying_5res.Rdata")

####################### Part IV. Longhurst #######################
temp <- readMat("~/Coding/Dynamic_SPT/map_data/Longhurst_180.mat") #used to identify longhursst regions
Longhurst <- temp$Longhurst
## reduce resolution
# Function to downsample the matrix
downsample_matrix <- function(mat, factor) {
  new_rows <- nrow(mat) / factor
  new_cols <- ncol(mat) / factor
  
  if (new_rows %% 1 != 0 || new_cols %% 1 != 0) {
    stop("Dimensions must be divisible by factor")
  }
  
  new_mat <- matrix(NA, nrow = new_rows, ncol = new_cols)
  
  for (i in seq(1, nrow(mat), by = factor)) {
    for (j in seq(1, ncol(mat), by = factor)) {
      block <- mat[i:(i+factor-1), j:(j+factor-1)]
      # Remove NAs from the block
      block <- block[!is.na(block)]
      if (length(block) > 0) {
        # Use mode (most frequent category) for downsampling
        new_mat[(i-1)/factor+1, (j-1)/factor+1] <- as.numeric(names(sort(table(block), decreasing = TRUE))[1])
      } else {
        # If block is empty after removing NAs, keep it as NA
        new_mat[(i-1)/factor+1, (j-1)/factor+1] <- NA
      }
    }
  }
  return(new_mat)
}

Longhurst_res <- downsample_matrix(Longhurst, factor = 5)
Longhurst <- Longhurst_res
area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
# area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean
lon <- seq(-179.5,179.5,5)
lat <- seq(-89.5,89.5,5)
nlon <- length(lon)
nlat <- length(lat)
for(i in 1:nlon){
  for(j in 1:nlat){
    if(Longhurst[i,j] %in% area){
      Longhurst[i,j] <- Longhurst[i,j]
    }else{
      Longhurst[i,j] <- NA
    }
  }
}

# correct labels
# trueid <- c(23,8,3,7,2,1,10,12,14,18,5,6,4,15,11,13,21,20,19,16,17,22,9)
# region <- c(3,5,9,14,15,16,20,21,23,24,28,32,33,37,38,39,44,45,46,48,49,51,52)
# long <- array(NA,dim=c(72,36))
# for(j in c(1:23)){
#   ind <- region[j]
#   long[which(Longhurst==ind)] <- trueid[j]
# }
Longhurst_res <- Longhurst
save(Longhurst,lon,lat,file="res_Longhurst_54.Rdata")

# ### Corresponding Longhurst area
# temp <- readMat("~/Coding/Dynamic_SPT/map_data/Longhurst_180.mat") #used to identify longhursst regions
# Longhurst <- temp$Longhurst
# ## reduce resolution
# r <- raster(Longhurst) 
# extent(r) <- extent(c(-180, 180, -90, 90))
# s <- raster(nrow=72, ncol=36)
# s <- resample(r,s)
# Longhurst_res <- as.matrix(s)
# Longhurst <- Longhurst_res
# area <- sort(unique(Longhurst[!is.na(Longhurst)]))#list of longhurst areas to iterate through
# area <- area[-c(1,2,4,6,7,8,10,11,12,13,17,18,19,22,25,26,27,29,30,31,34,35,36,40,43,41,42,47,50,53,54)] #remove coastal, polar, GoM, Archipelagic Deep basin, Mediterranean
# 
# nlon <- length(lon)
# nlat <- length(lat)
# for(i in 1:nlon){
#   for(j in 1:nlat){
#     if(Longhurst[i,j] %in% area){
#       Longhurst[i,j] <- Longhurst[i,j]
#     }else{
#       Longhurst[i,j] <- NA
#     }
#   }
# }

