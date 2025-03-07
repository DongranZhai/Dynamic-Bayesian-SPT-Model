library(rgdal)
library(ggplot2)
library(mapproj)
library(munsell)
library(oceanmap)
library(dplyr)
library(scales)
library(patchwork) # organize ggplot figures
library(R.matlab)

########## Part I. Map trend under OC DSTC model ##########
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

### Gather together (trend estimates) with GlobColour data product
layout <- "
AAA
BBB
"
gather <- p_dstc + p_globcolour
map <- gather +
  plot_layout(nrow = 2,design=layout) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(face = 'bold',size=30))
map
ggsave(filename = paste0("figure_dstc_maps.png"), plot = map, width=14, height=12, dpi=300)

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
  geom_point(data =comb_df, aes(x = Longitude,y = Latitude,col=Sign,shape=Sign,size=Sign),show.legend = F) +
  scale_size_manual(values=c(miss=1.2,opposite=2,same=1.2)) +
  scale_shape_manual(values=c(miss=4,opposite=17,same=1)) +
  scale_color_manual(values = c(same = alpha("white", 0),opposite = alpha("#353b48", 0.8),
                                miss = alpha("#353b48", 0.5))) + # 
  coord_equal() +
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
ggsave(filename = paste0("figure_dstc_products_robust.png"), plot = map, width=14, height=12, dpi=300)

