library(ggplot2)
library(ggtree)
library(treeio)
library(readxl)
library(lubridate)
library(cowplot)
library(raster)
library(sf)
library(rgeos)
library(dplyr)
library(rworldmap)
library(rnaturalearth)

##### Figure 1


#### Figure 1A - Epicurve + sequencing dates

metadata<-read_excel('Data/PatientSamplingMetadata.xlsx')
metadata$Age<-as.numeric(metadata$Age)

epi_data<-read_excel('Data/Senegal_epi_data_2023.xlsx')

epi_curve<-ggplot()+
  theme_bw()+
  geom_bar(data=epi_data, aes(as.Date(date, format="%d/%m/%Y"), National,fill='Other Regions of Senegal'),stat='identity', colour='black', size=0.2)+
  geom_bar(data=epi_data, aes(as.Date(date, format="%d/%m/%Y"), Kedougou_total,fill='Kédougou Region'),stat='identity', colour='black', size=0.2)+
  scale_fill_manual(values=c('pink3','dodgerblue4'))+
  theme(legend.position = c(0.3,0.8), legend.background = element_blank(), legend.title = element_blank())+
  scale_x_date(date_labels = "%b\n%Y",date_breaks = "2 month")+
  ylab('New Cases')+xlab('Date')+
  geom_rug(data=metadata,aes(x=as.Date(Sampling_date)), colour='pink4',size=0.4,alpha=0.5,length = unit(0.05, "npc"))
epi_curve

###Figure 1B - Demographics plot

demographics<-metadata %>% 
  dplyr::mutate(
    # Create categories
    age_group = dplyr::case_when(
      Age <= 4            ~ "0-4",
      Age > 4 & Age <= 14 ~ "5-14",
      Age > 14 & Age <= 29 ~ "15-29",
      Age > 29 & Age <= 44 ~ "30-44",
      Age > 44  ~ "> 45"
    ),
    # Convert to factor
    age_group = factor(
      age_group,
      level = c("0-4", "5-14","15-29", "30-44", "> 45")
    )
  )

demographics_count<-demographics %>% dplyr::count(Sex, age_group, sort = TRUE)


demographics_plot<-apyramid::age_pyramid(data = subset(demographics_count,age_group!='NA'),
                                         age_group = "age_group",# column name for age category
                                         split_by = "Sex",   # column name for gender
                                         count = "n")   +   # column name for case counts+
  theme_minimal()+
  theme(legend.position = c(0.9,0.7))+
  ylab('Count')+xlab('Age Groups')+
  scale_fill_manual(values=c('pink4','darkslategray'))

demographics_plot

#Merging 1A,B
plot_grid(epi_curve,demographics_plot, ncol=1, labels=c("A","B"))

## Supplementary Figure S2
coverage_ct_value<-ggplot(metadata,aes(-as.numeric(CtValue),as.numeric(Coverage)))+
  theme_bw()+
  geom_point(shape=21, size=4, fill='hotpink4', alpha=0.7)+
  geom_smooth(method='lm', colour='grey30', se=FALSE,size=0.5)+
  scale_x_continuous(labels=abs)+
  ylab("Genome Coverage")+xlab("CT Value")

coverage_ct_value


### Figure 1C

Senegal_level0_map <- raster::getData("GADM", country = "Senegal", level = 0) %>%
  st_as_sf() 

Senegal_level1_map <- raster::getData("GADM", country = "Senegal", level = 1) %>%
  st_as_sf() 

Kedougou_level2_map <- raster::getData("GADM", country = "Senegal", level = 2) %>%
  st_as_sf() %>%
  filter(NAME_1 == "Kédougou")

Kedougou_level3_map <- raster::getData("GADM", country = "Senegal", level = 3) %>%
  st_as_sf() %>%
  filter(NAME_1 == "Kédougou")

Kedougou_level4_map <- raster::getData("GADM", country = "Senegal", level = 4) %>%
  st_as_sf() %>%
  filter(NAME_1 == "Kédougou")

Tambacounda_level2_map <- raster::getData("GADM", country = "Senegal", level = 2) %>%
  st_as_sf() %>%
  filter(NAME_1 == "Tambacounda")



epi_map<-ggplot() +
  theme_void()+
  geom_sf(data = Senegal_level0_map, fill='white')+
  geom_sf(data = Senegal_level1_map, fill='white')+
  #geom_sf(data = Kedougou_level2_map, colour='red', fill='red3')+
  geom_sf(data = subset(Kedougou_level2_map, NAME_2=='Kédougou'), colour='grey50', aes(fill=66), size=2)+
  geom_sf(data = subset(Kedougou_level2_map, NAME_2=='Saraya'), colour='grey50', aes(fill=103), size=2)+
  geom_sf(data = subset(Kedougou_level2_map, NAME_2=='Salémata'), colour='grey50', aes(fill=6), size=2)+
  geom_sf(data = subset(Tambacounda_level2_map, NAME_2=='Bakel'), colour='grey50', aes(fill=2), size=2)+
  geom_sf(data = subset(Tambacounda_level2_map, NAME_2=='Goudiry'), colour='grey50', aes(fill=95), size=2)+
  geom_sf(data = subset(Tambacounda_level2_map, NAME_2=='Koupentoum'), colour='grey50', aes(fill=1), size=2)+
  geom_sf(data = subset(Tambacounda_level2_map, NAME_2=='Tambacounda'), colour='grey50', aes(fill=12), size=2)+
  geom_sf(data = subset(Senegal_level1_map, NAME_1=='Dakar'), colour='grey50', aes(fill=2), size=2)+
  geom_sf(data = subset(Senegal_level1_map, NAME_1=='Fatick'), colour='grey50', aes(fill=3), size=2)+
  geom_sf(data = subset(Senegal_level1_map, NAME_1=='Louga'), colour='grey50', aes(fill=1), size=2)+
  geom_sf(data = subset(Senegal_level1_map, NAME_1=='Thiès'), colour='grey50', aes(fill=1), size=2)+
  geom_sf(data = subset(Senegal_level1_map, NAME_1=='Matam'), colour='grey50', aes(fill=1), size=2)+
  geom_sf(data = subset(Senegal_level1_map, NAME_1=='Ziguinchor'), colour='grey50', aes(fill=1), size=2)+
  scale_fill_distiller(palette='YlGnBu', direction=1, name='CHIKV\nCases')+
  theme(legend.position = c(0.9,0.9))+
  scale_size_area(name='No. Samples',max_size = 6,breaks=c(20,35))+
  #geom_sf(data = Kedougou_level2_map, colour='red', fill=NA, size=10)+
  #geom_sf(data = Kedougou_level4_map, colour='grey70', fill=NA, size=2)+
  geom_sf(data = Senegal_level1_map, colour='black',lwd=0.5, fill=NA)+
  geom_sf(data = Senegal_level0_map, colour='black',lwd=0.5, fill=NA)+
  geom_sf(data = subset(Senegal_level1_map, NAME_1=='Kédougou'), colour='red', lwd=1,fill=NA)+
  geom_count(data=subset(metadata, Sites=='Bandafassi'),mapping=aes(x=-12.3,y=12.54), shape=21, fill=NA, colour='white', show.legend=FALSE)+
  geom_text(aes(x=-12.4,y=12.8),label="19", colour='white', size=4)+
  geom_text(aes(x=-11.9,y=12.6),label="37", colour='white', size=4)+
  geom_count(data=subset(metadata, Sites=='Ndiormi'),mapping=aes(x=-12.18,y=12.5), shape=21, fill=NA, colour='white', show.legend=FALSE)+
  coord_sf()
epi_map




## Figure 1F

map1 <- ne_countries(type = "countries", country = "Senegal",
                     scale = "medium", returnclass = "sf")
map2 <- rnaturalearth::ne_states(country = "Senegal",
                                 returnclass = "sf")

Senegal_forestloss<-raster("Data/Senegal_forest_loss.tiff")
Senegal_forestloss_all<-Senegal_forestloss

Senegal_forestloss_2020to2023_reclassified<-calc( Senegal_forestloss_all , function(x) { x[ x == 1 ] <- 0; return(x) } )
Senegal_forestloss_2020to2023_reclassified<-calc( Senegal_forestloss_2020to2023_reclassified , function(x) { x[ x >= 20 ] <- 1; return(x) } )
Senegal_forestloss_2020to2023_reclassified<-calc( Senegal_forestloss_2020to2023_reclassified , function(x) { x[ x > 1 ] <- 0; return(x) } )

Senegal_forestloss_2020to2023_reclassified_aggregated2<-terra::aggregate(Senegal_forestloss_2020to2023_reclassified,100)
plot(Senegal_forestloss_2020to2023_reclassified_aggregated2)


writeRaster(Senegal_forestloss_2020to2023_reclassified_aggregated2, "Data/Senegal_forest_loss_medium_res_2020to2023.asc")


cols1=colorRampPalette(brewer.pal(9,"Greens"))(130)[16:115]
cols2=colorRampPalette(brewer.pal(9,"Reds"))(130)[20:130]


Senegal_forestloss_2020to2023_reclassified_aggregated2_v2<-calc( Senegal_forestloss_2020to2023_reclassified_aggregated2 , function(x) { x[ x < 0.005 ] <- NA; return(x) } )

mixed_trees<-raster("Data/global_mixed_trees.tif")
Senegal_mixed_trees<-crop(mixed_trees,map1)
Senegal_mixed_trees<-raster::mask(Senegal_mixed_trees,map1)


plot(Senegal_mixed_trees,col=cols1,axes=F,frame.plot=F,legend=F)
plot(Senegal_mixed_trees, legend.only=T, add=T, col=cols1, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.55,0.75,0.2,0.21),
     legend.args=list(text="Forest Cover (2014)", cex=0.6, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.5, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
plot(Senegal_forestloss_2020to2023_reclassified_aggregated2_v2,col=cols2,axes=F,frame.plot=F,legend=F,add=T)
plot(Senegal_forestloss_2020to2023_reclassified_aggregated2_v2, legend.only=T, add=T, col=cols2, legend.width=0.5, legend.shrink=0.3, smallplot=c(0.25,0.45,0.2,0.21),
     legend.args=list(text="Forest Cover Loss (2020-2023)", cex=0.6, line=0.3, col="gray30"), horizontal=T,
     axis.args=list(cex.axis=0.5, lwd=0, lwd.tick=0.2, tck=-0.5, col.axis="gray30", line=0, mgp=c(0,-0.02,0)))
plot(map2$geometry,add=T)




##### tree figure

### Figure 2A was plotted in Figtree

##### Figure 2B - root to tip

residuals<-read_excel('Data/2009_2023_ML_tree_tempest.xlsx')
residuals$days<-as.Date(date_decimal(as.numeric(residuals$date)))

root_tip<-ggplot(residuals, aes(days, as.numeric(distance)))+
  theme_classic()+
  geom_smooth(method='lm',colour='grey60')+
  geom_point(shape=21,size=3,alpha=0.5,aes(fill=days>as.Date("2023-01-01")))+
  scale_fill_manual(values=c('dodgerblue4','darkseagreen4'))+
  ylab('Root-to-tip Divergence')+xlab('Time')+
  annotate(geom = 'text', label='Correlation coefficient = 0.95',x=as.Date("2015-01-30"),y=0.05)+
  annotate(geom = 'text',label='R squared = 0.90', x=as.Date("2015-01-30"),y=0.04)+
  theme(legend.position='none')


root_tip

#### Figure 2C - Population density

pop_den<-read_excel('Data/skygrid_population.xlsx')

p_skygrid<-ggplot()+
  theme_bw()+
  #geom_ribbon(data=pop_den,aes(x=as.Date(date),ymin=as.numeric(lower),ymax=as.numeric(upper)), fill='pink4',alpha=0.3)+
  geom_line(data=pop_den,aes(as.Date(date),as.numeric(median)), colour='pink4', size=1)+
  scale_y_continuous(trans='log10')+
  ylab('Effective Population Size')+
  theme(axis.title.x = element_blank())+
  scale_x_date(limits=as.Date(c("2008-01-01","2024-01-01")))

p_skygrid


##Figure 2D
beast_tree<-read.beast("Data/CHIKV_West_africa_Senegal_2009_2023_aligned_minusoutliers_cleaned_V2_WITHOUT2005_editted_at_read_ends_V2.TMRCA.POPSIZE.relaxed.skygrid.500ml.Combined_MCC.tree")

p_tree <- ggtree(beast_tree, mrsd="2023-08-25", color='pink4',size=1) + theme_tree2()


p <- p_tree  + 
  scale_colour_manual(values=c("salmon1", "gold2", "palegreen3",'cadetblue2','mediumpurple2','plum2'))+

  theme(axis.text=element_text(size=15))
p


### age density

age_density_IRESSEF<-read_excel('Data/age_2023_density.xlsx')
age_density_IRESSEF$date2<-as.Date(date_decimal(as.numeric(age_density_IRESSEF$date)))
age_density_IRESSEF$days<-as.Date(cut(age_density_IRESSEF$date2,breaks = "day",start.on.monday = FALSE))

tmrca<-ggplot()+
  theme_classic()+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  geom_area(data=age_density_IRESSEF,aes(x=date2,y=as.numeric(density)), fill='darkseagreen4',alpha=0.5)
 
tmrca

age_density_2009<-read_excel('Data/age_2009_density.xlsx')
age_density_2009$date2<-as.Date(date_decimal(as.numeric(age_density_2009$date)))
age_density_2009$days<-as.Date(cut(age_density_2009$date2,breaks = "day",start.on.monday = FALSE))

tmrca<-ggplot()+
  theme_classic()+
  #geom_area(data=age_densityroot,aes(x=date,y=density), fill='deeppink2',alpha=0.5)+
  theme(plot.background = element_blank())+
  theme(panel.background = element_blank())+
  geom_area(data=age_density_2009,aes(x=date2,y=as.numeric(density)), fill='dodgerblue4',alpha=0.5)+
  scale_x_date(limits=c(as.Date("2005/01/01"),as.Date("2010/01/01")))
tmrca



