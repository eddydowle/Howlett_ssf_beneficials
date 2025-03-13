#making pie charts for brad

#need to make them resized based on abundance
#need to be editable for brad (SVG etc)
library(tidyverse)
library(lme4)
library(readxl)
library(svglite)

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024")

apple<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Apple')
pear<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Pear')
avocado<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Avocado')
kiwifruit<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Kiwifruit')
pakchoi<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Pak choi')
radish<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Radish')
onion<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Onion')
carrot<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Carrot')

#apple contains apple and pear
unique(apple$Crop)
unique(pear$Crop)
unique(avocado$Crop)
unique(kiwifruit$Crop)
unique(pakchoi$Crop)
unique(radish$Crop)
unique(onion$Crop)
unique(carrot$Crop)

#check region
unique(apple$Region)
unique(pear$Region)
unique(avocado$Region)
unique(kiwifruit$Region)
unique(pakchoi$Region)
unique(radish$Region)
unique(onion$Region)
unique(carrot$Region)

#check location and season
unique(apple$`Location and season`)
unique(pear$`Location and season`)
unique(avocado$`Location and season`)
unique(kiwifruit$`Location and season`)
unique(pakchoi$`Location and season`)
unique(radish$`Location and season`)
unique(onion$`Location and season`)
unique(carrot$`Location and season`)

#brad wants them grouped on the column labeled 'Ordering'
unique(apple$Ordering)
unique(pear$Ordering)
unique(avocado$Ordering)
unique(kiwifruit$Ordering)
unique(pakchoi$Ordering)
unique(radish$Ordering)
unique(onion$Ordering)
unique(carrot$Ordering)

#this column is also the one for the ordering on around pie chart based on the lettering
levels(as.factor(apple$Ordering))
#will default to correct ordering

#remove “Ephydridae spp.” from Pak choi and onion data
#as per brad is wasnt counted in the other crops so needs to be removed
onion<-onion %>% filter(`Scientific name`!='Ephydridae spp.')
pakchoi<-pakchoi %>% filter(`Scientific name`!='Ephydridae spp.')


#looks good 
unique(apple$Region)
#[1] "Hawkes Bay" "Motueka" 
mydf<-apple %>% filter(Region=='Hawkes Bay')
head(mydf)
totals<-mydf %>% select(`Location and season`,`Corrected Daily Count`) %>% group_by(`Location and season`) %>% summarise(total=sum(`Corrected Daily Count`))

mydf<-left_join(mydf,totals,by='Location and season')



#genus<-c("Apis","Bombus" ,"Hylaeus","Lasioglossum", "Leioproctus" , "Megachile")
#col_genus<-c('gold','gold3','firebrick4','firebrick1','firebrick','mediumorchid4')

#brads_col<-data.frame(genus,col_genus) %>% arrange(genus)

brads_col_2<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Insect_ordering')
head(brads_col_2)
apple 

#just bees vs flies to start with
#for this just turning everything into a percentage
test<-mydf%>% select(`Location and season`,`Corrected Daily Count`,Grouping) %>% group_by(`Location and season`,Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()

    test %>%   ggplot(aes(x = 1, y = per, fill = Grouping)) +
  geom_bar(stat = "identity" ,width=1) +
  facet_wrap(~ `Location and season` , strip.position = "bottom") +
  coord_polar("y", start = 0, direction = -1) +
  theme_bw(base_size = 12) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size = 6), 
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 4))+
  scale_fill_manual(values=c('gold','dodgerblue'))+
 #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
  ggtitle(paste('Apple','Hawkes Bay'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))

  mydf %>% filter(Grouping=='Non_Bees')%>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ `Location and season` , strip.position = "bottom") +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste('Apple','Hawkes Bay'))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  
  
############################
  #actual figures#
  
  
#BEES VS FLIES
#decided on a barchart of bees vs flies to sit next to the pies

  #graph with the bees vs flies
  
  #just bees vs flies to start with
  #for this just turning everything into a percentage
  #then brad wants a little bar he can jiggle in next to his barchart
unique(apple$Region)
unique(pear$Region)
unique(avocado$Region)
unique(kiwifruit$Region)
unique(pakchoi$Region)
unique(radish$Region)
unique(onion$Region)
unique(carrot$Region)
  
test<-avocado%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()

  
  test %>%   ggplot(aes(x = `Location and season`, y = per, width=0.2, fill = Grouping)) +
    geom_bar(position='stack',stat='identity') +
    facet_grid(~Region, scales = "free", space = "free") +
    theme_bw(base_size = 12) +
    theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
    scale_fill_manual(values=c('lightgrey','black'))+
    ggtitle(paste('Avocado'))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))  
  
#maybe all as one on one figure?
  apple_per<-apple%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  apple_per$crop<-'Apple'
  
  pear_per<-pear%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pear_per$crop<-'Pear'
  
  avocado_per<-avocado%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  avocado_per$crop<-'Avocado'
  
  kiwifruit_per<-kiwifruit%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  kiwifruit_per$crop<-'Kiwifruit'
  
  pakchoi_per<-pakchoi%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  pakchoi_per$crop<-'Pakchoi'
  
  radish_per<-radish%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  radish_per$crop<-'Radish'
  
  onion_per<-onion%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  onion_per$crop<-'Onion'
  
  carrot_per<-carrot%>% select(`Location and season`,`Corrected Daily Count`,Grouping,Region) %>% group_by(`Location and season`,Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(`Location and season`,Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
  carrot_per$crop<-'Carrot'
allcrops_per<-rbind(apple_per,pear_per,avocado_per,kiwifruit_per,pakchoi_per,radish_per,onion_per,carrot_per)  

unique(allcrops_per$Grouping)

allcrops_per %>%   ggplot(aes(x = `Location and season`, y = per, fill = Grouping)) +
  geom_bar(position='stack',stat='identity',width=0.4) +
  facet_grid(crop~Region, scales = "free", space = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=c('lightgrey','black'))+
  ggtitle(paste('Bees vs Flies'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))  


#doing an overview figure do it per crop by region and across crop
#by region
apple_per_r<-apple%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
apple_per_r$crop<-'Apple'

pear_per_r<-pear%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
pear_per_r$crop<-'Pear'

avocado_per_r<-avocado%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
avocado_per_r$crop<-'Avocado'

kiwifruit_per_r<-kiwifruit%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
kiwifruit_per_r$crop<-'Kiwifruit'

pakchoi_per_r<-pakchoi%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
pakchoi_per_r$crop<-'Pakchoi'

radish_per_r<-radish%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
radish_per_r$crop<-'Radish'

onion_per_r<-onion%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
onion_per_r$crop<-'Onion'

carrot_per_r<-carrot%>% select(`Corrected Daily Count`,Grouping,Region) %>% group_by(Grouping,Region) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% group_by(Region) %>% mutate(per=100*total_count/sum(total_count)) %>% ungroup()
carrot_per_r$crop<-'Carrot'
allcrops_per_r<-rbind(apple_per_r,pear_per_r,avocado_per_r,kiwifruit_per_r,pakchoi_per_r,radish_per_r,onion_per_r,carrot_per_r)  



allcrops_per_r %>%   ggplot(aes(x = Region, y = per, fill = Grouping)) +
  geom_bar(position='stack',stat='identity',width=0.9) +
  facet_grid(.~crop, scales = "free", space = "free") +
  #facet_wrap(~crop, scales = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=c('lightgrey','black'))+
  ggtitle(paste('Bees vs Flies'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line")) 

#across all crops
apple_per_tot<-apple%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
apple_per_tot$crop<-'Apple'

pear_per_tot<-pear%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
pear_per_tot$crop<-'Pear'

avocado_per_tot<-avocado%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
avocado_per_tot$crop<-'Avocado'

kiwifruit_per_tot<-kiwifruit%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
kiwifruit_per_tot$crop<-'Kiwifruit'

pakchoi_per_tot<-pakchoi%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
pakchoi_per_tot$crop<-'Pakchoi'

radish_per_tot<-radish%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
radish_per_tot$crop<-'Radish'

onion_per_tot<-onion%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
onion_per_tot$crop<-'Onion'

carrot_per_tot<-carrot%>% select(`Corrected Daily Count`,Grouping) %>% group_by(Grouping) %>% summarise(total_count=sum(`Corrected Daily Count`)) %>% ungroup() %>% mutate(per=100*total_count/sum(total_count)) 
carrot_per_tot$crop<-'Carrot'
allcrops_per_tot<-rbind(apple_per_tot,pear_per_tot,avocado_per_tot,kiwifruit_per_tot,pakchoi_per_tot,radish_per_tot,onion_per_tot,carrot_per_tot)  


allcrops_per_tot %>%   ggplot(aes(x = crop, y = per, fill = Grouping)) +
  geom_bar(position='stack',stat='identity',width=0.9) +
  #facet_grid(.~crop, scales = "free", space = "free") +
  #facet_wrap(~crop, scales = "free") +
  theme_bw(base_size = 12) +
  theme(strip.text.x = element_text(angle = 90,size=7),axis.text.x = element_text(angle = 90,size=7,hjust=1))+
  scale_fill_manual(values=c('lightgrey','black'))+
  ggtitle(paste('Bees vs Flies'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))

  
#############################
  #Pies of flies#
############################
  
unique(apple$Region)
unique(pear$Region)
unique(avocado$Region)
unique(kiwifruit$Region)
unique(pakchoi$Region)
unique(radish$Region)
unique(onion$Region)
unique(carrot$Region)

#just realising that the size wont be consistent between the sheets so lets do a facet grid and see if that solves it
#make it a loop
  cropsp<-'Pakchoi'
  crop_nonbees<-pakchoi %>% filter(Grouping=='Non_Bees')
  #apple looks dumb so doing a log+1 to see if that helps
  totals<-crop_nonbees %>% filter(Grouping=='Non_Bees') %>% select(`Location and season`,`Corrected Daily Count`,Region) %>% group_by(`Location and season`,Region) %>% summarise(total=sum(`Corrected Daily Count`))
  mydf<-left_join(crop_nonbees,totals,by=c('Location and season','Region'))
   image<-mydf %>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_grid( Region~`Location and season` ) +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          strip.text.x = element_text(angle = 90,size=4,hjust=0),
          strip.text.y = element_text(angle = 0,size=4,hjust=0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA))+
        #  strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  #This actually save the plot in a image
  #ggsave(file=paste0(cropsp,"_",'HawkesBay',".svg"), plot=image, width=15, height=12)
  ggsave(file=paste0(cropsp,"_","allareas",".svg"), plot=image, width=30, height=12)
  ggsave(file=paste0(cropsp,"_","allareas",".pdf"), plot=image, width=30, height=12)

  
#barcharts but log+1  
  #apple looks dumb so doing a log+1 to see if that helps
  crop_nonbees$Corrected_Daily_Count_log1<-log(crop_nonbees$`Corrected Daily Count`+1)
  totals<-crop_nonbees %>% filter(Grouping=='Non_Bees') %>% select(`Location and season`,Corrected_Daily_Count_log1,Region) %>% group_by(`Location and season`,Region) %>% summarise(total=sum(Corrected_Daily_Count_log1))
  mydf<-left_join(crop_nonbees,totals,by=c('Location and season','Region'))
  image<-mydf %>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_grid( Region~`Location and season` ) +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          strip.text.x = element_text(angle = 90,size=4,hjust=0),
          strip.text.y = element_text(angle = 0,size=4,hjust=0),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA))+
    #  strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  
  ggsave(file=paste0(cropsp,"_","allareas_log1",".svg"), plot=image, width=30, height=12)
  ggsave(file=paste0(cropsp,"_","allareas_log1",".pdf"), plot=image, width=30, height=12)







######################################
#old code that looped through locatins within a crop 
#unfortunelty it wont keep the size consistent between regions so shouldnt use it
###################################


#make it a loop
for (item in unique(apple$Region)){
  cropsp<-'Apple'
  print(item)
  mydf<-apple %>% filter(Region==item)
  #  mydf<-apple %>% filter(Region=='Hawkes Bay')
  totals<-mydf %>% select(`Location and season`,`Corrected Daily Count`) %>% group_by(`Location and season`) %>% summarise(total=sum(`Corrected Daily Count`))
  mydf<-left_join(mydf,totals,by='Location and season')
  image<-mydf %>% filter(Grouping=='Non_Bees')%>% ggplot(aes(x = total/2, y = `Corrected Daily Count`, fill = as.factor(Ordering), width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ `Location and season` , strip.position = "bottom") +
    coord_polar("y", start = 0, direction = -1) +
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 6), 
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 4))+
    scale_fill_manual(values=with(brads_col_2,setNames(Colour,Code)))+
    #   scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp,": ",item))+
    theme(
      legend.text = element_text(size = 6), 
      legend.title = element_text(size = 6),
      legend.key.size = unit(1,"line"))
  #This actually save the plot in a image
  #ggsave(file=paste0(cropsp,"_",'HawkesBay',".svg"), plot=image, width=15, height=12)
  ggsave(file=paste0(cropsp,"_",item,".svg"), plot=image, width=15, height=12)
  ggsave(file=paste0(cropsp,"_",item,".pdf"), plot=image, width=15, height=12)
}


#colours
insects_taxonomy<-read_excel('Insect groupings life histories Eddy_cp.xlsx', sheet = 'Taxonomic and life histories',na='NA')

