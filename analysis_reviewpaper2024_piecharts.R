#making pie charts for brad

#need to make them resized based on abundance
#need to be editable for brad (SVG etc)
library(tidyverse)
library(lme4)
library(readxl)
library(svglite)

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024")

apple<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Apple')
pear<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Pear')
avocado<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Avocado')
kiwifruit<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Kiwifruit')
pakchoi<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Pak choi')
radish<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Radish')
onion<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Onion')
carrot<-read_excel('Copy of Pie chart data for Eddy.xlsx',sheet='Carrot')

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

#check genus
#check location and season
unique(apple$Genus)
unique(pear$Genus)
unique(avocado$Genus)
unique(kiwifruit$Genus)
unique(pakchoi$Genus)
unique(radish$Genus)
unique(onion$Genus)
unique(carrot$Genus)

genus<-c("Apis","Bombus" ,"Hylaeus","Lasioglossum", "Leioproctus" , "Megachile")
col_genus<-c('gold','gold3','firebrick4','firebrick1','firebrick','mediumorchid4')

brads_col<-data.frame(genus,col_genus) %>% arrange(genus)
#brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)



#looks good 
unique(apple$Region)
#[1] "Hawkes Bay" "Motueka" 
mydf<-apple %>% filter(Region=='Hawkes Bay')
head(mydf)
totals<-mydf %>% select(`Location and season`,Count) %>% group_by(`Location and season`) %>% summarise(total=sum(Count))

mydf<-left_join(mydf,totals,by='Location and season')

mydf %>% ggplot(aes(x = total, y = Count, fill = Genus, width = total)) +
#mydf %>% ggplot(aes(x = total/5, y = Count, fill = Genus, width = total)) +
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
  scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
  ggtitle(paste('Apple','Hawkes Bay'))+
  theme(
    legend.text = element_text(size = 6), 
    legend.title = element_text(size = 6),
    legend.key.size = unit(1,"line"))


#save the plot in a variable image to be able to export to svg
image<-mydf %>% ggplot(aes(x = total/2, y = Count, fill = Genus, width = total)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ `Location and season` , strip.position = "bottom") +
  coord_polar("y", start = 0, direction = -1) +
  #  theme_grey()
  theme_bw(base_size = 12) +
  theme(axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        panel.border = element_blank(),
        legend.title = element_text(size = 6), 
        strip.background = element_rect(fill = NA, colour = NA),
        strip.text = element_text(size = 4))+
  scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))

#This actually save the plot in a image
ggsave(file="test.svg", plot=image, width=10, height=8)

unique(apple$Region)
unique(pear$Region)
unique(avocado$Region)
unique(kiwifruit$Region)
unique(pakchoi$Region)
unique(radish$Region)
unique(onion$Region)
unique(carrot$Region)

#make it a loop
for (item in unique(carrot$Region)){
  cropsp<-'Carrot'
  print(item)
  mydf<-carrot %>% filter(Region==item)
  totals<-mydf %>% select(`Location and season`,Count) %>% group_by(`Location and season`) %>% summarise(total=sum(Count))
  mydf<-left_join(mydf,totals,by='Location and season')
  image<-mydf %>% ggplot(aes(x = total/2, y = Count, fill = Genus, width = total)) +
    geom_bar(stat = "identity", position = "fill") +
    facet_wrap(~ `Location and season` , strip.position = "bottom") +
    coord_polar("y", start = 0, direction = -1) +
    #  theme_grey()
    theme_bw(base_size = 12) +
    theme(axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          legend.title = element_text(size = 8), 
          strip.background = element_rect(fill = NA, colour = NA),
          strip.text = element_text(size = 8))+
    scale_fill_manual(values=with(brads_col,setNames(col_genus,genus)))+
    ggtitle(paste0(cropsp,": ",item))+
    theme(
      legend.text = element_text(size = 8), 
      legend.title = element_text(size = 8),
      legend.key.size = unit(1,"line"))
  #This actually save the plot in a image
  ggsave(file=paste0(cropsp,"_",item,".svg"), plot=image, width=15, height=12)
  ggsave(file=paste0(cropsp,"_",item,".pdf"), plot=image, width=15, height=12)
  
}

#colours
insects_taxonomy<-read_excel('Insect groupings life histories Eddy_cp.xlsx', sheet = 'Taxonomic and life histories',na='NA')

