#clean analysis brads data
#eddy 26/9/24

#other script has all the workings but got really messy with the scribling to find the duplicates, mislabeled stuff etc so just writing out final clean analysis here to clarity

library('tidyverse')
library('readxl')
library('vegan')

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Analysis_Sep2024")

#cant get it in on excel converted to csv and no issue
#csv probably the safest way to get these in judging from the issues I have with them
#two sets one for 2021-2023 and nes for 2023-2024
insects_2021_2023<-read.csv('Insects visiting native plants Eddy BH202123 final_sep26.csv', strip.white=TRUE)

insects_2024<-read.csv('Flower visit data Eddy 2024_sep26.csv', strip.white=TRUE)

#pull all two species of interest out
test<-insects_2021_2023 %>% filter(Native_Plant_Species=="Veronica_salicifolia"|Native_Plant_Species=="Kunzea_ericoides") %>% select(Insect_key_name,Date_Text,Native_Plant_Species,Timeperiod,Property_name,Tree_number,Insect_count)

test_2024<-insects_2024 %>% filter(Native_Plant_Species=="Veronica_salicifolia"|Native_Plant_Species=="Kunzea_ericoides") %>% select(Insect_key_name,Date_Text,Native_Plant_Species,Timeperiod,Property_name,Tree_number,Insect_count)

#brad would also like me to do cabbage tree for 2021-2022 season but will do that separately
test_cabbage<-insects_2021_2023 %>% filter(Native_Plant_Species=="Cordyline_australis") %>% select(Insect_key_name,Date_Text,Native_Plant_Species,Timeperiod,Property_name,Tree_number,Insect_count)

#brads new taxonomy file
insects_taxonomy<-read_excel('Insect groupings life histories Eddy_cp.xlsx', sheet = 'Taxonomic and life histories',na='NA')

#check insect names maps across
x<-sort(insects_taxonomy$Survey_sheet_name)
y<-sort(unique(test$Insect_key_name))
identical(x,y)
y<-sort(unique(test_2024$Insect_key_name))
identical(x,y)
y<-sort(unique(test_cabbage$Insect_key_name))
identical(x,y)

#adding in season column, so season in this case is classed from june 1st to may 31st per year
test<-test %>% mutate(Date = as.Date(Date_Text, format= "%d_%m_%Y")) %>% mutate(season=case_when(Date>='2020-06-01' & Date<='2021-05-31' ~'2020-2021 Season',Date>='2021-06-01' & Date<='2022-05-31' ~'2021-2022 Season', Date>='2022-06-01' & Date<='2023-05-31' ~'2022-2023 Season', Date>='2023-06-01' & Date<='2024-05-31' ~'2023-2024 Season'))
unique(test$season)

test_2024<-test_2024 %>% mutate(Date = as.Date(Date_Text, format= "%d_%m_%Y")) %>% mutate(season=case_when(Date>='2020-06-01' & Date<='2021-05-31' ~'2020-2021 Season',Date>='2021-06-01' & Date<='2022-05-31' ~'2021-2022 Season', Date>='2022-06-01' & Date<='2023-05-31' ~'2022-2023 Season', Date>='2023-06-01' & Date<='2024-05-31' ~'2023-2024 Season'))
unique(test_2024$season)

test_cabbage<-test_cabbage %>% mutate(Date = as.Date(Date_Text, format= "%d_%m_%Y")) %>% mutate(season=case_when(Date>='2020-06-01' & Date<='2021-0 5-31' ~'2020-2021 Season',Date>='2021-06-01' & Date<='2022-05-31' ~'2021-2022 Season', Date>='2022-06-01' & Date<='2023-05-31' ~'2022-2023 Season', Date>='2023-06-01' & Date<='2024-05-31' ~'2023-2024 Season'))
unique(test_cabbage$season)
#just dropping the 2022-2023 season for cabbage as there is only a few enteries for it and brad just wants to tlook at 2021-2022
#test_cabbage %>% select(Property_name,Date,Timeperiod,season) %>% unique() %>% arrange(season,Property_name,Date,Timeperiod)
test_cabbage<-test_cabbage %>% filter(season=='2021-2022 Season')

#check there is no na
test[is.na(test$season),]
test_2024[is.na(test_2024$season),]
test_cabbage[is.na(test_2024$season),]

#making a long dataframe and checking for dups
#checking for dups like there was in Marr 2024
data_long <- test %>% ungroup() %>% select(-Date_Text,-Date) %>% 
  group_by(Property_name, Native_Plant_Species,Timeperiod,Tree_number,Insect_key_name,season) %>%
  summarise(Insect_count = sum(Insect_count))

colnames(test_2024)
data_long_2024 <- test_2024 %>% ungroup() %>% select(-Date_Text,-Date) %>% 
  group_by(Property_name, Native_Plant_Species,Timeperiod,Tree_number,Insect_key_name,season) %>%
  summarise(Insect_count = sum(Insect_count))

nrow(test)
nrow(data_long)
#there is a few dups still here but I have checked them and they are for globetail hover nutpoint, all the counts are 0 so the summarise pushes them together and it wont change any stats - mentioned it to brad and he said he would delete the dups from the master sheet

nrow(test_2024)
nrow(data_long_2024)


#making a megadata frame

head(test)
head(test_2024)
head(test_cabbage)
colnames(test)
colnames(test_2024)
test_allseasons<-rbind(test_2024,test,test_cabbage)

data_long_allseasons <- test_allseasons %>% ungroup() %>% select(-Date_Text,-Date) %>% 
  group_by(Property_name, Native_Plant_Species,Timeperiod,Tree_number,Insect_key_name,season) %>%
  summarise(Insect_count = sum(Insect_count))


unique(data_long_allseasons$season)

#add columns in with new taxonomy groupings and colours from the insects_taxonomy file
new_names<-insects_taxonomy %>% select(`Categorical insect grouping`,Brad_names,Eddy_names,Brad_colours,Survey_sheet_name)
head(data_long_allseasons)
#new_names$`Survey sheet name`
data_long_allseasons<-left_join(data_long_allseasons,new_names,by=c('Insect_key_name'='Survey_sheet_name'))
#data_long<-left_join(data_long,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

#resetting the levels for plotting
data_long_allseasons$Timeperiod <- factor(data_long_allseasons$Timeperiod, levels=c("Morning", "Midday", "Afternoon"))
#data_long$Timeperiod <- factor(data_long$Timeperiod, levels=c("Morning", "Midday", "Afternoon"))

data_long_allseasons$season <- factor(data_long_allseasons$season, levels=c("2021-2022 Season", "2022-2023 Season", "2023-2024 Season"))

#brad wants thing in a specfic order
data_long_allseasons$Eddy_names<-factor(data_long_allseasons$Eddy_names, levels=c('Bee_Honey','Bee_Bumble','Bee_Leioproctus','Bee_Lasioglossum','Bee_Hylaeus','Wasp','Fly_Blow','Fly_Cluster','Fly_Anthomyiid','Fly_Bristle','Fly_Flesh','Fly_House','Fly_Horse','Fly_Stilleto','Fly_March','Fly_Soldier','Fly_Hover','Ladybird Beetle','Butterflies/Moths','Other Insect'))

#can probably do brads names and get away with it its 20 colours
cols<-insects_taxonomy %>% select(Brad_names,Brad_colours,Eddy_names,Brad_colours_bh,Brad_colours_reworked,Survey_sheet_name) %>% unique() %>% arrange(Eddy_names)

#i've made a column that is a new set of names for plotting
ggplot(data_long_allseasons, aes(x = Timeperiod, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_wrap(~season*Native_Plant_Species,ncol = 2) +labs(y= "Insect Counts", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))

#just going to break out cabbage for plot as its a bit messy looking with it in there
data_long_allseasons %>% filter(Native_Plant_Species=='Cordyline_australis') %>% ggplot(., aes(x = Timeperiod, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle(expression('Raw abundance:'~italic('Cordyline australis')~'2021-2022 season'))+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names))) +labs(y= "Insect Counts", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))+theme(plot.title = element_text(size = 8))

data_long_allseasons %>% filter(Native_Plant_Species!='Cordyline_australis') %>% ggplot(., aes(x = Timeperiod, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_wrap(~season*Native_Plant_Species,ncol = 2) +labs(y= "Insect Counts", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))

ggplot(data_long_allseasons, aes(x = factor(Property_name), y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_grid(~Native_Plant_Species*season,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Raw abundances')+ylab('Insect Count') + xlab('Property') + theme(strip.text.x =element_text(size=8))+ guides(fill=guide_legend(title="Taxon"))

#relative abundances
data_long_allseasons_ra<-data_long_allseasons %>% ungroup() %>%  select(-Tree_number) %>% 
  group_by(Native_Plant_Species,season,Timeperiod,Insect_key_name) %>%
  summarise(Insect_count =  sum(Insect_count)) %>%  mutate(per =  100 * (Insect_count/sum(Insect_count))) %>% 
  ungroup()

data_long_allseasons_ra<-left_join(data_long_allseasons_ra,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

#brad wants thing in a specfic order
data_long_allseasons_ra$Eddy_names<-factor(data_long_allseasons_ra$Eddy_names, levels=c('Bee_Honey','Bee_Bumble','Bee_Leioproctus','Bee_Lasioglossum','Bee_Hylaeus','Wasp','Fly_Blow','Fly_Cluster','Fly_Anthomyiid','Fly_Bristle','Fly_Flesh','Fly_House','Fly_Horse','Fly_Stilleto','Fly_March','Fly_Soldier','Fly_Hover','Ladybird Beetle','Butterflies/Moths','Other Insect'))

#plots
#just going to break out cabbage for plot as its a bit messy looking with it in there
data_long_allseasons_ra %>% filter(Native_Plant_Species=='Cordyline_australis') %>% ggplot(., aes(x = Timeperiod, y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle(expression('Relative abundance:'~italic('Cordyline australis')~'2021-2022 season'))+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names))) +labs(y= "Insect proportions", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))+theme(plot.title = element_text(size = 8))

data_long_allseasons_ra %>% filter(Native_Plant_Species!='Cordyline_australis') %>% ggplot(., aes(x = Timeperiod, y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Relative abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_wrap(~season*Native_Plant_Species,ncol = 2) +labs(y= "Insect proportion", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))

#by location no time period
data_long_allseasons_ra<-data_long_allseasons %>% ungroup() %>%  select(-Tree_number) %>% 
  group_by(Native_Plant_Species,season,Property_name,Insect_key_name) %>%
  summarise(Insect_count =  sum(Insect_count))%>%  mutate(per =  100 *Insect_count/sum(Insect_count)) %>% 
  ungroup()

data_long_allseasons_ra<-left_join(data_long_allseasons_ra,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

data_long_allseasons_ra$Eddy_names<-factor(data_long_allseasons_ra$Eddy_names, levels=c('Bee_Honey','Bee_Bumble','Bee_Leioproctus','Bee_Lasioglossum','Bee_Hylaeus','Wasp','Fly_Blow','Fly_Cluster','Fly_Anthomyiid','Fly_Bristle','Fly_Flesh','Fly_House','Fly_Horse','Fly_Stilleto','Fly_March','Fly_Soldier','Fly_Hover','Ladybird Beetle','Butterflies/Moths','Other Insect'))

ggplot(data_long_allseasons_ra, aes(x = factor(Property_name), y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_grid(~Native_Plant_Species*season,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Relative abundances')+ylab('Insect proportion') + xlab('Property') + theme(strip.text.x =element_text(size=8))+ guides(fill=guide_legend(title="Taxon"))

#that should be all the bar charts needed moving on to other analyses
##############
##############

#PCA analyses
#I think Im going to have to group these out by either species or year

#brad wants
#one year one species

#one year all species

#all years one species

#okey dokey

#After talking to brad each sample is going to be the plants sum of counts across the morning, midday, afternoon observations

#so sum across the plant replicates morning, midday, afternoon
#doing one year one species

#########################################
#start with cabbage for 2021-2022 season#
#########################################

data_long_cordyline<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Cordyline_australis') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

data_long_cordyline_sumproperty<-data_long_cordyline %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number,-season) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))

#MDS
data_long_cordyline_sumproperty<-data_long_cordyline_sumproperty %>% remove_rownames %>% column_to_rownames(var="Property_plant")
plant_NMDS <- metaMDS(data_long_cordyline_sumproperty)
plant_NMDS
#stress is >0.2 (0.298)
#maybe its a bit complex so as per metaMDS increasing the k value and adding more trys
plant_NMDS <- metaMDS(data_long_cordyline_sumproperty,trymax=200,k=3)
plant_NMDS #repeats best solution now
#stress is better though still >0.2 (0.21)
stressplot(plant_NMDS)
plot(plant_NMDS)

#adding in variables for plotting
meta_data_farm<-read.csv('Farm_meta_data_sep2024_brad.csv')



plot_df <- scores(plant_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% mutate(site=str_extract(site, ".+(?=_Cor)")) %>% 
  left_join(meta_data_farm, by = c("site" = "Property_name"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = site)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS Cordyline australis (2021-2022)")
plot_nmds

library(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 6))
plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = site)) +
  geom_point(size = 2, alpha = 0.8) +
#  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = expression('NMDS:'~italic('Cordyline australis')~'2021-2022 season'))+
  scale_colour_manual(values=mycolors)+theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Farm"))
plot_nmds

mycol3 = c(brewer.pal(name="Set1", n = 3))
plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Farm.type)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = expression('NMDS:'~italic('Cordyline australis')~'2021-2022 season'))+theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Farm type"))+
  scale_colour_manual(values=mycol3)
plot_nmds

#I dont think there is much patterns there to try and plot any variables out so Im not going too at the moment

############################################################################
#next kanuka for 2021-2022,2022-2023 and 2023-2024 seasons Kunzea ericoides#
############################################################################

#firstly 2021-2022
#data_long_kunzea<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Kunzea_ericoides'& season=='2021-2022 Season') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
#second 2022-2023
#am dropping glentunnel as 3 trees have no insect counts so its fucking with the maths
data_long_kunzea<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Kunzea_ericoides'& season=='2022-2023 Season'& Property_name!='Glentunnel') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
#third 2023-2024
data_long_kunzea<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Kunzea_ericoides'& season=='2023-2024 Season') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

data_long_kunzea_sumproperty<-data_long_kunzea %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number,-season) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))

#MDS
data_long_kunzea_sumproperty<-data_long_kunzea_sumproperty %>% remove_rownames %>% column_to_rownames(var="Property_plant")

#hmmmm some rows are empty in 2022-2023 lets see which ones:
#rowSums(data_long_kunzea_sumproperty)
#glentunnel tree 4,5,6 have 0 counts...have confirmed that on the original spreedsheet
#Im going to drop glentunnel for now

plant_NMDS <- metaMDS(data_long_kunzea_sumproperty)
plant_NMDS
#stress is >0.2 (0.26)
#maybe its a bit complex so as per metaMDS increasing the k value and adding more trys
plant_NMDS <- metaMDS(data_long_kunzea_sumproperty,trymax=200,k=3)
plant_NMDS #repeats best solution now
#stress is better 0.188
stressplot(plant_NMDS)
plot(plant_NMDS)

#adding in variables for plotting
meta_data_farm<-read.csv('Farm_meta_data_sep2024_brad.csv')

plot_df <- scores(plant_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% mutate(site=str_extract(site, ".+(?=_Kun)")) %>% 
  left_join(meta_data_farm, by = c("site" = "Property_name"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = site)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS Kunzea ericoides")
plot_nmds

library(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 9))
plot_nmds1 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = site)) +
  geom_point(size = 2, alpha = 0.8) +
 #   stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
 # labs(title = expression('NMDS:'~italic('Kunzea ericoides')~'2021-2022 season'))+
  #labs(title = expression('NMDS:'~italic('Kunzea ericoides')~'2022-2023 season'))+
  labs(title = expression('NMDS:'~italic('Kunzea ericoides')~'2023-2024 season'))+
  scale_colour_manual(values=mycolors)+theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Farm"))
plot_nmds1

mycol3 = c(brewer.pal(name="Set1", n = 3))
plot_nmds2 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Farm.type)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  #labs(title = expression('NMDS:'~italic('Kunzea ericoides')~'2021-2022 season'))+
  #labs(title = expression('NMDS:'~italic('Kunzea ericoides')~'2022-2023 season'))+
  labs(title = expression('NMDS:'~italic('Kunzea ericoides')~'2023-2024 season'))+
  theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Farm type"))+scale_colour_manual(values=mycol3)
plot_nmds2

#if we wanted to plot the PCAs in a grid, but it doesnt look good as the legends get real squishy
#library(gridGraphics)
#myPlots = list(plot_nmds1, plot_nmds2)
#ggpubr::ggarrange(plotlist = myPlots, nrow = 1)
#data_long_wide<-data_long %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

##############################################################################
#next hebe for 2021-2022,2022-2023 and 2023-2024 seasons Veronica_salicifolia#
##############################################################################

#firstly 2021-2022
#data_long_veronica<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Veronica_salicifolia'& season=='2021-2022 Season') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
#second 2022-2023
#data_long_veronica<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Veronica_salicifolia'& season=='2022-2023 Season') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
#third 2023-2024
data_long_veronica<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Veronica_salicifolia'& season=='2023-2024 Season') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

data_long_veronica_sumproperty<-data_long_veronica %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number,-season) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))


#MDS
data_long_veronica_sumproperty<-data_long_veronica_sumproperty %>% remove_rownames %>% column_to_rownames(var="Property_plant")

plant_NMDS <- metaMDS(data_long_veronica_sumproperty)
plant_NMDS
#stress is >0.2 (0.26)
#maybe its a bit complex so as per metaMDS increasing the k value and adding more trys
plant_NMDS <- metaMDS(data_long_veronica_sumproperty,trymax=200,k=3)
plant_NMDS #repeats best solution now
#stress is better 0.188
stressplot(plant_NMDS)
plot(plant_NMDS)

#adding in variables for plotting
meta_data_farm<-read.csv('Farm_meta_data_sep2024_brad.csv')

plot_df <- scores(plant_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% mutate(site=str_extract(site, ".+(?=_Ver)")) %>% 
  left_join(meta_data_farm, by = c("site" = "Property_name"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = site)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS Veronica salicifolia")
plot_nmds

library(RColorBrewer)
mycolors = c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 12),brewer.pal(name='Set1',n=4))
plot_nmds1 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = site)) +
  geom_point(size = 2, alpha = 0.8) +
  #   stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  #labs(title = expression('NMDS:'~italic('Veronica salicifolia')~'2021-2022 season'))+
  #labs(title = expression('NMDS:'~italic('Veronica salicifolia')~'2022-2023 season'))+
  labs(title = expression('NMDS:'~italic('Veronica salicifolia')~'2023-2024 season'))+
  scale_colour_manual(values=mycolors)+theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Farm"))
plot_nmds1

mycol3 = c(brewer.pal(name="Set1", n = 3))
plot_nmds2 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Farm.type)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
 # labs(title = expression('NMDS:'~italic('Veronica salicifolia')~'2021-2022 season'))+
  #labs(title = expression('NMDS:'~italic('Veronica salicifolia')~'2022-2023 season'))+
  labs(title = expression('NMDS:'~italic('Veronica salicifolia')~'2023-2024 season'))+
  theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Farm type"))+scale_colour_manual(values=mycol3)
plot_nmds2

######################
#one year all species#
######################

#firstly Native_Plant_Species=='Kunzea_ericoides'
data_long_plant<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Kunzea_ericoides'& Property_name!='Glentunnel') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
#second Native_Plant_Species=='Veronica_salicifolia'
data_long_plant<-data_long_allseasons %>% ungroup() %>% filter(Native_Plant_Species=='Veronica_salicifolia') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

data_long_plant<-data_long_plant %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,season,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number,-season) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))


#MDS
data_long_plant<-data_long_plant %>% remove_rownames %>% column_to_rownames(var="Property_plant")

plant_NMDS <- metaMDS(data_long_plant)
plant_NMDS
#stress is >0.2 (0.26)
#maybe its a bit complex so as per metaMDS increasing the k value and adding more trys
plant_NMDS <- metaMDS(data_long_plant,trymax=300,k=3)
plant_NMDS #repeats best solution now
#stress is better 0.188
stressplot(plant_NMDS)
plot(plant_NMDS)

#adding in variables for plotting
meta_data_farm<-read.csv('Farm_meta_data_sep2024_brad.csv')

plot_df <- scores(plant_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% mutate(site_name=str_extract(site, ".+(?=_Ver)|.+(?=_Kun)|.+(?=_Cor)")) %>% 
  mutate(plant_name=str_extract(site, "Veronica_salicifolia|Kunzea_ericoides|Cordyline_australis"))%>% 
  mutate(season=str_extract(site, "2021-2022 Season|2022-2023 Season|2023-2024 Season")) %>%
  left_join(meta_data_farm, by = c("site" = "Property_name"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = season)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS season")
plot_nmds

library(RColorBrewer)
mycol3 = c(brewer.pal(name="Set1", n = 3))
plot_nmds1 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = season)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
 #labs(title = 'NMDS: Kunzea ericoides')+
  labs(title = 'NMDS: Veronica salicifolia')+
  scale_colour_manual(values=mycol3)+theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Plant"))
plot_nmds1

#######################
#all years one species#
#######################

#dont need to do cabbage as that only has one year which we already did
#firstly 2021-2022
#data_long_season<-data_long_allseasons %>% ungroup() %>% filter(season=='2021-2022 Season') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
#second 2022-2023
#drop glentunnel which has the 0 counts
#data_long_season<-data_long_allseasons %>% ungroup() %>% filter( season=='2022-2023 Season'& Property_name!='Glentunnel') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
#third 2023-2024
data_long_season<-data_long_allseasons %>% ungroup() %>% filter( season=='2023-2024 Season') %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

data_long_season<-data_long_season %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number,-season) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))

#MDS
data_long_season<-data_long_season %>% remove_rownames %>% column_to_rownames(var="Property_plant")

plant_NMDS <- metaMDS(data_long_season)
plant_NMDS
#stress is >0.2 (0.26)
#maybe its a bit complex so as per metaMDS increasing the k value and adding more trys
plant_NMDS <- metaMDS(data_long_season,trymax=300,k=3)
plant_NMDS #repeats best solution now
#stress is better 0.188
stressplot(plant_NMDS)
plot(plant_NMDS)

#adding in variables for plotting
meta_data_farm<-read.csv('Farm_meta_data_sep2024_brad.csv')

plot_df <- scores(plant_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% mutate(site_name=str_extract(site, ".+(?=_Ver)|.+(?=_Kun)|.+(?=_Cor)")) %>% 
  mutate(plant_name=str_extract(site, "Veronica_salicifolia|Kunzea_ericoides|Cordyline_australis"))%>% 
  mutate(season=str_extract(site, "2021-2022 Season|2022-2023 Season|2023-2024 Season")) %>%
  left_join(meta_data_farm, by = c("site_name" = "Property_name"))

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = plant_name)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS season")
plot_nmds

library(RColorBrewer)
mycol3 = c(brewer.pal(name="Set1", n = 3))
mycol3
#keep colours consistent across season plots
mycol2<-c("#377EB8","#4DAF4A")
plot_nmds1 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = plant_name)) +
  geom_point(size = 2, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  #labs(title = 'NMDS: Season 2021-2022')+
  #labs(title = 'NMDS: Season 2022-2023')+
  labs(title = 'NMDS: Season 2023-2024')+
  scale_colour_manual(values=mycol3)+
  #scale_colour_manual(values=mycol2)+
    theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Plant"))
plot_nmds1

#this is the only set of plots that show some spatial structure so seeing what insects contribute most heavily to that
#envfit() takes the output of metaMDS() and the species matrix you created
fit <- envfit(plant_NMDS, data_long_season, perm = 999) 
fit
warnings()
# extract p-values for each species
fit_pvals <- fit$vectors$pvals %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  dplyr::rename("pvals" = ".")

# extract coordinates for species, only keep species with p-val = 0.05
fit_spp <- fit %>% 
  scores(., display = "vectors") %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  full_join(., fit_pvals, by = "species") %>% 
  #filter(pvals <= 0.05)
filter(pvals <= 0.001)
#0.05 is a lot of speices could try 0.01, which is still>20 which is hard to plot

#brad has new names
new_names_brad<-read.csv('NewNames_NMDSarrows.csv')
fit_spp<-left_join(fit_spp,new_names_brad,by=c('species'='Old_name'))

# new plot
nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(color = plant_name), size = 3, alpha = 0.8) +
  stat_ellipse(aes(color = plant_name)) +
  #scale_color_manual(values = pal) +
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  geom_text(data = fit_spp, aes(label = species)) +
  theme_bw()
nmds_plot_new


library(ggrepel)
mycol3 = c(brewer.pal(name="Set1", n = 3))
mycol3
#keep colours consistent across season plots
mycol2<-c("#377EB8","#4DAF4A")
plot_nmds1 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(color=plant_name),size = 2, alpha = 0.8) +
  stat_ellipse(aes(color=plant_name),linetype = 2,size=1) +
  theme_bw()+
 # labs(title = 'NMDS: Season 2021-2022, pval <0.001')+
  #labs(title = 'NMDS: Season 2022-2023, pval <0.001')+
  labs(title = 'NMDS: Season 2023-2024, pval <0.001')+
  #scale_colour_manual(values=mycol3)+
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  scale_colour_manual(values=mycol2)+
  #geom_text(data = fit_spp, aes(label = species)) +
  geom_text_repel(data = fit_spp, aes(label = species), segment.color="red",max.overlaps=59)+
  theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Plant"))
plot_nmds1

#new names
plot_nmds1 <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(color=plant_name),size = 2, alpha = 0.8) +
  stat_ellipse(aes(color=plant_name),linetype = 2,size=1) +
  theme_bw()+
 # labs(title = 'NMDS: Season 2021-2022, pval <0.001')+
  #labs(title = 'NMDS: Season 2022-2023, pval <0.001')+
  labs(title = 'NMDS: Season 2023-2024, pval <0.001')+
  #scale_colour_manual(values=mycol3)+
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  scale_colour_manual(values=mycol2)+
  #geom_text(data = fit_spp, aes(label = species)) +
  geom_text_repel(data = fit_spp, aes(label = New_name), segment.color="red",max.overlaps=59)+
  theme(plot.title = element_text(size = 10),axis.title=element_text(size=8))+ guides(color=guide_legend(title="Plant"))
plot_nmds1


#very busy but maybe of use to brad

#alpha diversity analyses

data_long_wide<-data_long_allseasons %>% ungroup() %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)
data_long_wide<-data_long_wide %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,season,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number,-season) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))

data_long_wide<-data_long_wide %>% remove_rownames %>% column_to_rownames(var="Property_plant")
?diversity
simpsondiv <- diversity(data_long_wide,index='simpson')
head(simpsondiv)

#can plot that out
simpsondiv_df <- simpsondiv %>% 
  enframe() %>%  mutate(site_name=str_extract(name, ".+(?=_Ver)|.+(?=_Kun)|.+(?=_Cor)")) %>% 
  mutate(plant_name=str_extract(name, "Veronica_salicifolia|Kunzea_ericoides|Cordyline_australis"))%>% 
  mutate(season=str_extract(name, "2021-2022 Season|2022-2023 Season|2023-2024 Season")) %>%
  left_join(meta_data_farm, by = c("site_name" = "Property_name"))

mycol3 = c(brewer.pal(name="Set1", n = 3))
mycol3
#keep colours consistent across season plots
mycol2<-c("#377EB8","#4DAF4A")
plot_sppr <- ggplot(simpsondiv_df, aes(x = Farm.type, y = value, fill = Farm.type)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Farm type",
       y = "Simpson diversity richness",
       title = "Species richness")+ facet_grid(~season*plant_name,scales='free',space='free')+
  scale_fill_manual(values=mycol3)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_sppr

#looking across the seasons
plot_sppr <- ggplot(simpsondiv_df, aes(x = season, y = value, fill = season)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Season",
       y = "Simpson diversity richness",
       title = "Species richness")+ facet_grid(~plant_name,scales='free',space='free')+
scale_fill_manual(values=mycol3)+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
plot_sppr

#is it significant
sppr_aov <-aov(value ~Farm.type, simpsondiv_df)
summary(sppr_aov)

sppr_aov <-aov(value ~plant_name, simpsondiv_df)
summary(sppr_aov)

sppr_aov <-aov(value ~season, simpsondiv_df)
summary(sppr_aov)

sppr_aov <-aov(value ~Farm.type+plant_name+season, simpsondiv_df)
summary(sppr_aov)


########################
#beta diversity
########################

?adonis2
#cant handle rows with only zeros
data_long_wide_nozeros<-data_long_wide[rowSums(data_long_wide)>0, ]

data_long_wide_nozeros_meta <- data_long_wide_nozeros %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>%  mutate(site_name=str_extract(site, ".+(?=_Ver)|.+(?=_Kun)|.+(?=_Cor)")) %>% 
  mutate(plant_name=str_extract(site, "Veronica_salicifolia|Kunzea_ericoides|Cordyline_australis"))%>% 
  mutate(season=str_extract(site, "2021-2022 Season|2022-2023 Season|2023-2024 Season")) %>%
  left_join(meta_data_farm, by = c("site_name" = "Property_name"))


perm <- adonis2(data_long_wide_nozeros ~ Farm.type, data = data_long_wide_nozeros_meta)
perm
perm <- adonis2(data_long_wide_nozeros ~ season, data = data_long_wide_nozeros_meta)
perm
perm <- adonis2(data_long_wide_nozeros ~ plant_name, data = data_long_wide_nozeros_meta)
perm
perm <- adonis2(data_long_wide_nozeros ~ plant_name+season+Farm.type, data = data_long_wide_nozeros_meta)
perm

#hmmmmm I need to think about this analysis more
