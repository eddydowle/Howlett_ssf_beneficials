#having a look at the files
library('tidyverse')
library('readxl')
library('vegan')

#https://www.rpubs.com/an-bui/vegan-cheat-sheet

setwd("C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Analysis_Sep2024")
?read_excel
#im treating blanks as 0 I have discussed the difference between a null or NA and a 0 with brad and he thinks all blank cells should be 0
#insect visitation data
insects_2024<-read_excel('Flower visit data Eddy 2024_noblanks.xlsx', sheet = '2024 data',na='NA')
?read_excel
#have strings in proportion farming the te maania farm says te_maania rather than a number
#check with brad
warnings()
#so I think to start with just do within a species how do the different farms group

#going to need to recreate a metadata file form brads sheet
head(insects_2024)
insects_2024 %>% filter(Insect_key_name=='Drone Fly')

#meta_data<-insects_2024 %>% select(Property_name,`Plant age`,`Planting Age at sampling`,`Planting area size`,Perimeter,`Planting type`,`Nearest remnant`,`Fields within 1 km`,`Farm type`,`Other features`,`Nearest River`,Lat,long,`Hedgerow length within 1 km`,`Proportion farming`,`Building No`,`Water catchment area`,`Proportion Peri-urban`,`Proportion Urban area`,`Plantation area`,`Effluent Pond No.`) %>% unique()

#write.csv(meta_data,'Farm_meta_data_sep2024.csv')

#tidied up due to some doubles so use tidy version for metadata
meta_data_farm<-read.csv('Farm_meta_data_sep2024.csv')

#meta_data for each sampling (seperate from farm metadata)
#want it to be like
#farm_name temp_morn_sample temp_afternoon_sample
#and
#farm_name_morning temp
#farm_name_afternoon temp

meta_data_sample<-insects_2024 %>% select(Property_name,Date,Timeperiod,Native_Plant_Species,Temperature,Humidity,`Wind_min)`,Wind_max,`Light_Intensity (sun)`) %>% unique() %>% arrange(Property_name,Native_Plant_Species)

#some have multiple points per sampling period(morning, afternoon, evening)
#Im going to average them by property, plant species, time period (will need to drop date)
meta_data_sample_ave<-meta_data_sample %>% select(-Date) %>% group_by(Property_name,Timeperiod,Native_Plant_Species) %>% summarise(across(everything(), mean)) %>% arrange(Property_name,Native_Plant_Species)

#write.csv(meta_data_sample_ave,'Sample_2024_metadata_ave.csv')
#write.csv(meta_data_sample,'Sample_2024_metadata.csv')

#planting age at sampling is specific to year

unique(insects_2024$Native_Plant_Species)
#[1] "Veronica_salicifolia"    "Veronica_strictissima"  
#[3] "Kunzea_ericoides"        "Lophomyrtus_obcordata"  
#[5] "Olearia_adenocarpa"      "Hoheria_angustifolia"   
#[7] "Muehlenbeckia_complexa"  "Teucrium_parvifolium"   
#[9] "Muehlenbeckia_axillaris" "Leptospermum_scoparium" 
#[11] "Phormium_tenax"          "Carmichaelia_australis" 
#[13] "Cordyline_australis"     "Phormium_cookianum"     
#[15] "Corpodetus_serratus"     "Corokia_cotoneaster"    
#[17] "Plagianthus_regius"      "Griselinia_littoralis"  
#[19] "Pittosporum_eugenioides" "Olearia paniculata"  

#brad wants to focus on
#Veronica_salicifolia hebe
#Leptospermum_scoparium kanuka
#Cordyline_australis cabbage tree


test<-insects_2024 %>% filter(Native_Plant_Species=="Veronica_salicifolia") %>% select(Insect_key_name,Timeperiod,Property_name,Tree_number,Insect_count)

unique(test$Insect_key_name)
unique(test$Timeperiod)
unique(test$Property_name)
unique(test$Insect_count)
unique(test$Tree_number)

#so what do we want?
#diversity across farms, diversity morning afternoon midday,
#diversity across tree species within a farm? Could group plots by farm?

#pivot wider 
head(test)
#just incase there is any dups
data_long <- test %>% 
  group_by(Property_name, Timeperiod,Tree_number,Insect_key_name) %>%
  summarise(Insect_count = sum(Insect_count))

#lots of dups?
test %>% arrange(Property_name,Timeperiod,Insect_key_name, Tree_number)
duplicates <- test %>% group_by_all() %>% filter(n() > 1) %>%   ungroup()
#all seem to be from Marr, just going to ignore for now but flagging with Brad
#its not a straight dup as the counts vary across the dups so I dont know whats going on but Im just going to keep the sumed values, maybe two people did the survey and it got entered twice???? who knows
test %>% filter(Property_name=='Marr',Insect_key_name=='Honey bee', Timeperiod=='Morning')

test2<-insects_2024 %>% filter(Native_Plant_Species=="Veronica_salicifolia") %>% select(Insect_key_name,Timeperiod,Date,Property_name,Tree_number,Insect_count)

test2 %>% filter(Property_name=='Marr',Insect_key_name=='Honey bee', Timeperiod=='Morning')

#so we want a spreadsheet of:
#farm #time #rep #counts_spA #counts_spB etc
data_long_wide<-data_long %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

#think thats right
#raw abundance plots
ggplot(data_long, aes(x = Property_name, y = Insect_count, fill = Insect_key_name)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')

levels(data_long$Timeperiod)
data_long$Timeperiod <- factor(data_long$Timeperiod, levels=c("Morning", "Midday", "Afternoon"))

ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Insect_key_name)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')

ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Insect_key_name)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+ facet_wrap(vars(Property_name))

#relative abundances
data_long_ra<-data_long %>% ungroup() %>%  select(-Tree_number) %>% 
  group_by(Property_name,Timeperiod,Insect_key_name) %>%
  summarise(Insect_count =  sum(Insect_count))%>%  mutate(per =  100 *Insect_count/sum(Insect_count)) %>% 
  ungroup()

ggplot(data_long_ra, aes(x = Timeperiod, y = per, fill = Insect_key_name)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Relative abundances')+ facet_wrap(vars(Property_name))

library('vegan')
#just going to simplify down to Property_name to start with
data_long_wide_sumproperty<-data_long_wide %>% ungroup() %>% select(-Tree_number,-Timeperiod) %>% group_by(Property_name) %>% summarise(across(everything(), sum,na.rm = TRUE))

#diversity plots
data_long_wide_sumproperty<-data_long_wide_sumproperty %>% remove_rownames %>% column_to_rownames(var="Property_name")

#alpha diversity
#count number of species per property
sppr <- specnumber(data_long_wide_sumproperty)
sppr
#anova against environmental variables, does mean species richness vary among sites?
#not all farms have this plants data:
relavent_farms<-row.names(data_long_wide_sumproperty)
meta_data_farm_sub<-meta_data_farm %>% filter(Property_name %in% relavent_farms)
#try farm.type
sppr_aov <-aov(sppr ~ Farm.type, data = meta_data_farm_sub)
summary(sppr_aov)
#try planting.type
sppr_aov <-aov(sppr ~ Planting.type, data = meta_data_farm_sub)
summary(sppr_aov)
#plant age
sppr_aov <-aov(sppr ~ Plant.age, data = meta_data_farm_sub)
summary(sppr_aov)
#planting area size
sppr_aov <-aov(sppr ~ Planting.area.size, data = meta_data_farm_sub)
summary(sppr_aov)
#no environmental variables significant

#future thing
#should do this with the different plant species

#can plot that out
sppr_df <- sppr %>% 
  enframe() %>% 
  full_join(meta_data_farm_sub, by = c("name" = "Property_name"))

plot_sppr <- ggplot(sppr_df, aes(x = Farm.type, y = value, fill = Farm.type)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Farm type",
       y = "Number of species per site",
       title = "Species richness")
plot_sppr

#shannon diversity index (takes into account richness and evenness)
#diversity function in vegan
shannondiv <- diversity(data_long_wide_sumproperty)
head(shannondiv)

#anova against shannon diversity, does mean species richness vary among sites?
#try farm.type
sppr_aov <-aov(shannondiv ~ Farm.type, data = meta_data_farm_sub)
summary(sppr_aov)
#try planting.type
sppr_aov <-aov(shannondiv ~ Planting.type, data = meta_data_farm_sub)
summary(sppr_aov)
#plant age
sppr_aov <-aov(shannondiv ~ Plant.age, data = meta_data_farm_sub)
summary(sppr_aov)
#planting area size
sppr_aov <-aov(shannondiv ~ Planting.area.size, data = meta_data_farm_sub)
summary(sppr_aov)

#can plot that out
shannondiv_df <- shannondiv %>% 
  enframe() %>% 
  full_join(meta_data_farm_sub, by = c("name" = "Property_name"))

plot_sppr <- ggplot(shannondiv_df, aes(x = Farm.type, y = value, fill = Farm.type)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Farm type",
       y = "Shannon diversity richness",
       title = "Species richness")
plot_sppr

#beta diversity
#permanova to assess community composition differences
perm <- adonis2(data_long_wide_sumproperty ~ Farm.type, data = meta_data_farm_sub)
perm
perm <- adonis2(data_long_wide_sumproperty ~ Plant.age, data = meta_data_farm_sub)
perm


#plot them out on PCA 
plantPCA <- rda(data_long_wide_sumproperty)
plantPCA

PCAscores <- scores(plantPCA, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_farm_sub, by = c("site" = "Property_name"))

PCAvect <- scores(plantPCA, display = "species") %>% 
  as.data.frame()


plot_PCA <- ggplot() +
  geom_point(data = PCAscores, aes(x = PC1, y = PC2, color = Farm.type)) +
  labs(title = "Principal Components Analysis") +
  theme_bw()
plot_PCA
#could plot insect species out on them

#MDS
plant_NMDS <- metaMDS(data_long_wide_sumproperty)
plant_NMDS
#stress <0.2
stressplot(plant_NMDS)
plot(plant_NMDS)

plot_df <- scores(plant_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_farm_sub, by = c("site" = "Property_name"))
plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Farm.type, shape = Farm.type)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS")
plot_nmds

#seeing what species contribute to the plot
# envfit() takes the output of metaMDS() and the species matrix you created
fit <- envfit(plant_NMDS, data_long_wide_sumproperty, perm = 999) 

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
  filter(pvals == 0.05)

# new plot
nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(color = Farm.type, shape = Farm.type), size = 3, alpha = 0.8) +
  stat_ellipse(aes(color = Farm.type)) +
  #scale_color_manual(values = pal) +
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  geom_text(data = fit_spp, aes(label = species)) +
  theme_bw()
nmds_plot_new

#environmental variables
#arrows only work for continuous variables so I cant add in factors
ef <- envfit(plant_NMDS ~ Planting.area.size + Plant.age, data = meta_data_farm_sub, perm = 999)
ef

class(meta_data_farm_sub$Planting.type)

# extract p-values for each variable
ef_pvals <- ef$vectors$pvals %>% 
  as.data.frame() %>% 
  rownames_to_column("env_var") %>% 
  dplyr::rename("pvals" = ".")
# extract coordinates for variable, only keep those with p-val = 0.05
ef_spp <- ef %>% 
  scores(., display = "vectors") %>% 
  as.data.frame() %>% 
  rownames_to_column("env_var") %>% 
  full_join(., ef_pvals, by = "env_var") %>% 
  filter(pvals == 0.05)

#none significant so will not show up on plot
# new plot
nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(color = Farm.type, shape = Farm.type), size = 3, alpha = 0.8) +
  stat_ellipse(aes(color = Farm.type)) +
  #scale_color_manual(values = pal) +
  geom_segment(data = ef_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  geom_text(data = ef_spp, aes(label = env_var)) +
  theme_bw()
nmds_plot_new

#canonical corrrespondence analysis
#think maybe I can add in categorical variables here, lets try
plantCCA <- cca(data_long_wide_sumproperty ~ Farm.type +Planting.area.size + Plant.age+ Planting.type , data = meta_data_farm_sub)
plantCCA
plot(plantCCA)

ccavectors <- as.matrix(scores(plantCCA, display = "bp", scaling = "species")*7.627807) %>% 
  as.data.frame()

# site coordinates
site_data <- scores(plantCCA, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(meta_data_farm_sub, by = c("site" = "Property_name"))

# species coordinates
species_data <- scores(plantCCA, display = "species") %>% 
  as.data.frame()

# plotting
plot_cca <- ggplot(site_data) +
  geom_point(aes(x = CCA1, y = CCA2, color = Farm.type), shape = 19, size = 2, alpha = 0.8) +
  coord_fixed() +
  #scale_color_manual(values = pal) +
  geom_segment(data = ccavectors, aes(x = 0, y = 0, xend = CCA1, yend = CCA2), arrow = arrow(length = unit(0.2, "cm"))) +
  geom_vline(xintercept = c(0), color = "grey70", linetype = 2) +
  geom_hline(yintercept = c(0), color = "grey70", linetype = 2) +
#  geom_point(data = species_data, aes(x = CCA1, y = CCA2), shape = 17, size = 2, color = "slateblue") +
#  scale_x_continuous(limits = c(-12, 10)) +
 # scale_y_continuous(limits = c(-3, 12)) +
  geom_text(data = ccavectors, aes(x = CCA1, y = CCA2, label = rownames(ccavectors)), nudge_x = 0.3, nudge_y = 0.3) +
  theme_bw() +
  labs(title = "Canonical Correspondence Analysis")
plot_cca



#reworking analysis based on what Brad and I discussed 19/9/24 
#just trying plots with species A, B, C next to each other from one year


#brad wants to focus on
#Veronica_salicifolia hebe
#Kunzea ericoides kanuka
#and for 2021 only also do:
#Cordyline_australis cabbage tree 

#have just removed all the other columns to get rid of the errors
insects_2024<-read_excel('Flower visit data Eddy 2024_fixMarr_cp.xlsx', sheet = 'Sheet2')
warnings()

#pull all three species out of dataset
test<-insects_2024 %>% filter(Native_Plant_Species=="Veronica_salicifolia"|Native_Plant_Species=="Kunzea_ericoides") %>% select(Insect_key_name,Native_Plant_Species,Timeperiod,Property_name,Tree_number,Insect_count)

unique(insects_2024$Native_Plant_Species)

#brads new taxonomy file
insects_taxonomy<-read_excel('Insect groupings life histories Eddy_cp.xlsx', sheet = 'Taxonomic and life histories',na='NA')

#check if everything maps across
x<-sort(insects_taxonomy$Survey_sheet_name)
y<-sort(unique(test$Insect_key_name))
x
y
identical(x,y)


#so adundance plots using the new variables names

#getting rid of the Marr dups by summing them (not sure if this is the way we should deal with them)
data_long <- test %>% 
  group_by(Property_name, Native_Plant_Species,Timeperiod,Tree_number,Insect_key_name) %>%
  summarise(Insect_count = sum(Insect_count))
#there is still dups on the new sheet



#renaming to new names
new_names<-insects_taxonomy %>% select(`Categorical insect grouping`,Brad_names,Eddy_names,Brad_colours,Survey_sheet_name)
head(data_long)
#new_names$`Survey sheet name`
data_long<-left_join(data_long,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

data_long$Timeperiod <- factor(data_long$Timeperiod, levels=c("Morning", "Midday", "Afternoon"))

ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Insect_key_name)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances') +theme(legend.position = 'none')

#brads full colours

unique(data_long$`Categorical insect grouping`)
ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = `Categorical insect grouping`)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')

ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Brad_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances') 

#can probably do brads names and get away with it its 20 colours
cols<-insects_taxonomy %>% select(Brad_names,Brad_colours,Eddy_names,Brad_colours_bh,Brad_colours_reworked,Survey_sheet_name) %>% unique() %>% arrange(Eddy_names)

#i've made a column that is a new set of names for plotting
ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))

#if we were to use all of brads colours and names
ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Insect_key_name)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_bh,Survey_sheet_name)))

ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Brad_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours,Brad_names)))

#still looks awful, brad is going to set the colours how he wants them. 

unique(data_long$Brad_names)


#ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = as.character(Brad_names))) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=as.character(cols$Brad_colours), breaks=as.character(cols$Brad_names))

#fucking about trying to get the colours to look better
#ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Brad_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=c("Anthomyiid flies"="steelblue1", "Blow flies"="cyan", "Bristle flies"="turquoise1","Bumble bee"="gold3", "Butterflies/Moths"="darkviolet","Cluster flies"="dodgerblue","Flesh flies"="lightskyblue","Honey bee"="yellow","Horse flies"="darkseagreen1","House flies"="navy","Hover flies"="darkolivegreen2","Hylaeus"="red4","Ladybird beetles"="plum","Lasioglossum"="red","Leioproctus"="orangered","March flies"="rosybrown1","Other insects"="grey","Soldier flies"="green4","Stilleto flies"="limegreen","Wasps"="deeppink"))

#just carrying on for now and will remake the plots once brad has fixed the colours
  
ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_wrap(vars(Native_Plant_Species))+ ylab('Insect Count')

#relevel native plants to flowering order cabbage, kanuka, hebe

unique(data_long$Native_Plant_Species)
data_long$Native_Plant_Species <- factor(data_long$Native_Plant_Species, levels=c( "Kunzea_ericoides", "Veronica_salicifolia"))
head(data_long)

ggplot(data_long, aes(x = Property_name, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_grid(~Native_Plant_Species) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Raw abundances')+ylab('Insect Count')

#what are the blanks probably tree didnt exist at those sites

#yes so replotting to remove blanks 
ggplot(data_long, aes(x = factor(Property_name), y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Native_Plant_Species,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Raw abundances')+ylab('Insect Count') + xlab('Property name') + theme(strip.text.x =element_text(size=8))#+ theme(strip.text.x =element_text(angle=90))

ggplot(data_long, aes(x = factor(Native_Plant_Species), y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Property_name,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Raw abundances')+ylab('Insect Count') + xlab('Native Plant') + theme(strip.text.x =element_text(size=4))


#is too much having morning/midday/night in the same graph
#but can do that for each year think that is okay for raw abundances

#relative abundances
data_long_ra<-data_long %>% ungroup() %>%  select(-Tree_number) %>% 
  group_by(Native_Plant_Species,Timeperiod,Insect_key_name) %>%
  summarise(Insect_count =  sum(Insect_count))%>%  mutate(per =  100 *Insect_count/sum(Insect_count)) %>% 
  ungroup()

data_long_ra<-left_join(data_long_ra,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

#time period across all sites
ggplot(data_long_ra, aes(x = Timeperiod, y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Proportional abundance')+scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_wrap(vars(Native_Plant_Species))+ ylab('Proportion')+ theme(strip.text.x =element_text(size=8))

#by location no time period
data_long_ra<-data_long %>% ungroup() %>%  select(-Tree_number) %>% 
  group_by(Native_Plant_Species,Property_name,Insect_key_name) %>%
  summarise(Insect_count =  sum(Insect_count))%>%  mutate(per =  100 *Insect_count/sum(Insect_count)) %>% 
  ungroup()

data_long_ra<-left_join(data_long_ra,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

#edit to remove blanks
ggplot(data_long_ra, aes(x = Property_name, y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Native_Plant_Species) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Proportional abundance')+ylab('Proportion')

ggplot(data_long_ra, aes(x = factor(Property_name), y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Native_Plant_Species,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Proportional abundance')+ylab('Proportion') + xlab('Native Plant') + theme(strip.text.x =element_text(size=4))

ggplot(data_long_ra, aes(x = factor(Native_Plant_Species), y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Property_name,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Proportional abundance')+ylab('Proportion')+ xlab('Property Name') + theme(strip.text.x =element_text(size=5))

#okay so that is the bar plots, do that for the three years
#also redo 2024 using the sheet brad has fixed up the Marr duplicate issue on


#alpha diversity
data_long_wide<-data_long %>% pivot_wider(names_from = Insect_key_name, values_from = Insect_count)

#here Im setting the replicates as each species and tree within a property (so 6 reps for most properties)
#initally I had this set to merge across all replicates on a site but I think this is better

#this takes into account location, plant_type, time period, and tree number
#but I think time period and tree number are non independent as they are the same tree multiple times a day so they are not independent samples. 
#so how do we combine?
data_long_wide_sumproperty<-data_long_wide %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Timeperiod,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))

#perhaps sum across the plant replicates morning, midday, afternoon? Im making this up no idea if its correct...
data_long_wide_sumproperty<-data_long_wide %>% ungroup() %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Tree_number,sep='_')) %>% select(-Timeperiod,-`Categorical insect grouping`,-Brad_names,-Eddy_names,-Brad_colours,-Property_name,-Native_Plant_Species,-Tree_number) %>% group_by(Property_plant) %>% summarise(across(everything(), sum,na.rm = TRUE))



#diversity plots
data_long_wide_sumproperty<-data_long_wide_sumproperty %>% remove_rownames %>% column_to_rownames(var="Property_plant")

#alpha diversity
#count number of species per property
sppr <- specnumber(data_long_wide_sumproperty)
sppr

#anova against environmental variables, does mean species richness vary among sites?
#not all farms have this plants data:
relavent_farms<-unique(data_long_wide$Property_name)
#relavent_farms<-row.names(data_long_wide_sumproperty)
meta_data_farm_sub<-meta_data_farm %>% filter(Property_name %in% relavent_farms) %>% arrange(Property_name)
#try farm.type
#now need to rebuild up a spreadsheet, string split the farm name
#for spliting via property, plant, tree and TIMEperiod
#sample_data_meta<-data_long_wide %>% select(Property_name,Native_Plant_Species,Timeperiod,Tree_number) %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Timeperiod,Tree_number,sep='_')) %>% unique() %>% select(Property_plant, everything())

#for splitting via property, plant and tree
sample_data_meta<-data_long_wide %>% ungroup()%>% select(Property_name,Native_Plant_Species,Tree_number) %>% mutate(Property_plant=paste(Property_name,Native_Plant_Species,Tree_number,sep='_')) %>% unique() %>% select(Property_plant, everything())
#order so that it matches the sample sheet
sample_data_meta<-left_join(sample_data_meta,meta_data_farm_sub,by='Property_name') %>% arrange(Property_plant)

identical(names(sppr), sample_data_meta$Property_plant)

?aov
sppr_aov <-aov(sppr ~ Farm.type+Native_Plant_Species+Planting.type+Plant.age+Planting.area.size, data = sample_data_meta)
summary(sppr_aov)

#can plot that out
sppr_df <- sppr %>% 
  enframe() %>% 
  full_join(sample_data_meta, by = c("name" = "Property_plant"))

plot_sppr <- ggplot(sppr_df, aes(x = Farm.type, y = value, fill = Farm.type)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Farm type",
       y = "Number of species per site",
       title = "Species richness") +
  facet_wrap(~Native_Plant_Species)
plot_sppr

#brad wants simpson
?diversity
simpdiv <- diversity(data_long_wide_sumproperty,index='simpson')
head(simpdiv)

sppr_aov <-aov(simpdiv ~ Farm.type+Native_Plant_Species+Planting.type+Plant.age+Planting.area.size, data = sample_data_meta)
summary(sppr_aov)

sppr_df <- sppr %>% 
  enframe() %>% 
  full_join(sample_data_meta, by = c("name" = "Property_plant"))

plot_sppr <- ggplot(sppr_df, aes(x = Farm.type, y = value, fill = Farm.type)) +
  geom_boxplot() +
  theme_bw()+
  labs(x = "Farm type",
       y = "Richness Simpson",
       title = "Species richness (Simpson)") +
  facet_wrap(~Native_Plant_Species)
plot_sppr

#beta diversity
perm <- adonis2(data_long_wide_sumproperty ~ Farm.type+Native_Plant_Species+Planting.type+Plant.age+Planting.area.size, data = sample_data_meta)
perm


plantPCA <- rda(data_long_wide_sumproperty)
plantPCA

PCAscores <- scores(plantPCA, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(sample_data_meta, by = c("site" = "Property_plant"))

PCAvect <- scores(plantPCA, display = "species") %>% 
  as.data.frame()


plot_PCA <- ggplot() +
  geom_point(data = PCAscores, aes(x = PC1, y = PC2, color = Native_Plant_Species)) +
  labs(title = "Principal Components Analysis") +
  theme_bw()
plot_PCA

plot_PCA <- ggplot() +
  geom_point(data = PCAscores, aes(x = PC1, y = PC2, color = Farm.type)) +
  labs(title = "Principal Components Analysis") +
  theme_bw()
plot_PCA

plot_PCA <- ggplot() +
  geom_point(data = PCAscores, aes(x = PC1, y = PC2, color = Farm.type, shape=Native_Plant_Species)) +
  labs(title = "Principal Components Analysis") +
  theme_bw()
plot_PCA

plot_PCA <- ggplot() +
  geom_point(data = PCAscores, aes(x = PC1, y = PC2, color = Property_name, shape=Native_Plant_Species)) +
  labs(title = "Principal Components Analysis") +
  theme_bw()
plot_PCA

#MDS
plant_NMDS <- metaMDS(data_long_wide_sumproperty)
plant_NMDS
#stress <0.2
stressplot(plant_NMDS)
#stress is >0.2 with splitting out the replicates but it is a lot of points, would probably drop down if we did one species at a time so maybe not such a big deal?
plot(plant_NMDS)

plot_df <- scores(plant_NMDS, display = "sites") %>% 
  as.data.frame() %>% 
  rownames_to_column("site") %>% 
  full_join(sample_data_meta, by = c("site" = "Property_plant"))
plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Native_Plant_Species, shape = Native_Plant_Species)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS")
plot_nmds

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Farm.type, shape = Farm.type)) +
  geom_point(size = 3, alpha = 0.8) +
  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS")
plot_nmds

plot_nmds <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2, color = Property_name, shape = Native_Plant_Species)) +
  geom_point(size = 3, alpha = 0.8) +
#  stat_ellipse(linetype = 2, size = 1) +
  theme_bw()+
  labs(title = "NMDS")
plot_nmds

fit <- envfit(plant_NMDS, data_long_wide_sumproperty, perm = 999) 

# extract p-values for each species
fit_pvals <- fit$vectors$pvals %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  dplyr::rename("pvals" = ".")

# extract coordinates for species, only keep species with p-val = 0.001
fit_spp <- fit %>% 
  scores(., display = "vectors") %>% 
  as.data.frame() %>% 
  rownames_to_column("species") %>% 
  full_join(., fit_pvals, by = "species") %>% 
  filter(pvals == 0.001)

# new plot
nmds_plot_new <- ggplot(plot_df, aes(x = NMDS1, y = NMDS2)) +
  coord_fixed() +
  geom_point(aes(color = Farm.type, shape = Farm.type), size = 3, alpha = 0.8) +
  stat_ellipse(aes(color = Farm.type)) +
  #scale_color_manual(values = pal) +
  geom_segment(data = fit_spp, aes(x = 0, xend = NMDS1, y = 0, yend = NMDS2),
               arrow = arrow(length = unit(0.25, "cm")),
               col = "black") +
  geom_text(data = fit_spp, aes(label = species)) +
  theme_bw()
nmds_plot_new

#I think this is too confounded with the different species


#having a look at the 2021-2023 spreedsheet
#cant get it in on excel converted to csv and no issue
#csv probably the safest way to get these in judging from the issues I have with them
#insects_2021_2023<-read.csv('Insects visiting native plants Eddy BH202123 final_cp.csv', strip.white=TRUE)
#brad fix duplicate issues
insects_2021_2023<-read.csv('Insects visiting native plants Eddy BH202123 final_sep26.csv', strip.white=TRUE)

insects_2024<-read.csv('Flower visit data Eddy 2024_sep26.csv', strip.white=TRUE)

#going to push them into same dataframe


warnings()

#pull all three species out of dataset
test<-insects_2021_2023 %>% filter(Native_Plant_Species=="Veronica_salicifolia"|Native_Plant_Species=="Kunzea_ericoides") %>% select(Insect_key_name,Date_Text,Native_Plant_Species,Timeperiod,Property_name,Tree_number,Insect_count)

test_2024<-insects_2024 %>% filter(Native_Plant_Species=="Veronica_salicifolia"|Native_Plant_Species=="Kunzea_ericoides") %>% select(Insect_key_name,Date_Text,Native_Plant_Species,Timeperiod,Property_name,Tree_number,Insect_count)

unique(insects_2021_2023$Native_Plant_Species)
unique(insects_2024$Native_Plant_Species)

#brads new taxonomy file
insects_taxonomy<-read_excel('Insect groupings life histories Eddy_cp.xlsx', sheet = 'Taxonomic and life histories',na='NA')

#check if everything maps across
x<-sort(insects_taxonomy$Survey_sheet_name)
y<-sort(unique(test$Insect_key_name))
y<-sort(unique(test_2024$Insect_key_name))
x
y
identical(x,y)
#z<-cbind(x,y)
#as.data.frame(z) %>% filter(x!=y)
#fucken spaces add strip whitespace to read.csv

#need to add a new column which is year
head(test)
#this doesnt work as its not calender year its season
#test<-test %>% mutate(year=str_extract(Date_Text, "[^_]+$"))
#class(test$Date_Text)
#create a new column for season
test<-test %>% mutate(Date = as.Date(Date_Text, format= "%d_%m_%Y")) %>% mutate(season=case_when(Date>='2020-06-01' & Date<='2021-05-31' ~'2020-2021 Season',Date>='2021-06-01' & Date<='2022-05-31' ~'2021-2022 Season', Date>='2022-06-01' & Date<='2023-05-31' ~'2022-2023 Season', Date>='2023-06-01' & Date<='2024-05-31' ~'2023-2024 Season'))
unique(test$season)
test %>% filter(season == '2023-2024 Season')
test %>% filter(Property_name=='Heslop'&Timeperiod=='Morning'&Native_Plant_Species=='Kunzea_ericoides') %>% select(season, Date) %>% unique()
test %>% filter(Property_name=='Reids_Pit'&Timeperiod=='Midday'&Native_Plant_Species=='Kunzea_ericoides') %>% select(season, Date) %>% unique()
test %>% filter(Property_name=='Lochan_Mor'&Timeperiod=='Afternoon'&Native_Plant_Species=='Kunzea_ericoides') %>% select(season, Date) %>% unique()

test_2024<-test_2024 %>% mutate(Date = as.Date(Date_Text, format= "%d_%m_%Y")) %>% mutate(season=case_when(Date>='2020-06-01' & Date<='2021-05-31' ~'2020-2021 Season',Date>='2021-06-01' & Date<='2022-05-31' ~'2021-2022 Season', Date>='2022-06-01' & Date<='2023-05-31' ~'2022-2023 Season', Date>='2023-06-01' & Date<='2024-05-31' ~'2023-2024 Season'))
unique(test_2024$season)
test_2024 %>% filter(season == '2022-2023 Season') %>% select(Property_name, Date_Text) %>% unique()

test[is.na(test$season),]
test_2024[is.na(test_2024$season),]
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

nrow(test_2024)
nrow(data_long_2024)

##################
#just nutting throught the various duplication issues#

#but these arent duplicated so why the difference
duplicates_2024 <- test_2024 %>% select(-Date_Text,-Date) %>% group_by_all() %>% filter(n() > 1) %>%   ungroup()
duplicates_2024 <- test_2024  %>% group_by_all() %>% filter(n() > 1) %>%   ungroup()
#is it nas?
test_2024%>% filter(if_any(everything(), is.na))
x<-test_2024 %>% select(-Date_Text, -Date)
setdiff(x,data_long_2024)
#okay so whats up with these
test_2024 %>% filter(Property_name=='Pareka' & Insect_key_name=='Pollenia fly spp.' & Timeperiod=='Midday' & Native_Plant_Species=='Kunzea_ericoides')
#ah they are not true duplicates as the counts dont match
duplicates_2024 <- test_2024  %>% select(-Insect_count) %>% group_by_all() %>% filter(n() > 1) %>%   ungroup()
write.csv(duplicates_2024,'Duplicated_rows_2024.csv')

#there is duplicates in this set too
test %>% arrange(Property_name,year,Insect_key_name, Tree_number)
duplicates <- test %>% select(-Date_Text,-Date) %>% group_by_all() %>% filter(n() > 1) %>%   ungroup()
duplicates
#just one set of dups left and it doesnt matter as the counts are all summing to 0 anyhow but have let brad know
test %>% filter(Property_name=='Nutpoint' & Insect_key_name=='Globetail hover fly' & Timeperiod=='Midday' & Native_Plant_Species=='Kunzea_ericoides')


duplicates %>% select(-Insect_key_name,-Insect_count,-Tree_number) %>% unique()
#these are now just issues with hover globe fly all the dups have 0 counts so just merging to remove them and it wont impact any of the results

duplicates_2024 <- test_2024 %>% select(-Date_Text,-Date) %>% group_by_all() %>% filter(n() > 1) %>%   ungroup()
duplicates_2024 <- test_2024  %>% group_by_all() %>% filter(n() > 1) %>%   ungroup()
duplicates_2024 %>% select(-Insect_key_name,-Insect_count,-Tree_number) %>% unique()
#duplicates_2024 %>% filter(Property_name=='Pareka')
#duplicates_2024 %>% filter(Property_name=='Terrace_view')
#duplicates_2024 %>% filter(Property_name=='Marr') %>% filter(Timeperiod=='Afternoon') %>% filter(Tree_number==1) %>% filter(Insect_key_name=='Bombus terrestris bee')

#x<-duplicates %>% filter(Property_name=='Ahuriri',Timeperiod=='Morning',season=='2022-2023 Season',Native_Plant_Species=='Veronica_salicifolia')

#x<-duplicates %>% filter(Property_name=='Ward_internal',Timeperiod=='Morning',season=='2022-2023 Season',Native_Plant_Species=='Veronica_salicifolia')

#x<-duplicates %>% filter(Property_name=='Nutpoint',Timeperiod=='Morning',season=='2021-2022 Season',Native_Plant_Species=='Veronica_salicifolia')

#this data is mostly (but not solely 2021-2022 and 2022-2023)
#just having a look at the other two seasons (2020-2021 and 2023-2024)
test %>% filter(season=='2020-2021 Season') %>% select(Property_name,Date) %>% unique()
test %>% filter(season=='2020-2021 Season') %>% select(Property_name,Date_Text,Native_Plant_Species,Timeperiod) %>% unique()
test %>% filter(Property_name=='Pamu'&Native_Plant_Species=='Veronica_salicifolia') %>% select(Property_name,Date_Text,Timeperiod) %>% unique()
#only Pamu
test %>% filter(season=='2023-2024 Season') %>% select(Property_name,Date) %>% unique()
unique(test_2024$Property_name)

#interesting as lochan_mor is in both perhaps its a different tree:
test %>% filter(season=='2023-2024 Season') %>% filter(Property_name=='Lochan_Mor') %>% select(Native_Plant_Species,Date_Text) %>% unique()
test_2024 %>% filter(Property_name=='Lochan_Mor') %>% select(Native_Plant_Species,Date_Text) %>% unique()

test %>% filter(Property_name=='Lochan_Mor') %>% select(Native_Plant_Species,Date_Text) %>% unique()
#I think lochan_mor is a mistake and 31_12_2023 should be 31_12_2022
test %>% filter(Property_name=='Reids_Pit') %>% select(Native_Plant_Species,Date_Text) %>% unique()
test %>% filter(Property_name=='Heslop') %>% select(Native_Plant_Species,Date_Text) %>% unique()


#mostly cleaned as of 25/9/24
#################################################

#going to merge the 2024 and 2021-2023 datasets into one sheet and then can just split the data out as needed

head(test)
head(test_2024)
colnames(test)
colnames(test_2024)
test_allseasons<-rbind(test_2024,test)

data_long_allseasons <- test_allseasons %>% ungroup() %>% select(-Date_Text,-Date) %>% 
  group_by(Property_name, Native_Plant_Species,Timeperiod,Tree_number,Insect_key_name,season) %>%
  summarise(Insect_count = sum(Insect_count))

#so adundance plots using the new variables names
#getting rid of dups by summing them (not sure if this is the way we should deal with them)
#data_long <- test %>% filter(season!='2020-2021 Season') %>% filter(season!='2023-2024 Season' ) %>%  select(-Date_Text,-Date) %>% group_by(Property_name, Native_Plant_Species,Timeperiod,Tree_number,Insect_key_name,season) %>%
#  summarise(Insect_count = sum(Insect_count))

unique(data_long_allseasons$season)

new_names<-insects_taxonomy %>% select(`Categorical insect grouping`,Brad_names,Eddy_names,Brad_colours,Survey_sheet_name)
head(data_long)
#new_names$`Survey sheet name`
data_long_allseasons<-left_join(data_long_allseasons,new_names,by=c('Insect_key_name'='Survey_sheet_name'))
data_long<-left_join(data_long,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

data_long_allseasons$Timeperiod <- factor(data_long_allseasons$Timeperiod, levels=c("Morning", "Midday", "Afternoon"))
data_long$Timeperiod <- factor(data_long$Timeperiod, levels=c("Morning", "Midday", "Afternoon"))

#brad wants thing in a specfic order
data_long$Eddy_names<-factor(data_long$Eddy_names, levels=c('Bee_Honey','Bee_Bumble','Bee_Leioproctus','Bee_Lasioglossum','Bee_Hylaeus','Wasp','Fly_Blow','Fly_Cluster','Fly_Anthomyiid','Fly_Bristle','Fly_Flesh','Fly_House','Fly_Horse','Fly_Stilleto','Fly_March','Fly_Soldier','Fly_Hover','Ladybird Beetle','Butterflies/Moths','Other Insect'))

#can probably do brads names and get away with it its 20 colours
cols<-insects_taxonomy %>% select(Brad_names,Brad_colours,Eddy_names,Brad_colours_bh,Brad_colours_reworked,Survey_sheet_name) %>% unique() %>% arrange(Eddy_names)


#i've made a column that is a new set of names for plotting
ggplot(data_long, aes(x = Timeperiod, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_wrap(~season*Native_Plant_Species) +labs(y= "Insect Counts", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))

ggplot(data_long_allseasons, aes(x = Timeperiod, y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Raw abundances')+scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_wrap(~season*Native_Plant_Species,ncol = 2) +labs(y= "Insect Counts", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))


#
ggplot(data_long, aes(x = factor(Native_Plant_Species), y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_grid(~Property_name*season,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Raw abundances')+ylab('Insect Count') + xlab('Native Plant') + theme(strip.text.x =element_text(size=4))+ guides(fill=guide_legend(title="Taxon"))
#way too busy

ggplot(data_long, aes(x = factor(Property_name), y = Insect_count, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours_reworked,Eddy_names)))+ facet_grid(~Native_Plant_Species*season,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Raw abundances')+ylab('Insect Count') + xlab('Property') + theme(strip.text.x =element_text(size=8))+ guides(fill=guide_legend(title="Taxon"))

#is too much having morning/midday/night in the same graph
#but can do that for each year think that is okay for raw abundances

#relative abundances
data_long_ra<-data_long %>% ungroup() %>%  select(-Tree_number) %>% 
  group_by(Native_Plant_Species,season,Timeperiod,Insect_key_name) %>%
  summarise(Insect_count =  sum(Insect_count)) %>%  mutate(per =  100 * (Insect_count/sum(Insect_count))) %>% 
  ungroup()

data_long_ra<-left_join(data_long_ra,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

#time period across all sites
ggplot(data_long_ra, aes(x = Timeperiod, y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +ggtitle('Proportional abundance')+scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~season*Native_Plant_Species)+ theme(strip.text.x =element_text(size=6))+labs(y= "Percentage", x = "Time Period") + guides(fill=guide_legend(title="Taxon"))
  
#by location no time period
data_long_ra<-data_long %>% ungroup() %>%  select(-Tree_number) %>% 
  group_by(Native_Plant_Species,season,Property_name,Insect_key_name) %>%
  summarise(Insect_count =  sum(Insect_count))%>%  mutate(per =  100 *Insect_count/sum(Insect_count)) %>% 
  ungroup()

data_long_ra<-left_join(data_long_ra,new_names,by=c('Insect_key_name'='Survey_sheet_name'))

#edit to remove blanks
#ggplot(data_long_ra, aes(x = Property_name, y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Native_Plant_Species*season) +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Proportional abundance')+ylab('Proportion')+ guides(fill=guide_legend(title="Taxon"))

ggplot(data_long_ra, aes(x = factor(Property_name), y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Native_Plant_Species*season,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Proportional abundance')+ylab('Proportion') + xlab('Native Plant') + theme(strip.text.x =element_text(size=4))+ guides(fill=guide_legend(title="Taxon"))

#too busy
ggplot(data_long_ra, aes(x = factor(Native_Plant_Species), y = per, fill = Eddy_names)) + geom_bar(stat = "identity")+ scale_fill_manual(values=with(cols,setNames(Brad_colours,Eddy_names)))+ facet_grid(~Property_name*season,scales='free',space='free') +theme_bw()+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+ggtitle('Proportional abundance')+ylab('Proportion')+ xlab('Property Name') + theme(strip.text.x =element_text(size=5))+ guides(fill=guide_legend(title="Taxon"))

#okay so that is the bar plots, do that for the three years
#also redo 2024 using the sheet brad has fixed up the Marr duplicate issue on



############################
############################



#figuring out the NA issue

#why is there NA
#first issue was drone fly being spelt two ways now fixed in main sheet
#data_long %>% filter(Insect_key_name=='Drone Fly')
#I see Drone fly is spelt Drone Fly and Drone fly in the original sheet have edited above to fix this

x<-data_long_wide %>% filter(if_any(everything(), is.na))
#Garrickfeild has an issue with Common Copper Butterfly
#Rakaia Gorge has an issue with Other Tabanid fly spp
#have a look at it
#x<-insects_2024 %>% filter(Property_name=='Garrickfield') 
#unique(x$Insect_key_name)
#insects_2024 %>% filter(Property_name=='Garrickfield') %>% filter(Insect_key_name=='Common Copper Butterfly')
#test  %>% filter(Property_name=='Garrickfield') %>% filter(Insect_key_name=='Common Copper Butterfly')
#ah this one I'll have to check with Brad but it looks like the afternoon tree number 6 common copper butterfly count has been mislabeled as tree number 3 so there will be a NA added in when R tries to sum across. So for now Im going to edit that to be tree number 6 rather than tree number 3
#test  %>% filter(Property_name=='Garrickfield') %>% filter(Insect_key_name=='Common Copper Butterfly')

#Insect_key_name         Timeperiod Property_name Tree_number Insect_count
#<chr>                   <chr>      <chr>               <dbl>        <dbl>
#  1 Common Copper Butterfly Morning    Garrickfield            1            0
#2 Common Copper Butterfly Morning    Garrickfield            2            0
#3 Common Copper Butterfly Morning    Garrickfield            3            0
#4 Common Copper Butterfly Morning    Garrickfield            4            0
#5 Common Copper Butterfly Morning    Garrickfield            5            0
#6 Common Copper Butterfly Morning    Garrickfield            6            0
#7 Common Copper Butterfly Afternoon  Garrickfield            1            0
#8 Common Copper Butterfly Afternoon  Garrickfield            2            0
#9 Common Copper Butterfly Afternoon  Garrickfield            3            0
#10 Common Copper Butterfly Afternoon  Garrickfield            4            0
#11 Common Copper Butterfly Afternoon  Garrickfield            5            0
#12 Common Copper Butterfly Afternoon  Garrickfield            3            1 ###########this should be tree_number 6 I think!
#13 Common Copper Butterfly Midday     Garrickfield            1            0
#14 Common Copper Butterfly Midday     Garrickfield            2            0
#15 Common Copper Butterfly Midday     Garrickfield            3            0
#16 Common Copper Butterfly Midday     Garrickfield            4            0
#17 Common Copper Butterfly Midday     Garrickfield            5            0
#18 Common Copper Butterfly Midday     Garrickfield            6            0
#row 6013 in brads excell spreedsheet

#issue with Rakaia_Gorge
#x<-insects_2024 %>% filter(Property_name=='Rakaia_Gorge') 
#unique(x$Insect_key_name)
#x<-test %>% filter(Insect_key_name=='Other Tabanid fly spp.')
#unique(x$Property_name)
#test %>% filter(Insect_key_name=='Other Tabanid fly spp.') %>% filter(Property_name=='Rakaia_Gorge')
#missing counts for 'Other Tabanid fly spp.' in tree 1,3,4 for midday sample
#to deal with this Im going to add a ignore na into the summarise function but will let brad know the issues. 





