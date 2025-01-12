install.packages("BeeBDC")
library(BeeBDC)
#didnt work on PC so downloaded on Mac and saved to disk
#beesTaxonomy <- beesTaxonomy()
#check_bee<-beesChecklist()
bee_check<-read.table('Bee_checklist_download8Jan2024.txt',header=T,sep='\t',row.names = NULL,quote='')
bee_tax<-read.table('Bee_taxonomy_download8Jan2024.txt',header=T,sep='\t',row.names = NULL,quote='')
land_mass_by_country<-read.csv('Landmassbycountry.csv',header=T,quote='')

head(bee_check)
test<-bee_check %>% filter(shortName=='New Zealand')


head(bee_tax)
bee_tax %>% filter(canonical=='Leioproctus pango')

countries_summarised<-bee_check %>% group_by(rNaturalEarth_name) %>% summarise(unique_species = n_distinct(species), unique_families = n_distinct(family), unique_genus = n_distinct(genus),unique_subfamilies = n_distinct(subfamily))

head(land_mass_by_country)
land_mass_by_country$Country
countries_summarised<-as.data.frame(countries_summarised)

#not all match
countries_summarised_landmass<-full_join(countries_summarised,land_mass_by_country,by=c('rNaturalEarth_name'='Country_mod'))

#find none matches

countries_summarised_landmass_nomatch<-anti_join(countries_summarised,land_mass_by_country,by=c('rNaturalEarth_name'='Country_mod'))

#Hmmmm turns out estimating landmass by country is hard
#what parts do you include (islands etc) we are probably more intrested in the contiguous landmassess of countries
#but then the bee lists are by country so it should be country total?
#switching to data from here https://www.nationsonline.org/oneworld/countries_by_area.htm
#as at least then we can be clear about what landmassess are included in the estimate

land_mass_by_country2<-read.csv('landarea_bycountry_nationsoline.csv',header=T,quote='')

#not all match
countries_summarised_landmass<-full_join(countries_summarised,land_mass_by_country2,by=c('rNaturalEarth_name'='Country_mod'))

#find none matches

countries_summarised_landmass_nomatch<-anti_join(countries_summarised,land_mass_by_country2,by=c('rNaturalEarth_name'='Country_mod'))

#bunch that dont match but fairly minor ones

countries_summarised_landmass_nzishsize<-countries_summarised_landmass %>% filter(Area_km2 %in% (100000:400000)|rNaturalEarth_name=='Fiji'|rNaturalEarth_name=='Taiwan'|rNaturalEarth_name=='Sri Lanka'|rNaturalEarth_name=='Mauritius'|rNaturalEarth_name=='Uruguay'|rNaturalEarth_name=='Ireland'|rNaturalEarth_name=='Iceland') %>% filter(!is.na(unique_species))


ggplot(countries_summarised_landmass_nzishsize,aes(Area_km2,unique_species,label=rNaturalEarth_name))+
  geom_point(aes(size=unique_families,colour=unique_genus))+
  theme_bw()+
  scale_colour_gradient()+
  geom_text()

#goingto drop a western sahara and that d=lvoire

countries_summarised_landmass_nzishsize$rNaturalEarth_name

countries_summarised_landmass_nzishsize<-countries_summarised_landmass_nzishsize %>% filter(rNaturalEarth_name!="Western Sahara") %>% filter(rNaturalEarth_name!='Guinea')%>% filter(rNaturalEarth_name!="Côte d'Ivoire")%>% filter(rNaturalEarth_name!="Burkina Faso")%>% filter(rNaturalEarth_name!="Ghana")%>% filter(rNaturalEarth_name!="Benin")%>% filter(rNaturalEarth_name!="Eritea")%>% filter(rNaturalEarth_name!="Bangladesh")

library(ggrepel)

ggplot(countries_summarised_landmass_nzishsize,aes(Area_km2,unique_species))+
  geom_point(aes(size=unique_families,colour=unique_genus))+
  theme_bw()+
  scale_colour_gradient()+
  geom_label_repel(aes(label=rNaturalEarth_name),nudge_x=0.5, box.padding=0.35,point.padding=0.5,force=1)+
  labs(x='Country km2',y='Number Unique Species',colour='Unique Genera', size='Unique Families')


write.csv(countries_summarised_landmass,'Bee_counts_per_country_landmass.csv',row.names=F)

#have given to brad and he is going to try and add a few specific ones in (e.g. tasmania etc)


bee_data_country<-read.csv('Bee_counts_per_country_landmass.csv',row.names=NULL,header=T)
library(countrycode)
countrycode('Albania',origin='country.name',destination = 'iso3c')


bee_data_country<-bee_data_country %>% mutate(country_code=countrycode(rNaturalEarth_name,origin='country.name',destination = 'iso3c'))


countries_summarised_landmass_nzishsize<-bee_data_country %>% filter(Area_km2 %in% (100000:400000)|rNaturalEarth_name=='Fiji'|rNaturalEarth_name=='Taiwan'|rNaturalEarth_name=='Sri Lanka'|rNaturalEarth_name=='Mauritius'|rNaturalEarth_name=='Uruguay'|rNaturalEarth_name=='Ireland'|rNaturalEarth_name=='Iceland') %>% filter(!is.na(unique_species))


countries_summarised_landmass_nzishsize<-bee_data_country %>% filter(Area_km2 %in% (100000:400000)|rNaturalEarth_name=='Fiji'|rNaturalEarth_name=='Taiwan'|rNaturalEarth_name=='Sri Lanka'|rNaturalEarth_name=='Mauritius'|rNaturalEarth_name=='Uruguay'|rNaturalEarth_name=='Ireland'|rNaturalEarth_name=='Iceland') %>% filter(!is.na(unique_species))


countries_summarised_landmass_nzishsize<-countries_summarised_landmass_nzishsize %>% filter(rNaturalEarth_name!="Western Sahara") %>% filter(rNaturalEarth_name!='Guinea')%>% filter(rNaturalEarth_name!="Côte d'Ivoire")%>% filter(rNaturalEarth_name!="Burkina Faso")%>% filter(rNaturalEarth_name!="Ghana")%>% filter(rNaturalEarth_name!="Benin")%>% filter(rNaturalEarth_name!="Eritea")%>% filter(rNaturalEarth_name!="Bangladesh")

library(ggrepel)


ggplot(countries_summarised_landmass_nzishsize,aes(Area_km2,unique_species))+
  geom_point(aes(size=unique_families,colour=unique_genus))+
  theme_bw()+
  scale_colour_gradient()+
  geom_label_repel(aes(label=country_code),nudge_x=0.5, box.padding=0.35,point.padding=0.5,force=1)+
  labs(x='Country km2',y='Number Unique Species',colour='Unique Genera', size='Unique Families')

ggplot(countries_summarised_landmass_nzishsize,aes(Area_km2,unique_species))+
  geom_point(aes(size=unique_families,colour=unique_genus))+
  theme_bw()+
  scale_colour_gradient()+
  geom_label_repel(aes(label=country_code),nudge_x=0.5, box.padding=0.35,point.padding=0.5,force=1,colour=countries_summarised_landmass_nzishsize$Name_colour,size=7)+
  labs(x='Country km2',y='Number Unique Species',colour='Unique Genera', size='Unique Families')




