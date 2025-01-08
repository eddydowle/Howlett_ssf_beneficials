#analysis of flower visitation data from brad
#this is the data with flower visited and time spent on each flower

library(readxl)
library(tidyverse)

#
setwd('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024/')

pak_choi<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Pak choi")
pak_choi$`Predicted stigmas/hour`<-NULL
radish<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Radish")
onion<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Onion")
carrot<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Carrot")
avocado<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Avocado")
kiwifruit<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Kiwifruit")
apple<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Apple")
pear<-read_excel('Copy of Simplified Behaviour data for Eddy different crops v3.xlsx', sheet = "Pear")

#box plot of average time per infloresence

#merging into one datasheet
colnames(pak_choi)
colnames(radish)
colnames(onion)
onion$`Full scientific name`<-NULL
head(onion)
names(onion)[names(onion) == 'Full_scientific_name'] <- 'Full scientific name'
colnames(carrot)
names(carrot)[names(carrot) == 'Full_Scientific_Name'] <- 'Full scientific name'
names(carrot)[names(carrot) == 'Inflorescence No.'] <- 'inflorescence'
names(carrot)[names(carrot) == 'No. Umblets visited in inflorescence'] <- 'stigmas/inflorescence contacted'
names(carrot)[names(carrot) == 'Time on umbel'] <- 'Time on inflorescence (sec)'
colnames(avocado)
names(avocado)[names(avocado) == 'Full_Scientific_Name'] <- 'Full scientific name'
names(avocado)[names(avocado) == 'Flower No.'] <- 'inflorescence'
names(avocado)[names(avocado) == 'Time on flower'] <- 'Time on inflorescence (sec)'
names(avocado)[names(avocado) == 'Tree No.'] <- 'Tree number'
colnames(kiwifruit)
names(kiwifruit)[names(kiwifruit) == 'Full_scientific_name'] <- 'Full scientific name'
names(kiwifruit)[names(kiwifruit) == 'Individual_bee_id'] <- 'Individual bee id'
names(kiwifruit)[names(kiwifruit) == 'Flower No.'] <- 'inflorescence'
names(kiwifruit)[names(kiwifruit) == 'Time on flower'] <- 'Time on inflorescence (sec)'
colnames(apple)
names(apple)[names(apple) == 'Full_scientific_name'] <- 'Full scientific name'
names(apple)[names(apple) ==  'Flower_number'] <- 'inflorescence'
names(apple)[names(apple) ==  'Stigma_touch'] <- 'stigmas/inflorescence contacted'
names(apple)[names(apple) == 'Time_on_flower'] <- 'Time on inflorescence (sec)'
names(apple)[names(apple) == 'Individual_bee_id'] <- 'Individual bee id'
names(apple)[names(apple) == 'Tree_number'] <- 'Tree number'
colnames(pear)
names(pear)[names(pear) == 'Stigma touch'] <- 'stigmas/inflorescence contacted'
names(pear)[names(pear) == 'Time on flower'] <- 'Time on inflorescence (sec)'
names(pear)[names(pear) ==  'Flower number'] <- 'inflorescence'

#species name check
unique(pak_choi$`Full scientific name`)

#bind it up into a single dataframe
crops_together<-bind_rows(pak_choi,radish)
crops_together<-bind_rows(crops_together,onion)
crops_together<-bind_rows(crops_together,carrot)
crops_together<-bind_rows(crops_together,avocado)
crops_together<-bind_rows(crops_together,kiwifruit)
crops_together<-bind_rows(crops_together,apple)
crops_together<-bind_rows(crops_together,pear)

#im a little worried that bee number could be repeated between crops so creating a new column that is beenumber_crop
crops_together<-crops_together %>% mutate(Individual_bee_id_eddy=paste(Crop,`Individual bee id`))

#fix names to cover different flower naming
#inforesence/flower
#stigmas/umblets/inflorescence contacted
#Time on inflorescence/umbel/flower (sec) 
colnames(crops_together)
names(crops_together)[names(crops_together) == 'inflorescence'] <- 'Inflorescence/Flower'
names(crops_together)[names(crops_together) == 'stigmas/inflorescence contacted'] <- 'Stigmas/Umblets/Inflorescence contacted'
names(crops_together)[names(crops_together) ==  'Time on inflorescence (sec)'] <- 'Time on inflorescence/umbel/flower (sec)'

#species name check
#going back to same naming used in previous boxplot
unique(crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("\\.", "", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("\\*", "", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Leioprotus spp", "Leioproctus spp",crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Apis_mellifera", "Apis mellifera", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Lasioglossum_spp", "Lasioglossum spp", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Leioproctus_spp", "Leioproctus spp", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Bombus_terrestris", "Bombus terrestris", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Leioproctus huakiwi", "Leioproctus spp", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Leioproctus huakiwi", "Leioproctus spp", crops_together$`Full scientific name`)
crops_together$`Full scientific name` <- gsub("Bombus ruderatus", "Bombus hortorum/ruderatus", crops_together$`Full scientific name`)
crops_together$Crop <- gsub("Apple_", "Apple", crops_together$Crop)
#drop rows that have no time recorded
#brad said to just drop them as they are bung datapoints
crops_together<-crops_together %>% filter(!is.na(`Time on inflorescence/umbel/flower (sec)`))

species<-c('Bombus terrestris','Apis mellifera','Lasioglossum spp','Leioproctus spp','Leioproctus fulvescens','Bombus hortorum/ruderatus')
#species<-c('Bombus terrestris','Apis mellifera','Lasioglossum spp','Leioproctus spp','Leioproctus fulvescens','Bombus ruderatus')
species_col<-c('gold3','gold','firebrick1','firebrick','firebrick3','darkgoldenrod2')
brads_col<-data.frame(species,species_col) %>% arrange(species)

colnames(crops_together)
#now I think we can do it two ways

#first we can just boxplot it as time spent on flower by species/crop
ggplot(crops_together,aes(x=Crop,y=`Time on inflorescence/umbel/flower (sec)`,fill=`Full scientific name`))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  #scale_fill_manual(values=brads_col$species_col,labels=brads_col$species)
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Time on inflorescence/umbel/flower (sec)") + guides(fill=guide_legend(title="Species"))

#sqrt axis
ggplot(crops_together,aes(x=Crop,y=`Time on inflorescence/umbel/flower (sec)`,fill=`Full scientific name`))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  #scale_fill_manual(values=brads_col$species_col,labels=brads_col$species)
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  scale_y_sqrt()+
  labs(y= "Time on inflorescence/umbel/flower (sec)") + guides(fill=guide_legend(title="Species"))

#log(x+1)
library(scales)
ggplot(crops_together,aes(x=Crop,y=`Time on inflorescence/umbel/flower (sec)`,fill=`Full scientific name`))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Time on inflorescence/umbel/flower (sec)") + guides(fill=guide_legend(title="Species"))+
coord_trans(y='log1p')

#secondly we can average the time spent by individual bee and then box plot average time spent on flower by species/crop

crops_together_averagetime<-crops_together %>% group_by(Crop,`Full scientific name`,Individual_bee_id_eddy) %>% summarise(AverageTimeFlower=mean(`Time on inflorescence/umbel/flower (sec)`)) 

crops_together_averagetime%>% 
  ggplot(aes(x=Crop,y=AverageTimeFlower,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Average time on inflorescence/umbel/flower (sec)") + guides(fill=guide_legend(title="Species"))+
  coord_trans(y='log1p')

#working out standardised number of flower visited

#choosing an arbitary 60 seconds
crops_together_inflorescenceMin<-crops_together %>% group_by(Crop,`Full scientific name`,Individual_bee_id_eddy) %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n()) %>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff)

crops_together_inflorescenceMin%>% 
  ggplot(aes(x=Crop,y=inflorescencePerMin,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Estimated inflorescence/umbel/flower visits per minute") + guides(fill=guide_legend(title="Species"))+
  coord_trans(y='log1p')

#table for brad

#number of individuals #average duration followed #average visits per minute #% visits involving stigma contact

#for stigma turn into P/A and then just sum and turn that into proportion
crops_together_summarytable<-  crops_together %>% mutate(Stigmas_contacted_PA=case_when(`Stigmas/Umblets/Inflorescence contacted` > 0~1,`Stigmas/Umblets/Inflorescence contacted` == 0~0)) %>% group_by(Crop,`Full scientific name`,Individual_bee_id_eddy)  %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n(),CountVisitsSuccessStigma=sum(Stigmas_contacted_PA),SumStigmavisited=sum(`Stigmas/Umblets/Inflorescence contacted`))%>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff) %>% mutate(prop_visit_touch_stigma=CountVisitsSuccessStigma/Countinfloresence) %>% mutate(ave_num_stigmatouches_per_inflorsense=SumStigmavisited/Countinfloresence)


summary_table<-crops_together_summarytable %>% select(-Individual_bee_id_eddy,-time_diff) %>% group_by(Crop,`Full scientific name`) %>% summarise_all(mean)

summary_table_count<-crops_together_summarytable %>% select(-Individual_bee_id_eddy,-time_diff) %>% group_by(Crop,`Full scientific name`) %>% summarise(count_observations=n())

summary_table<-full_join(summary_table_count,summary_table)

write.csv(summary_table,'Summary_table_inflorescence_visits.csv',quote=F,row.names = F)


#doing some basic stats
################################################################################################
###############differences in the estimated inflorsence/umbel/flower visits per minute##########
################################################################################################

#Brad things average time on inflorescence is the right 
#test for normal distributions 
crops_together_inflorescenceMin$inflorescencePerMin
ggplot(crops_together_inflorescenceMin, aes(x=inflorescencePerMin)) + 
  geom_histogram()

shapiro.test(crops_together_inflorescenceMin$inflorescencePerMin)
#Shapiro-Wilk normality test
#data:  crops_together_inflorescenceMin$inflorescencePerMin
#W = 0.8808, p-value < 2.2e-16

data_split <-crops_together_inflorescenceMin %>% filter(Crop=='Carrot') 
data_split <-split(data_split$inflorescencePerMin, data_split$`Full scientific name`)
lapply(data_split, shapiro.test)

crops_together_inflorescenceMin %>% filter(Crop=='Carrot') %>% ggplot(aes(x=inflorescencePerMin)) + 
  geom_histogram()+
  facet_wrap(~`Full scientific name`)


#right so none parametric tests again
#most have >2 variables so back to kruskal-wallis and dunn.tests

library(FSA)
#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops<-unique(crops_together_inflorescenceMin$Crop)
for (item in crops) {
  print(item)
  subset_crop<-crops_together_inflorescenceMin %>% filter(Crop==item)
  result_kruskal<-kruskal.test(inflorescencePerMin ~`Full scientific name`,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(inflorescencePerMin ~`Full scientific name`,data=subset_crop,method='bonferroni')
  test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
  colnames(test_kruskal)[1] <- paste0(item,'_KruskalWallis')
  test_kruskal <- tibble::rownames_to_column(test_kruskal, "Stats")
  print(test_kruskal)
  dunn_table<-result_dunn$res
  dunn_table$P.unadj<-NULL
  names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(item,'_Z')
  names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(item,'_P.adj')
  print(dunn_table)
  if (is.null(kruskal_table_out)){
    kruskal_table_out<-test_kruskal
  }
  else{
    kruskal_table_out<-full_join(kruskal_table_out,test_kruskal)
  }
  if (is.null(dunn_table_out)){
    dunn_table_out<-dunn_table
  }
  else{
    dunn_table_out<-full_join(dunn_table_out,dunn_table,by='Comparison')
  }
  
}

write.table(dunn_table_out,'Dunn_analysis_Dec2024_inflorescenceVisitPerMin.csv',sep=',',quote=F,row.names = F)
write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_inflorescenceVisitPerMin.csv',sep=',',quote=F,row.names = F)


##########################################################################
############Average time on infloresence/umbel/flower (sec)###############
##########################################################################



#Brad things average time on inflorescence is the right 
#test for normal distributions 
crops_together_averagetime$AverageTimeFlower
ggplot(crops_together_averagetime, aes(x=AverageTimeFlower)) + 
  geom_histogram()

shapiro.test(crops_together_averagetime$AverageTimeFlower)
#Shapiro-Wilk normality test
#data:  crops_together_inflorescenceMin$inflorescencePerMin
#W = 0.8808, p-value < 2.2e-16

data_split <-crops_together_averagetime %>% filter(Crop=='Carrot') 
data_split <-split(data_split$AverageTimeFlower, data_split$`Full scientific name`)
lapply(data_split, shapiro.test)

crops_together_averagetime %>% filter(Crop=='Kiwifruit') %>% ggplot(aes(x=AverageTimeFlower)) + 
  geom_histogram()+
  facet_wrap(~`Full scientific name`)

#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops<-unique(crops_together_averagetime$Crop)
for (item in crops) {
  print(item)
  subset_crop<-crops_together_averagetime %>% filter(Crop==item)
  result_kruskal<-kruskal.test(AverageTimeFlower ~`Full scientific name`,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(AverageTimeFlower ~`Full scientific name`,data=subset_crop,method='bonferroni')
  test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
  colnames(test_kruskal)[1] <- paste0(item,'_KruskalWallis')
  test_kruskal <- tibble::rownames_to_column(test_kruskal, "Stats")
  print(test_kruskal)
  dunn_table<-result_dunn$res
  dunn_table$P.unadj<-NULL
  names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(item,'_Z')
  names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(item,'_P.adj')
  print(dunn_table)
  if (is.null(kruskal_table_out)){
    kruskal_table_out<-test_kruskal
  }
  else{
    kruskal_table_out<-full_join(kruskal_table_out,test_kruskal)
  }
  if (is.null(dunn_table_out)){
    dunn_table_out<-dunn_table
  }
  else{
    dunn_table_out<-full_join(dunn_table_out,dunn_table,by='Comparison')
  }
  
}

write.table(dunn_table_out,'Dunn_analysis_Dec2024_AverageTimeFlower.csv',sep=',',quote=F,row.names = F)
write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_AverageTimeFlower.csv',sep=',',quote=F,row.names = F)

########################################################
########analysis of stigma times########################
########################################################


head(crops_together_summarytable)

crops_together_summarytable$prop_visit_touch_stigma
crops_together_summarytable$ave_num_stigmatouches_per_inflorsense

crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% 
  ggplot(aes(x=Crop,y=prop_visit_touch_stigma,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Proportion of visits with stigma touch") + guides(fill=guide_legend(title="Species"))

crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Average number stigma touches per inflorescence") + guides(fill=guide_legend(title="Species"))



#Brad things average time on inflorescence is the right 
#test for normal distributions 
crops_together_summarytable$ave_num_stigmatouches_per_inflorsense
ggplot(crops_together_summarytable, aes(x=ave_num_stigmatouches_per_inflorsense)) + 
  geom_histogram()

shapiro.test(crops_together_summarytable$ave_num_stigmatouches_per_inflorsense)
#Shapiro-Wilk normality test
#data:  crops_together_inflorescenceMin$inflorescencePerMin
#W = 0.8808, p-value < 2.2e-16

data_split <-crops_together_summarytable %>% filter(Crop=='Carrot') 
data_split <-split(data_split$ave_num_stigmatouches_per_inflorsense, data_split$`Full scientific name`)
lapply(data_split, shapiro.test)

crops_together_summarytable %>% filter(Crop=='') %>% ggplot(aes(x=ave_num_stigmatouches_per_inflorsense)) + geom_histogram()+
  facet_wrap(~`Full scientific name`)


#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops_together_summarytable_stigma<-crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit')
crops<-unique(crops_together_summarytable_stigma$Crop)
for (item in crops) {
  print(item)
  subset_crop<-crops_together_summarytable_stigma %>% filter(Crop==item)
  result_kruskal<-kruskal.test(ave_num_stigmatouches_per_inflorsense ~`Full scientific name`,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(ave_num_stigmatouches_per_inflorsense ~`Full scientific name`,data=subset_crop,method='bonferroni')
  test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
  colnames(test_kruskal)[1] <- paste0(item,'_KruskalWallis')
  test_kruskal <- tibble::rownames_to_column(test_kruskal, "Stats")
  print(test_kruskal)
  dunn_table<-result_dunn$res
  dunn_table$P.unadj<-NULL
  names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(item,'_Z')
  names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(item,'_P.adj')
  print(dunn_table)
  if (is.null(kruskal_table_out)){
    kruskal_table_out<-test_kruskal
  }
  else{
    kruskal_table_out<-full_join(kruskal_table_out,test_kruskal)
  }
  if (is.null(dunn_table_out)){
    dunn_table_out<-dunn_table
  }
  else{
    dunn_table_out<-full_join(dunn_table_out,dunn_table,by='Comparison')
  }
  
}

write.table(dunn_table_out,'Dunn_analysis_Dec2024_averageNumStigTouchPerInflore.csv',sep=',',quote=F,row.names = F)
write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_averageNumStigTouchPerInflore.csv',sep=',',quote=F,row.names = F)

