#Eddy Dowle
#13/1/2025
#rates of movement and stigma contacted analyses


library(readxl)
library(tidyverse)
library(scales)
library(FSA)
#
setwd('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024/')
#cleaned data of behaviour (much data wrangling to get it cleaned)
crops_together_bee<-read.csv('Behaviour_data_different_crops_eddyJan2025.csv',quote="",row.names = NULL,header=T, check.names = FALSE)
colnames(crops_together_bee)

crops_together<-read.csv('Copy of Insect behaviour Data final.csv',quote="",row.names = NULL,header=T, check.names = FALSE)
colnames(crops_together)

#give each insect a unique name
#crop_fullsciename_insectno = individual_id_eddy

crops_together<-crops_together %>% mutate(Individual_id_eddy=paste0(Crop,"-",`Full scientific name`,"_",`Insect no.`))

names(crops_together)[names(crops_together) == 'stigmas flowers/inflorescence contacted'] <- 'Stigmas/Umblets/Inflorescence contacted'
names(crops_together)[names(crops_together) == 'Time on inflorescence/flower'] <- 'Time on inflorescence/umbel/flower (sec)'
names(crops_together)[names(crops_together) == 'Inflorescence'] <- 'Inflorescence/Flower'

colnames(crops_together_bee)
colnames(crops_together)


#summary table
#number of individuals 
#ave average duration followed 
#ave count flower unit (refered to as infloresence here just for brevity)
#ave visits successful stigma contact
#ave stigma visited
#ave flower unit visited per minute
#ave % visits involving stigma contact
#ave stigma contacts per flower unit

#generating rate
#for stigma turn into P/A and then just sum and turn that into proportion
crops_together_summarytable<-  crops_together %>% mutate(Stigmas_contacted_PA=case_when(`Stigmas/Umblets/Inflorescence contacted` > 0~1,`Stigmas/Umblets/Inflorescence contacted` == 0~0)) %>% group_by(Crop,`Full scientific name`,Individual_id_eddy)  %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n(),CountVisitsSuccessStigma=sum(Stigmas_contacted_PA),SumStigmavisited=sum(`Stigmas/Umblets/Inflorescence contacted`))%>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff) %>% mutate(prop_visit_touch_stigma=CountVisitsSuccessStigma/Countinfloresence) %>% mutate(ave_num_stigmatouches_per_inflorsense=SumStigmavisited/Countinfloresence)

summary_table<-crops_together_summarytable %>% select(-Individual_id_eddy,-time_diff) %>% group_by(Crop,`Full scientific name`) %>% summarise_all(mean)

summary_table_count<-crops_together_summarytable %>% select(-Individual_id_eddy,-time_diff) %>% group_by(Crop,`Full scientific name`) %>% summarise(count_observations=n())

summary_table<-full_join(summary_table_count,summary_table)

#write.csv(summary_table,'Summary_table_inflorescence_visits_flies.csv',quote=F,row.names = F)

###############################
#analysing rates of movements##
###############################

species<-c('Bombus terrestris','Apis mellifera','Lasioglossum spp','Leioproctus spp','Leioproctus fulvescens','Bombus hortorum/ruderatus')
#species<-c('Bombus terrestris','Apis mellifera','Lasioglossum spp','Leioproctus spp','Leioproctus fulvescens','Bombus ruderatus')
species_col<-c('gold3','gold','firebrick1','firebrick','firebrick3','darkgoldenrod2')
brads_col<-data.frame(species,species_col) %>% arrange(species)

colnames(crops_together)

#plot with brads colour scheme
library(readxl)
#unique(deposition_data_all$Bee.species)
brads_col_2<-read_excel('Copy of Insect behaviour Data final.xlsx',sheet='Insect Colours')
head(brads_col_2)

brads_col_2$`Full scientific name`
unique(crops_together$`Full scientific name`)


colours_figs<-crops_together %>% select(`Full scientific name`) %>% unique() %>% left_join(.,brads_col_2,by=c('Full scientific name'='Full scientific name'))
brads_col<-colours_figs %>% arrange(`Insect ordering for figs`)
brads_col$species<-brads_col$`Full scientific name`

#relevel to brads order
brads_col$species <- factor(brads_col$species, levels=brads_col$species[order(brads_col$`Insect ordering for figs`)], ordered=TRUE)
#brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)
levels(brads_col$species)



#Standardising number of flower visited per 60 seconds (one minute)
crops_together_inflorescenceMin<-crops_together %>% group_by(Crop,`Full scientific name`,Individual_id_eddy) %>% summarise(SumTimeFlower=sum(`Time on inflorescence/umbel/flower (sec)`),Countinfloresence=n()) %>% mutate(time_diff=60/SumTimeFlower) %>% mutate(inflorescencePerMin=Countinfloresence*time_diff)

#relevel to brads order
crops_together_inflorescenceMin$`Full scientific name` <- forcats::fct_relevel(crops_together_inflorescenceMin$`Full scientific name` ,levels(brads_col$species))
levels(crops_together_inflorescenceMin$`Full scientific name` )

crops_together_inflorescenceMin%>% 
  ggplot(aes(x=Crop,y=inflorescencePerMin,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  #scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Estimated inflorescence/umbel/flower visits per minute") + guides(fill=guide_legend(title="Species"))+
  coord_trans(y='log1p')

#plus dots
crops_together_inflorescenceMin%>% 
  ggplot(aes(x=Crop,y=inflorescencePerMin,fill=`Full scientific name`))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  labs(y= "Estimated inflorescence/umbel/flower visits per minute") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')


#Shapiro-Wilk normality test
data_split <-crops_together_inflorescenceMin %>% filter(Crop=='Apple') 
data_split <-split(data_split$inflorescencePerMin, data_split$`Full scientific name`)
lapply(data_split, shapiro.test)
#most significant so not normal

#right so none parametric tests again
#most have >2 variables so back to kruskal-wallis and dunn.tests
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

#write.table(dunn_table_out,'Dunn_analysis_Dec2024_inflorescenceVisitPerMin_flies.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_inflorescenceVisitPerMin_flies.csv',sep=',',quote=F,row.names = F)


#############################
#proportion stigma contacted#
#############################

#relevel to brads order
head(crops_together_summarytable)
crops_together_summarytable$`Full scientific name` <- forcats::fct_relevel(crops_together_summarytable$`Full scientific name` ,levels(brads_col$species))
levels(crops_together_summarytable$`Full scientific name` )
#drop avocado, kiwifruit, 
#brad will drop carrot as the data isnt actually for stigma but for umblet or something (flowers isnt really my jam)

#boxplot
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))


#boxplot with dots
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + 
  geom_boxplot(outliers = FALSE)+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
    scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))


#removing onion outlier
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% filter(Individual_id_eddy!='Onion-Calliphora stygia_81') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + geom_boxplot()+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))

#boxplot with dots
crops_together_summarytable %>% filter(Crop!='Avocado'&Crop!='Kiwifruit') %>% filter(Individual_id_eddy!='Onion-Calliphora stygia_81') %>% 
  ggplot(aes(x=Crop,y=ave_num_stigmatouches_per_inflorsense,fill=`Full scientific name`)) + 
  geom_boxplot(outliers = FALSE)+
  facet_wrap(~Crop,scale='free', ncol = 3)+
  theme_bw()+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Average number stigma touches per flowering unit") + guides(fill=guide_legend(title="Species"))



data_split <-crops_together_summarytable %>% filter(Crop=='Apple') 
data_split <-split(data_split$ave_num_stigmatouches_per_inflorsense, data_split$`Full scientific name`)
lapply(data_split, shapiro.test)

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

#write.table(dunn_table_out,'Dunn_analysis_Dec2024_averageNumStigTouchPerInflore_flies.csv',sep=',',quote=F,row.names = F)
#write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_averageNumStigTouchPerInflore_flies.csv',sep=',',quote=F,row.names = F)
