#new analysis after discussion with brad and mellisa
#switching to a kruskal-wallis test for comparing SVG distributions within a plant
#not analysing kiwifruit but replotting using Mellisa's data from her 2022 paper
library(tidyverse)

setwd('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024/')
deposition_data<-read.csv('Copy of PC controls SVD data for Eddy non bees sent.csv',header=T)
colnames(deposition_data)
head(deposition_data)
unique(deposition_data$Crop)
unique(deposition_data$Bee.species)

deposition_data_kiwi<-read.csv('SVDs-2013-2015-Hayward.csv',header=T)
#columns of interest pollen count = male.pollen.rest.stigmas.est + male.pollen.first.stigma
unique(deposition_data_kiwi$tax)

#need to lose the non-honey bee bees from kiwi
deposition_data_kiwi_fix<-deposition_data_kiwi %>% dplyr::select(Crop,Order,tax,Male.pollen.first.stigma,Male.pollen.rest.stigmas.est) %>% mutate(Pollen.deposition=Male.pollen.first.stigma+Male.pollen.rest.stigmas.est) %>% dplyr::select(-Male.pollen.first.stigma,-Male.pollen.rest.stigmas.est) %>% filter(tax!='Bombus terrestris') %>%  filter(tax!='Bombus ruderatus') %>% filter(tax!='Leioproctus spp.') %>% filter(tax!='Lasioglossum spp.') %>% filter(tax!='Calliprason pallidus') %>% filter(tax!='') %>% dplyr::select(-Order) %>% drop_na(Pollen.deposition)
unique(deposition_data_kiwi_fix$tax)

#need to switch Bibionidae to Dilophus nigrostigma
#exclude "Calliprason pallidus" not represented in the other crops and only 4 records
deposition_data_kiwi_fix$tax   <- gsub("Bibionidae", "Dilophus nigrostigma", deposition_data_kiwi_fix$tax)
unique(deposition_data_kiwi_fix$tax)

deposition_data_kiwi_fix<-deposition_data_kiwi_fix %>% mutate(Bee.species = str_replace(tax, "control", "Control")) %>% dplyr::select(-tax) 
unique(deposition_data_kiwi_fix$Bee.species)
unique(deposition_data_kiwi_fix$Crop)
deposition_data_kiwi_fix$Crop <- gsub("kiwifruit", "Kiwifruit", deposition_data_kiwi_fix$Crop)


colnames(deposition_data_kiwi_fix)
colnames(deposition_data)
deposition_data_all<-bind_rows(deposition_data,deposition_data_kiwi_fix)

#remake boxplots
#relevel to put controls at the end:
deposition_data_all$Bee.species<- forcats::fct_relevel(deposition_data_all$Bee.species,"Control", after = Inf)

#plot with brads colour scheme
library(readxl)
unique(deposition_data_all$Bee.species)
brads_col_2<-read_excel('Copy of fly Pie chart data for Eddyv2_flies.xlsx',sheet='Insect_ordering')
head(brads_col_2)
#fix spelling for merge

deposition_data_all$Bee.species <- gsub("\\*", "", deposition_data_all$Bee.species)
brads_col_2$Species<- gsub("\\*", "", brads_col_2$Species)

deposition_data_all$Bee.species <- gsub("Apis mellfera", "Apis mellifera", deposition_data_all$Bee.species)
deposition_data_all$Bee.species <- gsub("Helophilus cingulata", "Helophilus cingulatus", deposition_data_all$Bee.species)

deposition_data_all$Bee.species <- gsub("Australophyra rostrata", "Hydrotaea rostrata", deposition_data_all$Bee.species)
#Protohystricia alcis*
unique(deposition_data_all$Bee.species)
deposition_data_all$Bee.species <- gsub("Protohystricia alcis", "Prohystricia alcis", deposition_data_all$Bee.species)
deposition_data_all$Bee.species <- gsub("Odontomyia cloris\\/atrovirens", "Odontomyia spp.", deposition_data_all$Bee.species)


colours_figs<-deposition_data_all %>% select(Bee.species) %>% unique() %>% left_join(.,brads_col_2,by=c('Bee.species'='Species'))
brads_col<-colours_figs %>% arrange(Bee.species)
brads_col$species<-brads_col$Bee.species
brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)

?fct_relevel
unique(deposition_data_all$Bee.species)
deposition_data_all$Bee.species<- forcats::fct_relevel(deposition_data_all$Bee.species,"Control", after = Inf)
deposition_data_all$Bee.species<- forcats::fct_relevel(deposition_data_all$Bee.species,"Apis mellifera","Calliphora stygia","Calliphora vicina","Lucilia sericata","Pollenia pseudorudis","Pollenia spp.","Hydrotaea rostrata","Delia platura","Prohystricia alcis","Oxysarcodexia varia","Eristalis tenax","Helophilus hochstetteri","Helophilus cingulatus","Melangyna novaezelandiae","Melanostoma fasciatum","Allograpta dorsalis","Odontomyia spp.","Dilophus nigrostigma","Zorion guttigerum","Control")

brads_col$species<- forcats::fct_relevel(brads_col$species,"Apis mellifera","Calliphora stygia","Calliphora vicina","Lucilia sericata","Pollenia pseudorudis","Pollenia spp.","Hydrotaea rostrata","Delia platura","Prohystricia alcis","Oxysarcodexia varia","Eristalis tenax","Helophilus hochstetteri","Helophilus cingulatus","Melangyna novaezelandiae","Melanostoma fasciatum","Allograpta dorsalis","Odontomyia spp.","Dilophus nigrostigma","Zorion guttigerum","Control")

brads_col$species <- factor(brads_col$species, levels=brads_col$species[order(brads_col$Code)], ordered=TRUE)
brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)
levels(brads_col$species)
deposition_data_all$Bee.species <- forcats::fct_relevel(deposition_data_all$Bee.species,levels(brads_col$species))
levels(deposition_data_all$Bee.species)
library(scales)
#standard boxplot
ggplot(deposition_data_all,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
 facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
    coord_trans(y='log1p')

#plus dots
ggplot(deposition_data_all,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot(outliers = FALSE)+
  geom_jitter(position = position_jitterdodge(),colour='black',pch=21,alpha = 0.4)+
    scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
#scale_color_manual(values=set_names(brads_col$Colour, brads_col$species))+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')
#think the warning is about the black outline and mismatch to legent

#violin
ggplot(deposition_data_all,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_violin()+
  facet_wrap(~Crop,scale='free', ncol = 4)+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
  coord_trans(y='log1p')


library(ggforce)
ggplot(deposition_data_all,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
    geom_boxplot()+
    facet_row(vars(Crop),scale='free',space='free')+
    #  facet_grid(.~Crop, scales='free',space='free')+ 
    theme_bw()+
    scale_fill_manual(values=with(brads_col,setNames(Colour,species)))+
    labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
    coord_trans(y='log1p')+
  scale_x_discrete(expand = c(0, 0.5))
  
library(coin)
library(FSA)
  #kruskal-wallis test
deposition_data_all$Pollen_deposition<-deposition_data_all$Pollen.deposition
deposition_data_all$Bee_species<-deposition_data_all$Bee.species

test<-'Carrot'
subset_crop<-deposition_data_all %>% filter(Crop==test)
kruskal.test(Pollen_deposition ~ Bee_species,data=subset_crop)
result_kruskal<-kruskal.test(Pollen_deposition ~ Bee_species,data=subset_crop)
dunnTest(Pollen_deposition ~ Bee_species,data=subset_crop,method='bonferroni')
result_dunn<-dunnTest(Pollen_deposition ~ Bee_species,data=subset_crop,method='bonferroni')

result_kruskal$statistic
result_kruskal$parameter
result_kruskal$p.value
result_kruskal$method
result_kruskal$data.name
as.data.frame(result_kruskal)

test_kruskal<-data.frame(c(result_kruskal$statistic,result_kruskal$parameter,pvalue=result_kruskal$p.value))
colnames(test_kruskal)[1] <- paste0(test,'_KruskalWallis')
test_kruskal

result_dunn<-dunnTest(Pollen_deposition ~ Bee_species,data=subset_crop,method='bonferroni')
result_dunn
dunn_table<-result_dunn$res
dunn_table$P.unadj<-NULL
names(dunn_table)[names(dunn_table) == 'Z'] <- paste0(test,'_Z')
names(dunn_table)[names(dunn_table) == 'P.adj'] <- paste0(test,'_P.adj')
dunn_table
subset_crop$Bee_species

#doing a kruskal-wallis and Dunn test for each group
kruskal_table_out<-NULL
dunn_table_out<-NULL
crops<-unique(deposition_data_all$Crop)
for (item in crops) {
  print(item)
  subset_crop<-deposition_data_all %>% filter(Crop==item)
  #doesnt work for some reason in dunn test, it doesnt take into account levels so just going to rename control with a z to get it go at the end, doesnt really matter but keeps the Z values clean etc
  subset_crop$Bee_species <- gsub("Control", "ZControl", subset_crop$Bee_species)
  subset_crop$Bee_species<- forcats::fct_relevel(subset_crop$Bee_species,"ZControl", after = Inf)
#  subset_crop$Bee_species<- forcats::fct_relevel(subset_crop$Bee_species,"Control", after = Inf)
  result_kruskal<-kruskal.test(Pollen_deposition ~ Bee_species,data=subset_crop)
  #note dunn.test (dunn.test package R) runs a one sided test dunnTest (FSA) runs a two sided test
  result_dunn<-dunnTest(Pollen_deposition ~ Bee_species,data=subset_crop,method='bonferroni')
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

dunn_table_out<-dunn_table_out %>% arrange(Comparison)
dunn_table_out$Comparison<-gsub("ZControl", "Control", dunn_table_out$Comparison)


write.table(dunn_table_out,'Dunn_analysis_Dec2024_8crops_flies.csv',sep=',',quote=F,row.names = F)
write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_8crops_flies.csv',sep=',',quote=F,row.names = F)

#pairwise.wilcox.test(subset_crop$Pollen_deposition,subset_crop$Bee_species,p.adjust.method = 'BH')
#alternative dunn test, makes a pretty table, slightly different pvalues for some reason???
library(dunn.test)
dunn_test2<-dunn.test(subset_crop$Pollen_deposition,subset_crop$Bee_species,method='bonferroni')
dunn_test2$P.adjusted
dunn_test2$chi2
dunn_test2$Z
dunn_test2$comparisons
  
  
  
  
  