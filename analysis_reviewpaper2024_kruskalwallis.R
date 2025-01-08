#new analysis after discussion with brad and mellisa
#switching to a kruskal-wallis test for comparing SVG distributions within a plant
#not analysing kiwifruit but replotting using Mellisa's data from her 2022 paper
library(tidyverse)

setwd('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024/')
deposition_data<-read.csv('Stigma_counts_SVGanalyses_eddy_nokiwi.csv',header=T)
colnames(deposition_data)
head(deposition_data)
unique(deposition_data$Crop)
unique(deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("\\.", "", deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("\\*", "", deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("Lasioglossum sordidum/\\cognatum", "Lasioglossum spp", deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("Lasioglossum sordidum", "Lasioglossum spp", deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("Leioproctus sp", "Leioproctus spp", deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("Leioproctus huakiwi\\/imitatus", "Leioproctus spp", deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("Leioproctus huakiwi", "Leioproctus spp", deposition_data$Bee_species)
deposition_data$Bee_species <- gsub("Bombus hortorum", "Bombus hortorum/ruderatus", deposition_data$Bee_species)

unique(deposition_data$Bee_species)

deposition_data_kiwi<-read.csv('SVDs-2013-2015-Hayward.csv',header=T)
#columns of interest pollen count = male.pollen.rest.stigmas.est + male.pollen.first.stigma

deposition_data_kiwi_fix<-deposition_data_kiwi %>% dplyr::select(Crop,Order,tax,Male.pollen.first.stigma,Male.pollen.rest.stigmas.est) %>% mutate(Pollen_deposition=Male.pollen.first.stigma+Male.pollen.rest.stigmas.est) %>% dplyr::select(-Male.pollen.first.stigma,-Male.pollen.rest.stigmas.est) %>% filter(Order=='Hymenoptera'|tax=='control'|tax=='Control')%>% dplyr::select(-Order) %>% drop_na(Pollen_deposition)

unique(deposition_data_kiwi_fix$tax)
deposition_data_kiwi_fix<-deposition_data_kiwi_fix %>% mutate(Bee_species = str_replace(tax, "control", "Control")) %>% dplyr::select(-tax) %>% mutate(Bee_species = str_replace(Bee_species, "Bombus ruderatus", "Bombus hortorum/ruderatus"))
unique(deposition_data_kiwi_fix$Bee_species)
deposition_data_kiwi_fix$Bee_species <- gsub("\\.", "", deposition_data_kiwi_fix$Bee_species)
unique(deposition_data_kiwi_fix$Crop)
deposition_data_kiwi_fix$Crop <- gsub("kiwifruit", "Kiwifruit", deposition_data_kiwi_fix$Crop)


colnames(deposition_data_kiwi_fix)
colnames(deposition_data)
deposition_data_all<-bind_rows(deposition_data,deposition_data_kiwi_fix)

#remake boxplots
#relevel to put controls at the end:
deposition_data_all$Bee_species<- forcats::fct_relevel(deposition_data_all$Bee_species,"Control", after = Inf)

#plot with brads colour scheme
unique(deposition_data_all$Bee_species)
species<-c('Control','Bombus terrestris','Apis mellifera','Lasioglossum spp','Leioproctus spp','Leioproctus fulvescens','Bombus hortorum/ruderatus')
species_col<-c('grey','gold3','gold','firebrick1','firebrick','firebrick3','darkgoldenrod2')
brads_col<-data.frame(species,species_col) %>% arrange(species)
brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)

library(scales)
#ggplot(deposition_data_all,aes(x=Crop,y=log(Pollen_deposition+1),fill=Bee_species))+
  ggplot(deposition_data_all,aes(x=Crop,y=Pollen_deposition,fill=Bee_species))+
  geom_boxplot()+
 facet_wrap(~Crop,scale='free', ncol = 4)+
 #  facet_grid(.~Crop, scales='free',space='free')+ 
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
    coord_trans(y='log1p')
#  scale_y_log10() 
#scale_y_continuous(trans='log2' )
#    scale_y_continuous(trans = log2_trans(),
 #                      breaks = trans_breaks("log2", function(x) x + 1),
  #                     labels = trans_format("log2", math_format(.x + 1)))
  
  library(ggforce)
  #ggplot(deposition_data_all,aes(x=Crop,y=log(Pollen_deposition+1),fill=Bee_species))+
  ggplot(deposition_data_all,aes(x=Crop,y=Pollen_deposition,fill=Bee_species))+
    geom_boxplot()+
    facet_row(vars(Crop),scale='free',space='free')+
    #  facet_grid(.~Crop, scales='free',space='free')+ 
    theme_bw()+
    scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
    labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) + 
    coord_trans(y='log1p')+
  scale_x_discrete(expand = c(0, 0.5))
  
library(coin)
library(FSA)
  #kruskal-wallis test
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


write.table(dunn_table_out,'Dunn_analysis_Dec2024_8crops.csv',sep=',',quote=F,row.names = F)
write.table(kruskal_table_out,'Kruskal_analysis_Dec2024_8crops.csv',sep=',',quote=F,row.names = F)

#pairwise.wilcox.test(subset_crop$Pollen_deposition,subset_crop$Bee_species,p.adjust.method = 'BH')
#alternative dunn test, makes a pretty table, slightly different pvalues for some reason???
library(dunn.test)
dunn_test2<-dunn.test(subset_crop$Pollen_deposition,subset_crop$Bee_species,method='bonferroni')
dunn_test2$P.adjusted
dunn_test2$chi2
dunn_test2$Z
dunn_test2$comparisons
  
  
  
  
  