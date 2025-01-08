#find overlapping sites for towi
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

#farms with veronica
head(test)
head(test_2024)
colnames(test)
colnames(test_2024)
dat_bound<-rbind(test,test_2024) 
head(dat_bound)
Kanuka_sites<-dat_bound %>% filter(Native_Plant_Species=='Kunzea_ericoides') %>% select(Property_name) %>% unique()
Veronica_salicifolia_sites<-dat_bound %>% filter(Native_Plant_Species=='Veronica_salicifolia') %>% select(Property_name) %>% unique()

sites_bothkanukaveronica<-inner_join(Kanuka_sites,Veronica_salicifolia_sites,by='Property_name')
