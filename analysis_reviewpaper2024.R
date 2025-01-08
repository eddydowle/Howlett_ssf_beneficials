#R work for brad for biodiversity paper 2024
library(tidyverse)
library(lme4)
library(readxl)

setwd('C:/Users/hrlexd/Dropbox/PlantAndFood (1)/B4BI/Review_paper2024/')

deposition_data<-read.csv('All crop SVDv2.csv',header=T)
colnames(deposition_data)


ggplot(deposition_data,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free')

#bunch of issues with the data
unique(deposition_data$Bee.species)
deposition_data_fix<-deposition_data %>% mutate(Bee.species = str_replace(Bee.species, "control", "Control"))
#brad wants Lasioglossum spp. Leioproctus Fulvesencs, and Leioproctus spp.
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Lasioglossum sordidum\\*", "Lasioglossum spp"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Lasioglossum sordidum/cognatum", "Lasioglossum spp"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Lasioglossum sordidum", "Lasioglossum spp"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Leioproctus sp\\.", "Leioproctus spp"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Leioproctus huakiwi\\*", "Leioproctus spp"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Leioproctus huakiwi\\*", "Leioproctus spp"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Leioproctus huakiwi/imitatus", "Leioproctus spp"))


#relevel to put controls at the end:
deposition_data_fix$Bee.species<- forcats::fct_relevel(deposition_data_fix$Bee.species,"Control", after = Inf)

ggplot(deposition_data_fix,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free')+
  theme_bw()

insects_taxonomy<-read_excel('Insect groupings life histories Eddy_cp.xlsx', sheet = 'Taxonomic and life histories',na='NA')

#colours
#honey bee - gold
#bumble bee - gold3
#terrestris - gold3
#ruderatus - gold4
#hortorum - darkgoldenrod2
#lasioglossum - firebrick1
#leioproctus - firebrick
#fulevens - firebrick3
#control - grey

unique(deposition_data_fix$Bee.species)
species<-c('Control','Bombus terrestris','Apis mellifera','Lasioglossum spp','Leioproctus spp','Leioproctus fulvescens','Bombus hortorum','Bombus ruderatus')
species_col<-c('grey','gold3','gold','firebrick1','firebrick','firebrick3','darkgoldenrod2','gold4')
brads_col<-data.frame(species,species_col) %>% arrange(species)
brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)


ggplot(deposition_data_fix,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free')+
  theme_bw()+
  #scale_fill_manual(values=brads_col$species_col,labels=brads_col$species)
scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species"))

colnames(deposition_data_fix)
full_mod1  <- glm(Pollen.deposition ~ Bee.species*Crop, family="poisson", data=deposition_data_fix)
warnings()
summary(full_mod1)

#kiwifruit, apple, pear and carrot have fractions just model the others while awiating brad to discuss why there is fractiosn in the data

deposition_data_fix_fractions<-deposition_data_fix %>% filter(Crop=='Kiwifruit'|Crop=='Apple'|Crop=='Pear'|Crop=='Carrot')

deposition_data_fix_wholes<-deposition_data_fix %>% filter(Crop!='Kiwifruit'&Crop!='Apple'&Crop!='Pear'&Crop!='Carrot')

full_mod1  <- glm(Pollen.deposition ~ Bee.species*Crop, family="poisson", data=deposition_data_fix_wholes)
warnings()
summary(full_mod1)

#what the hell is up with Pak choi
test<-deposition_data_fix_wholes %>% filter(Crop=='Pak choi')

full_mod1  <- glm(Pollen.deposition ~ Bee.species, family="poisson", data=test)
summary(full_mod1)
mm <- model.matrix(~Bee.species*Crop, data = deposition_data_fix_wholes)
caret::findLinearCombos(mm)

full_mod1  <- glm(Pollen.deposition ~ Bee.species+Crop, family="poisson", data=deposition_data_fix_wholes)
summary(full_mod1)

deposition_data_fix_wholes$Bee.species<- forcats::fct_relevel(deposition_data_fix_wholes$Bee.species,"Control")
deposition_data_fix_wholes$Bee.species<- forcats::fct_relevel(deposition_data_fix_wholes$Bee.species,"Apis mellifera")
deposition_data_fix_wholes$Bee.species<- forcats::fct_relevel(deposition_data_fix_wholes$Bee.species,"Leioproctus spp")
deposition_data_fix_wholes$Crop<- forcats::fct_relevel(deposition_data_fix_wholes$Crop,"Pak choi")
deposition_data_fix_wholes$Crop<- forcats::fct_relevel(deposition_data_fix_wholes$Crop,"Avocado")

full_mod1  <- glm(Pollen.deposition ~ 0+Crop*Bee.species, family="poisson", data=deposition_data_fix_wholes)
summary(full_mod1)


##################################################
#new analysis
##################################################

#i've made a new sheet where I have count per 'floret' or flower, Im not sure what the term is, and I have stigma count per floret/flower so the model will be I think

#I think I have to use glmer to add a random effect

#glmer(pollen.deposition ~ crop*Bee.species + (1|stigma_count), family='poisson',data=table)

#this will give interactions between pollen deposition and crop, species and interaction between crop:species

#glmer(pollen.deposition ~ 0+crop*Bee.species + (1|stigma_count), family='poisson',data=table)
#zero intercept

deposition_data<-read.csv('Stigma_counts_SVGanalyses_eddy.csv',header=T)
colnames(deposition_data)

ggplot(deposition_data,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free')


#bunch of issues with the data
unique(deposition_data$Bee.species)
deposition_data_fix<-deposition_data %>% mutate(Bee.species = str_replace(Bee.species, "control", "Control"))
#brad wants Lasioglossum spp. Leioproctus Fulvesencs, and Leioproctus spp.

#relevel to put controls at the end:
deposition_data_fix$Bee.species<- forcats::fct_relevel(deposition_data_fix$Bee.species,"Control", after = Inf)

#plot out
ggplot(deposition_data_fix,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free')+
  theme_bw()

#plot with brads colour scheme
unique(deposition_data_fix$Bee.species)
species<-c('Control','Bombus terrestris','Apis mellifera','Lasioglossum spp','Leioproctus spp','Leioproctus fulvescens','Bombus hortorum','Bombus ruderatus')
species_col<-c('grey','gold3','gold','firebrick1','firebrick','firebrick3','darkgoldenrod2','gold4')
brads_col<-data.frame(species,species_col) %>% arrange(species)
brads_col$species<- forcats::fct_relevel(brads_col$species,"Control", after = Inf)


ggplot(deposition_data_fix,aes(x=Crop,y=Pollen.deposition,fill=Bee.species))+
  geom_boxplot()+
  facet_wrap(~Crop,scale='free')+
  theme_bw()+
  scale_fill_manual(values=with(brads_col,setNames(species_col,species)))+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species"))

colnames(deposition_data_fix)

#have a look at the interaction between stigma count and pollen deposition
crops_multi_stigma<-deposition_data_fix %>% filter(Crop=='Kiwifruit'|Crop=='Carrot'|Crop=='Pear'|Crop=='Apple')

ggplot(crops_multi_stigma,aes(x=Stigma.count,y=Pollen.deposition))+
  geom_point()+
  facet_wrap(~Crop,scale='free')+
  theme_bw()+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species"))

library(ggpubr)
ggplot(crops_multi_stigma,aes(x=Stigma.count,y=Pollen.deposition))+
  geom_point()+
  geom_smooth(method='lm')+
  facet_wrap(~Crop,scale='free')+
  theme_bw()+
  labs(y= "Pollen deposition") + guides(fill=guide_legend(title="Species")) +
  stat_cor(aes(label = after_stat(rr.label)), color = "red", geom = "label")

#okay just add it as a random effect doesnt seem to have a relationship in carrot ad kiwifruit

full_mod1  <- glm(Pollen.deposition ~ Crop*Bee.species, family="poisson", data=deposition_data_fix)
summary(full_mod1)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ Crop*Bee.species, data=deposition_data_fix)
summary(full_mod1)

#random effect for stigma count

full_mod2  <- glmer(Pollen.deposition ~ Crop*Bee.species + (1|Stigma.count), family='poisson',data=deposition_data_fix)
summary(full_mod2)

#few issues here
#1- Im fairly sure there is a really strong multicollinearity between some of the sp (e.g. las and leio)
#Im not convinced poison is the best distribution

ggplot(deposition_data_fix,aes(x=Pollen.deposition))+
  geom_histogram(bins=100)
?glmer
full_mod1  <- glm.nb(Pollen.deposition ~ Crop*Bee.species, data=deposition_data_fix)
summary(full_mod1)
full_mod2  <- glm(Pollen.deposition ~ Crop*Bee.species, data=deposition_data_fix,family='poisson')
summary(full_mod2)

library(DHARMa)
library(emmeans)
n_sim <- 250
simulationOutput <- simulateResiduals(fittedModel = full_mod1, n = n_sim)
plot(simulationOutput, asFactor = F)
testDispersion(simulationOutput)

simulationOutput <- simulateResiduals(fittedModel = full_mod2, n = n_sim)
plot(simulationOutput, asFactor = F)
testDispersion(simulationOutput)

full_mod2  <- glmer.nb(Pollen.deposition ~ Crop*Bee.species + (1|Stigma.count), data=deposition_data_fix,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
simulationOutput <- simulateResiduals(fittedModel = full_mod2, n = n_sim)
plot(simulationOutput, asFactor = F)
testDispersion(simulationOutput)
#this looks beter but there is still issues going on
summary(full_mod2)

#I cant rescale as its count data so -mean/sd is going to cause fractions and negatives. 
#can fix 4 optheta with:
#maxfun by using control=glmerControl(optCtrl=list(maxfun=1e6))

#full_mod2  <- glmer.nb(Pollen.deposition ~ Crop*Bee.species + (1|Stigma.count), data=deposition_data_fix)
#summary(full_mod2)

full_mod2_nointer  <- glmer.nb(Pollen.deposition ~ Crop+Bee.Origin + (1|Stigma.count), data=deposition_data_fix)
summary(full_mod2_nointer)

library(glmmTMB)
full_modTMB<-glmmTMB(Pollen.deposition ~ Crop*Bee.species + (1|Stigma.count), data=deposition_data_fix,family=nbinom2)
summary(full_modTMB)
simulationOutput <- simulateResiduals(fittedModel = full_modTMB, n = n_sim)
plot(simulationOutput, asFactor = F)
testDispersion(simulationOutput)


#not sure that this a good idea think I'll go back and clear the warnings on glmer.nb
#> full_mod2  <- glmer.nb(Pollen.deposition ~ Crop*Bee.species + (1|Stigma.count), data=deposition_data_fix,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
#boundary (singular) fit: see help('isSingular')
#
#Warning messages:
#  1: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#  Model is nearly unidentifiable: large eigenvalue ratio
#  - Rescale variables?
#  2: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv,  :
#  unable to evaluate scaled gradient
#  3: In checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv#,  :
#  Hessian is numerically singular: parameters are not #uniquely determined
#  4: In optTheta(g1, interval = interval, tol = tol, #verbose = verbose,  :
#  maxfun < 10 * length(par)^2 is not recommended.

#following https://rstudio-pubs-static.s3.amazonaws.com/33653_57fc7b8e5d484c909b615d8633c01d51.html
full_mod2  <- glmer.nb(Pollen.deposition ~ Crop*Bee.species + (1|Stigma.count), data=deposition_data_fix,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))

nrow(deposition_data_fix)
#number randome effects
length(getME(full_mod2,"theta"))
#number of fixed effects
length(fixef(full_mod2))

#rescale and update model
#cant rescale counts so trying to see if reschaling stigma.count fixes that issue
numcols<-'Stigma.count'
dfs <- deposition_data_fix
dfs[,numcols] <- scale(dfs[,numcols]) #-mean/sd
m1_sc <- update(full_mod2,data=dfs)
#did not solve it so cant solve scaling issue

#trying regrouping the dataset
unique(deposition_data_fix$Bee.Origin)
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.Origin = str_replace(Bee.Origin, "control", "Control"))

deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species = str_replace(Bee.species, "Leioproctus fulvescens", "Leioproctus spp"))
full_mod1_nb  <- glm.nb(Pollen.deposition ~ Crop+Bee.Origin, data=deposition_data_fix)
summary(full_mod1_nb)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ 0+Crop+Bee.species, data=deposition_data_fix)
summary(full_mod1_nb)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ 0+Crop*Bee.species, data=deposition_data_fix)
summary(full_mod1_nb)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ 0+Crop:Bee.species, data=deposition_data_fix)
summary(full_mod1_nb)

deposition_data_fix$Bee.species_eddy<-deposition_data_fix$Bee.species
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species_eddy = str_replace(Bee.species_eddy, "Leioproctus spp", "Native bee"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species_eddy = str_replace(Bee.species_eddy, "Lasioglossum spp", "Native bee"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species_eddy = str_replace(Bee.species_eddy, "Bombus terrestris", "Bombus"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species_eddy = str_replace(Bee.species_eddy, "Bombus ruderatus", "Bombus"))
deposition_data_fix<-deposition_data_fix %>% mutate(Bee.species_eddy = str_replace(Bee.species_eddy, "Bombus hortorum", "Bombus"))
unique(deposition_data_fix$Bee.species_eddy)

full_mod1_nb  <- glm.nb(Pollen.deposition ~ 0+Crop+Bee.species_eddy, data=deposition_data_fix)
summary(full_mod1_nb)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ 0+Crop*Bee.species_eddy, data=deposition_data_fix)
summary(full_mod1_nb)
full_mod1_nb  <- glm.nb(Pollen.deposition ~ 0+Crop*Bee.species, data=deposition_data_fix)
summary(full_mod1_nb)
n_sim <- 250
simulationOutput <- simulateResiduals(fittedModel = full_mod1_nb, n = n_sim)
plot(simulationOutput, asFactor = F)
testDispersion(simulationOutput)
testOutliers(simulationOutput)

full_mod2  <- glmer.nb(Pollen.deposition ~ Crop*Bee.species_eddy , data=deposition_data_fix,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
full_mod3  <- glmer.nb(Pollen.deposition ~ Crop+Bee.species + (1|Stigma.count) , data=deposition_data_fix,glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)))
full_mod3  <- glm.nb(Pollen.deposition ~ 0+Crop+Bee.species , data=deposition_data_fix)
simulationOutput <- simulateResiduals(fittedModel = full_mod3, n = n_sim)
plot(simulationOutput, asFactor = F)
testDispersion(simulationOutput)
testOutliers(simulationOutput)
summary(full_mod3)

#having a look at multilinerality
deposition_data_control<-deposition_data_fix %>% dplyr::select(Crop, Bee.species,Pollen.deposition) %>% filter(Bee.species=='Control') %>% mutate(Pollen.deposition_control=Pollen.deposition) %>% dplyr::select(Crop,Pollen.deposition_control)


#%>% spread(key = Crop, value = Bee.species)

