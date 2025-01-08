#fucking about 
setwd('C:/Users/hrlexd/Dropbox/Otago_2023 (1)/Ben_hairworm/Jeff_work/RNA_extractions_2024/')
#merge sample stuff RNAseq
subsheet<-read.table('Submission_sheet_AGRF.csv',header=T,row.names=NULL,sep=',')
mysheet<-read.table('SequencingSamples_Sep2024.csv',header=T,row.names=NULL,sep=',')
joint<-left_join(subsheet,mysheet,by=c('Sample.Name'='Codes'))
write.table(joint,'SequencingSamples_Sep2024_forPFRsheet.csv',row.names=F,sep=',')
