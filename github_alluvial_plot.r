#Bifidobacterium alluvial plot
library(openxlsx)
library(ggplot2)
library(RColorBrewer)
library(ggalluvial)

data<-read.xlsx("count_nr_species_EarlyCognition.xlsx",colNames=TRUE,rowNames=TRUE)

highOMS<-data[,c(9,19,21,24,41,50,72,73)]
lowOMS<-data[,c(1,4,8,18,35,43,47,56)]

high_RA<-apply(highOMS,1,mean)
low_RA<-apply(lowOMS,1,mean)

df<-data.frame(low_RA,high_RA,rownames(data))
colnames(df)<-c("low","high","species")

#high abundant species RA>0.001
selected<-union(which(df$low>0.001),which(df$high>0.001))
#df[selected,]
rownames(df)<-1:dim(df)

low_other_RA<-mean(df$low[-selected])
high_other_RA<-mean(df$high[-selected])

df<-df[selected,]
dim(df)
df[(dim(df)[1]+1),]<-c(low_other_RA,high_other_RA,"Other")
df<-df[order(df$high,decreasing=TRUE),]

group<-factor(c(rep("low",dim(df)[1]),rep("high",dim(df)[1])),levels=c("low","high"))
species<-factor(rep(df$species,2),levels=c(levels=c("Bifidobacteriumpseudolongum","Bifidobacteriumcriceti","Bifidobacteriumanimalis","Bifidobacteriumpullorum","Other")))
RA<-as.numeric(c(df$low,df$high))
alluvium<-rep(1:dim(df)[1],2)

df2<-data.frame(group,species,RA,alluvium)

ggplot(df2,aes(x = group, stratum = species, alluvium = alluvium, y = RA, fill = species)) +
  geom_stratum() +
  geom_flow() +
  scale_fill_manual(values = brewer.pal(5,"Set1"))+
  theme_bw()

