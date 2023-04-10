library(openxlsx)
library(ape)
library(ggplot2)
library(vegan)
library(RColorBrewer)

data<-read.xlsx("count_nr_species_EarlyCognition.xlsx"colNames=TRUE,rowNames=TRUE)
#dim(species)


timepoint<-factor(c(1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3,1,2,3))
litter<-c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","B","B","B","B","B","B","B","B","B","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","C","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D","D")
correct_trials<-factor(c(2,5,5,1,4,4,3,2,6,2,4,4,2,3,3,4,3,1,6,5,6,4,4,6,2,5,5,3,5,3,5,3,5,3,2,3,4,4,5,4,6,3,2,4,5,4,1,4,4,6,4,3,3,3,4,2,5,3,3,3,3,4,5,4,5,4,4,5,3,3,4,6,6,4,4,4,4,5,5,5))
gender<-c("M","M","M","M","M","M","F","F","F","M","M","M","F","F","F","F","F","F","M","M","M","M","M","M","F","F","F","F","F","F","F","F","F","F","F","F","F","F","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","M","F","F","F","F","F","F","F","F","F","M","M","M")

#-------------alpha diversity binned by litter/timepoint/correct_trials/gender-----------------#
alpha_species<-diversity(t(data),index="shannon")
group<-litter
data_species<-data.frame(alpha_species,group)

ggplot(data_species, aes(x=group, y=alpha_species, fill=group)) + 
  geom_boxplot()+
  stat_boxplot(geom = "errorbar",width=0.15,aes(color="black"))+ 
  geom_boxplot(size=0.5)+ 
  scale_fill_manual(values = c("#48EFAB","#EFEB59","#B28BF2"))+
  theme_bw()+ 
  theme(legend.position="none",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ylab("Shannon index at species level")+xlab("")+
  ylim(0.9*min(data_species$alpha_species),1.1*max(data_species$alpha_species))
  
wilcox.test(data_species[which(data_species$group==1),1],data_species[which(data_species$group==2),1])

#------------------------------------beta diversity--------------------------------------------#
jaccard_dist_species<-vegdist(t(data),method = "bray")

#PCoA analysis
data.pcoa_species<-pcoa(jaccard_dist_species,correction = "cailliez")

#plotting
data.plot_species<-data.frame(data.pcoa_species$vectors)
data.plot_species$group<-group

x_label_species<-round(data.pcoa_species$values$Rel_corr_eig [1]*100,2)
y_label_species<-round(data.pcoa_species$values$Rel_corr_eig [2]*100,2)

ggplot(data=data.plot_species,aes(x=Axis.1,y=Axis.2,fill=group,shape=group))+
  geom_point(stroke = 0.5,size=2)+
  scale_shape_manual(values = rep(21,42))+
  scale_fill_manual(values = c("#48EFAB","#EFEB59","#B28BF2"))+
  theme_bw()+
  theme(panel.grid = element_blank())+
  geom_vline(xintercept = 0,lty="dashed")+
  geom_hline(yintercept = 0,lty="dashed")+
  labs(x=paste0("PCoA1 ",x_label_species,"%"),
       y=paste0("PCoA2 ",y_label_species,"%"))+
  stat_ellipse(data=data.plot_species,
               geom = "polygon",
               aes(x=Axis.1,y=Axis.2,fill=group),
               alpha=0.25)

##PERMANOVA
adonis2(jaccard_dist_species~data.plot_species$group,data = data.plot_species,permutations = 999,method="bray") 


