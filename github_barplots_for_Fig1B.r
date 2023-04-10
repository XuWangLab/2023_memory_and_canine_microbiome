#selection based on number of correct trials (rank order 80 measurements)
library(RColorBrewer)
library(ggplot2)

Mem10sRatio<-c(0.33,1.00,1.00,0.33,0.67,0.67,0.33,0.33,1.00,0.67,1.00,0.67,0.00,0.33,0.67,0.67,0.33,0.33,1.00,1.00,1.00,0.67,1.00,1.00,0.00,1.00,0.67,0.67,0.67,0.67,0.67,0.67,0.67,0.67,0.33,0.33,0.67,0.67,1.00,1.00,1.00,0.67,0.33,0.33,1.00,1.00,0.33,1.00,0.33,1.00,0.67,0.33,0.67,0.67,0.67,0.33,0.67,0.67,0.33,0.67,0.33,0.67,1.00,1.00,1.00,0.67,0.67,1.00,0.33,0.33,0.67,1.00,1.00,1.00,0.67,0.33,0.67,1.00,1.00,0.67)
Mem40sRatio<-c(0.33,0.67,0.67,0.00,0.67,0.67,0.67,0.33,1.00,0.00,0.33,0.67,0.67,0.67,0.33,0.67,0.67,0.00,1.00,0.67,1.00,0.67,0.33,1.00,0.67,0.67,1.00,0.33,1.00,0.33,1.00,0.33,1.00,0.33,0.33,0.67,0.67,0.67,0.67,0.33,1.00,0.33,0.33,1.00,0.67,0.33,0.00,0.33,1.00,1.00,0.67,0.67,0.33,0.33,0.67,0.33,1.00,0.33,0.67,0.33,0.67,0.67,0.67,0.33,0.67,0.67,0.67,0.67,0.67,0.67,0.67,1.00,1.00,0.33,0.67,1.00,0.67,0.67,0.67,1.00)

Mem10sCounts<-round(Mem10sRatio*3)
Mem40sCounts<-round(Mem40sRatio*3)

total <- Mem10sCounts+Mem40sCounts
sampleID<-1:80

df<-data.frame(Mem10sCounts,Mem40sCounts,total,sampleID)
df2<-df[order(total,Mem40sCounts),]

#change order
temp<-df2[4,]
temp2<-df2[9,]
df2[4,]<-temp2
df2[9,]<-temp

number<-c(df2$Mem10sCounts,df2$Mem40sCounts)
group<-factor(c(rep("10s",80),rep("40s",80)),levels=c("40s","10s"))
sampleID<-factor(rep(df2$sampleID,2),levels=df2$sampleID)

df3<-data.frame(number,group,sampleID)
ggplot(data=df3, aes(x=sampleID, y=number, fill=group)) +
  geom_bar(stat="identity")+
  scale_fill_manual(values=c('#999999','#E69F00')) +
   labs(title="", x="", y = "Number of correct trials")+
   theme_bw()+ #背景变为白色
   theme(#legend.position="none", #不需要图例
        # axis.text.x=element_text(colour="black",species="Arial",size=14), #设置x轴刻度标签的字体属性
        # axis.text.y=element_text(species="Arial",size=14,face="plain"), #设置x轴刻度标签的字体属性
        # axis.title.y=element_text(species="Arial",size = 14,face="plain"), #设置y轴的标题的字体属性
        # axis.title.x=element_text(species="Arial",size = 14,face="plain"), #设置x轴的标题的字体属性
        # #plot.title = element_text(species="Arial",size=15,face="bold",hjust = 0.5), #设置总标题的字体属性
        panel.grid.major = element_blank(), #不显示网格线
        panel.grid.minor = element_blank())









