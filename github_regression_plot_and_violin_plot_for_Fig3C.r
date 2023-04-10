# a Scatterplot with a fitted line based on linear regression model
library(openxlsx)
library(ggplot2)
library(RColorBrewer)

data<-read.xlsx("sig altered or lefse featured taxa RA.xlsx",colNames=TRUE,rowNames=TRUE)

mem_count<-c(2,5,5,1,4,4,3,2,6,2,4,4,2,3,3,4,3,1,6,5,6,4,4,6,2,5,5,3,5,3,5,3,5,3,2,3,4,4,5,4,6,3,2,4,5,4,1,4,4,6,4,3,3,3,4,2,5,3,3,3,3,4,5,4,5,4,4,5,3,3,4,6,6,4,4,4,4,5,5,5)

group1<-c()
for(i in 1:length(mem_count))
{
	if(mem_count[i]%in%c(1,2,3))
	{
		group1<-c(group1,"low")
	}
	if(mem_count[i]%in%c(4,5,6))
	{
		group1<-c(group1,"high")
	}
	
}
group1<-factor(group1,levels=c("low","high"))

pvalue<-c()
rho<-c()
pvalue_twogroup<-c()
for(i in 1:dim(data)[1])
{
	
	df<-data.frame(log2(as.numeric(data[i,])),mem_count)
	colnames(df)<-c("RA","mem")
	
	test<-cor.test(df$RA,df$mem,method="spearman",exact = FALSE)
	pvalue<-c(pvalue,test$p.value)
	rho<-c(rho,test$estimate)
	
	df2<-data.frame(log2(as.numeric(data[i,])),group1)
	colnames(df2)<-c("RA","group")
	test2<-wilcox.test(df2[which(df2$group=="high"),1],df2[which(df2$group=="low"),1])
	pvalue_twogroup<-c(pvalue_twogroup,test2$p.value)
}
qvalue<-qvalue(pvalue,pi0=1)$qvalues
temp<-cbind(rho,pvalue,qvalue,pvalue_twogroup)
rownames(temp)<-rownames(data)

selected<-which(temp[,2]<0.05&temp[,3]<0.05)
selected_taxa<-rownames(data)[selected]


#scatterplot for regression analysis
for (i in 1:length(selected_taxa))
{
	filename<-paste("regression plot of ",selected_taxa[i],".pdf",sep='')
	pdf(filename, w=3.6, h=4)
	
	
	df<-data.frame(log2(as.numeric(data[which(rownames(data)==selected_taxa[i]),])),mem_count)
	colnames(df)<-c("RA","mem")
	
	p<-ggplot(df,aes(mem, RA)) +
		geom_point() +
		geom_smooth(method='lm', se=FALSE,color="blue")+
		theme_bw()+ 
		theme(legend.position="none", 
			plot.title = element_text(size=8,hjust = 0.5), 
			panel.grid.major = element_blank(), 
			panel.grid.minor = element_blank())+
		ylab("Relative abundance")+xlab("memory performance")+
		ggtitle(paste('Regression for ',rownames(data)[63],sep=''))+
		scale_x_continuous(limits = c(1,6), breaks = c(1, 2, 3, 4, 5, 6))

	print(p)

	dev.off()

}



#violin plot of two group comparison (1-3 vs 4-6)
for (i in 1:length(selected_taxa))
{
	filename<-paste("violin plot of ",selected_taxa[i],".pdf",sep='')
	pdf(filename, w=3, h=6)
	
	if(min(as.numeric(data[which(rownames(data)==selected_taxa[i]),]))==0)
	{
		data[which(rownames(data)==selected_taxa[i]),which(as.numeric(data[which(rownames(data)==selected_taxa[i]),])==0)]=0.0000000001
	}
	
	df2<-data.frame(log2(as.numeric(data[which(rownames(data)==selected_taxa[i]),])),group1)
	colnames(df2)<-c("RA","group")
	
	p<-ggplot(df2, aes(x=group, y=RA, fill=group)) + 
		geom_violin(lwd=0.5)+
		scale_fill_manual(values = c("#CE2020","#056868"))+
		theme_bw()+
		theme(legend.position="none", 
			panel.grid.major = element_blank(), 
			panel.grid.minor = element_blank())+
		ylab("Relative abundance")+xlab("")+
		ylim(1.1*min(df2$RA),0.9*max(df2$RA))

	print(p)

	dev.off()

}





