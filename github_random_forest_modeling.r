# load the RData file
library(readxl)
library(ranger)
library(randomForest)
library(ggplot2)
Data_raw <- read_excel("Data for random forest.xlsx")
View(Data_raw)

data_v1 <- Data_raw[, -c(1,4)]#delete label and timepoint
View(data_v1)

data_v1$Sex <- as.factor(data_v1$Sex)
data_v1$litter <- as.factor(data_v1$litter)
data_v1$Breed <- as.factor(data_v1$Breed)

##---------------------------- RF Modeling -------------------------------------
library(ranger)
data_rf <- janitor::clean_names(data_v1)

## feature importance score
imp_c1 <- matrix(0, (dim(data_rf)[2]-1), 500)
for(iter in 1: 500){
  fit <- ranger(mem_total ~., data = data_rf, importance = "impurity")
  imp_c1[, iter] <- fit$variable.importance
}
imp_score <- apply(imp_c1, 1, mean)
##------------------------- plot feature importance ----------------------------
a <- sort(imp_score, decreasing = TRUE)
b <- 1+sort(imp_score, decreasing = TRUE, index.return = TRUE)$ix
names(b) <- names(data_rf)[b]

feature_importance <- data.frame(feature = names(data_rf)[b],
                                 score = a)
								 
#Fig A
library(ggplot2)
ggplot(feature_importance[c(1:17,28,38:40),], aes(x=reorder(feature,score), y=score,fill=score))+ 
  geom_bar(stat="identity", position="dodge")+ coord_flip()+
  ylab("Variable Importance")+ xlab("")+
  ggtitle("Information Value Summary")+ #guides(fill=F)+
  scale_fill_gradient(low="grey", high="dark red") + 
  theme_classic() + theme(axis.text=element_text(size=7),
                          axis.title=element_text(size=7,face="bold"))
##keep value importance > 3 and (age, litter, gender, breed)						  
						


# Fig B
library(ranger)
opt_ft_num <- 17 #17 taxa that value importance > 3
rf.fit <- ranger(mem_total ~ ., data = data_rf[, c(1, b[1:opt_ft_num])])
pred_data <- data.frame(true = data_rf$mem_total, pred = rf.fit$predictions)
ggplot(pred_data, aes(x = true, y = pred)) +
  geom_point() + geom_smooth(method = "loess", se = TRUE) + theme_classic()+
  ggtitle(paste("Number of correct trials vs predicted number of correct trials", 
                round(cor(rf.fit$predictions, data_rf$mem_total, method = "spearman"), 3)))
cor.test(rf.fit$predictions, data_rf$mem_total, 
         method = "spearman")
		 
## if you can't make the following figures, please library the following packages
library(shapper)
library(DALEX2)
library(SHAPforxgboost)
library(shapviz)
library(treeshap)
# if you don't have shapviz, please install it.
#devtools::install_github("mayer79/shapviz")
data_shap <- data_rf[, c(1, b[1:opt_ft_num])]

## treeshap
rf.fit <- ranger(mem_total~., data = data_shap)
unified <- ranger.unify(rf.fit, data_shap)
treeshap1 <- treeshap(unified,  data_shap, verbose = 0)
shp <- shapviz(treeshap1, X_pred = as.matrix(data_shap[, -1]), X = data_shap)

# Fig E
sv_importance(shp, kind = "beeswarm", size = 1,max_display=17L,show_numbers=FALSE,
              viridis_args = list(begin = 0.1, end = 0.95, option = "magma")) +
  theme_grey(base_size = 18)

shp[["X"]][["mem_total"]]<-factor(shp[["X"]][["mem_total"]],levels=c("6","5","4","3","2","1"))
 
# Fig C
sv_dependence(shp, v = "bifidobacterium_pseudolongum", color_var = "mem_total",
              viridis_args = NULL,
              size = 1) + theme_classic()+
  theme_grey(base_size = 18) + labs(color='Mem Scores')+
  scale_colour_manual(values=c("#A63603","#E6550D","#FD8D3C","#FDAE6B","#FDD0A2","#FEEDDE"))+ 
  theme_classic() +
  theme(axis.text=element_text(size=7),
                          axis.title=element_text(size=7,face="bold"))


# Fig D
sv_dependence(shp, v = "streptococcusuberis", color_var = "mem_total",
              viridis_args = NULL,
              size = 1) + theme_classic() +
  theme_grey(base_size = 18) + labs(color='Mem Scores') +
  scale_colour_manual(values=c("#A63603","#E6550D","#FD8D3C","#FDAE6B","#FDD0A2","#FEEDDE")) +
  theme_classic() +
  theme(axis.text=element_text(size=7),
                          axis.title=element_text(size=7))