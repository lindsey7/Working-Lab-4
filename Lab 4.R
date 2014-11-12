## LAB 4: CLOUD DATA
## Statistics 215A, Fall 2014
## Lindsey Hearn

## PART I : EXPLORATORY DATA ANALYSIS
## install & load packages
library(plyr)
library(ggplot2)
library(grid)

setwd("/Users/lindsey/Desktop/STAT 215A/Lab 4")

## load images
image.1 <- read.table('image_data/image1.txt', header=FALSE)
image.2 <- read.table('image_data/image2.txt', header=FALSE)
image.3 <- read.table('image_data/image3.txt', header=FALSE)

## see 'lab 4 assignment' file for list of column names
column.names <- c('y', 'x', 'expert', 'ndai', 'sd', 'corr', 'DF', 'CF', 'BF', 'AF', 'AN')
names(image.1) <- column.names
names(image.2) <- column.names
names(image.3) <- column.names

## create subset of data if expert==1|expert==-1
expert.image.1 <- subset(image.1, expert==1|expert==-1)
expert.image.2 <- subset(image.2, expert==1|expert==-1)
expert.image.3 <- subset(image.3, expert==1|expert==-1)

## combine expert.image data
expert.image <- rbind(expert.image.1, expert.image.2, expert.image.3)
## recode cloud==1 if expert==1 in expert.image* data
expert.image$cloud[expert.image$expert==1] <- 1
expert.image$cloud[expert.image$expert==-1] <- 0

## plot expert labels according X-Y coordinates: images 1-3
pdf("image1_expert.pdf")
ggplot(expert.image.1) + geom_point(aes(x=x, y=y, color=expert)) + 
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
dev.off()
pdf("image2_expert.pdf")
ggplot(expert.image.2) + geom_point(aes(x=x, y=y, color=expert)) + 
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("image3_expert.pdf")
ggplot(expert.image.3) + geom_point(aes(x=x, y=y, color=expert)) + 
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()

## X-Y coordinate distribution of radiance angles
pdf("xy_df.pdf")
ggplot(data=expert.image) +
  geom_point(aes(x=x, y=y, colour=DF)) +
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("xy_cf.pdf")
ggplot(data=expert.image) +
  geom_point(aes(x=x, y=y, colour=CF)) +
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
dev.off()
pdf("xy_bf.pdf")
ggplot(data=expert.image) +
  geom_point(aes(x=x, y=y, colour=BF)) +
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("xy_af.pdf")
ggplot(data=expert.image) +
  geom_point(aes(x=x, y=y, colour=AF)) +
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("xy_an.pdf")
ggplot(data=expert.image) +
  geom_point(aes(x=x, y=y, colour=AN)) +
  xlab("X") + ylab("Y") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
## plots ndai vs. corr
ggplot(expert.image) + geom_point(aes(x=corr, y=ndai, color = expert)) + 
  ylab("NDAI") + xlab("CORR") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
 

## conditional density plots by radiance angle
pdf("density_df.pdf")
ggplot(expert.image) + geom_density(aes(x=DF, group=factor(expert), fill=factor(expert)), alpha=0.25) +
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("Radiance angle DF") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("density_cf.pdf")
ggplot(expert.image) + geom_density(aes(x=CF, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("Radiance angle CF") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("density_bf.pdf")
ggplot(expert.image) + geom_density(aes(x=BF, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("Radiance angle BF") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("density_af.pdf")
ggplot(expert.image) + geom_density(aes(x=AF, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("Radiance angle AF") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("density_an.pdf")
ggplot(expert.image) + geom_density(aes(x=AN, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("Radiance angle AN") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()

## conditional density plots by CORR: images 1-3
pdf("image1_den_corr.pdf")
ggplot(expert.image.1) + geom_density(aes(x=corr, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("CORR") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("image2_den_corr.pdf")
ggplot(expert.image.2) + geom_density(aes(x=corr, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("CORR") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("image3_den_corr.pdf")
ggplot(expert.image.3) + geom_density(aes(x=corr, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("CORR") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()

## conditional density plots by NDAI: images 1-3
pdf("image1_den_ndai.pdf")
ggplot(expert.image.1) + geom_density(aes(x=ndai, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("NDAI") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("image2_den_ndai.pdf")
ggplot(expert.image.2) + geom_density(aes(x=ndai, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("NDAI") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("image3_den_ndai.pdf")
ggplot(expert.image.3) + geom_density(aes(x=ndai, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("NDAI") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()

## conditional density plots by SD: images 1-3
pdf("image1_den_sd.pdf")
ggplot(expert.image.1) + geom_density(aes(x=sd, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("SD") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("image2_den_sd.pdf")
ggplot(expert.image.2) + geom_density(aes(x=sd, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("SD") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()
pdf("image3_den_sd.pdf")
ggplot(expert.image.3) + geom_density(aes(x=sd, group=factor(expert), fill=factor(expert)), alpha=0.25) + 
  scale_fill_manual( values = c("grey","blue")) + 
  ylab("Density") + xlab("SD") +  
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
dev.off()

## PART II: MODELING
library(stats)
library(ellipse)
library(corrplot)
## recode cloud==1 if expert==1 in expert.image* data
expert.image.1$cloud[expert.image.1$expert==1] <- 1
expert.image.1$cloud[expert.image.1$expert==-1] <- 0
expert.image.2$cloud[expert.image.2$expert==1] <- 1
expert.image.2$cloud[expert.image.2$expert==-1] <- 0
expert.image.3$cloud[expert.image.3$expert==1] <- 1
expert.image.3$cloud[expert.image.3$expert==-1] <- 0

## subset by cloud==1, cloud==0
cloud1.image.1 <- subset(expert.image.1, cloud==1)
cloud0.image.1 <- subset(expert.image.1, cloud==0)
cloud1.image.2 <- subset(expert.image.2, cloud==1)
cloud0.image.2 <- subset(expert.image.2, cloud==0)
cloud1.image.3 <- subset(expert.image.3, cloud==1)
cloud0.image.3 <- subset(expert.image.3, cloud==0)

## calculate correlations between radiance angles
cor.rcloud1.image.1 <- cor(cloud1.image.1[,c('DF', 'CF', 'BF', 'AF', 'AN')], use="pairwise")
cor.rcloud0.image.1 <- cor(cloud0.image.1[,c('DF', 'CF', 'BF', 'AF', 'AN')], use="pairwise")
cor.rcloud1.image.2 <- cor(cloud1.image.2[,c('DF', 'CF', 'BF', 'AF', 'AN')], use="pairwise")
cor.rcloud0.image.2 <- cor(cloud0.image.2[,c('DF', 'CF', 'BF', 'AF', 'AN')], use="pairwise")
cor.rcloud1.image.3 <- cor(cloud1.image.3[,c('DF', 'CF', 'BF', 'AF', 'AN')], use="pairwise")
cor.rcloud0.image.3 <- cor(cloud0.image.3[,c('DF', 'CF', 'BF', 'AF', 'AN')], use="pairwise")

cor <- cor(expert.image[, c('DF', 'CF', 'BF', 'AF', 'AN')], use="pairwise")
cor <- cor(expert.image[, c('ndai', 'corr', 'sd')], use="pairwise")

## correlation plots by image and cloud==1, cloud==0
corrplot(cor.rcloud1.image.1, method=c("shade"), addCoef.col="black", tl.col="black")
corrplot(cor.rcloud0.image.1, method=c("shade"), addCoef.col="black", tl.col="black")
corrplot(cor.rcloud1.image.2, method=c("shade"), addCoef.col="black", tl.col="black")
corrplot(cor.rcloud0.image.2, method=c("shade"), addCoef.col="black", tl.col="black")
corrplot(cor.rcloud1.image.3, method=c("shade"), addCoef.col="black", tl.col="black")
corrplot(cor.rcloud0.image.3, method=c("shade"), addCoef.col="black", tl.col="black")

## check potential predictors: angles vs. CORR-NDAI-SD
angles <- glm(cloud ~ DF + CF + BF + AF + AN,
              data = expert.image,
              family = "binomial")
angles.predictions <- predict(angles, type="response")

others <- glm(cloud ~ corr + ndai + sd, 
              data = expert.image, 
              family = "binomial")
others.predictions <- predict(others, type="response")

roc.curve = function(s, P, print = FALSE){
  Ps = (P > s)*1
  cloud = expert.image$cloud
  ## false positive rate 
  FP = sum((Ps==1)*(expert.image$cloud==0))/sum(expert.image$cloud==0)
  ## true positive rate
  TP = sum((Ps==1)*(expert.image$cloud==1))/sum(expert.image$cloud==1)
  if(print==TRUE){
    print(table(Observed=expert.image$cloud, Predicted=Ps))
  }
  vect = c(FP,TP)
  names(vect) = c("FPR","TPR")
  return(vect)
}
threshold = 0.5
roc.curve(threshold, angles.predictions, print=TRUE)
roc.curve(threshold, others.predictions, print=TRUE)

## models: develop 0-1 classifiers for the presence of clouds  
library(glmnet)
library(nnet)
library(caret)
library(parallel)
library(doParallel)
library(foreach)
library(lattice)
library(kernlab)
library(pROC)
library(e1071)

## partition to create testing and training groups
expert.image$cloud = factor(expert.image$cloud)
expert.image <- subset(expert.image, select=c('cloud', 'ndai', 'sd', 'corr', 'DF', 'CF', 'BF', 'AF', 'AN'))
train <- createDataPartition(y = expert.image$cloud,                               
                             p = .7,
                             list = FALSE)
train.data <- expert.image[train,]
test.data <- expert.image[-train,]

## train data: train control 
## browse training model specifications
## specify control function
control <- trainControl(method = "repeatedcv", 
                        repeats = 3)
train.data$cloud = as.factor(train.data$cloud)
## logistic regression
set.seed(12345)
glm.train <- train(cloud ~ ndai + sd + corr,
                   data = train.data,
                   method = "glm", 
                   family = "binomial",
                   trControl = control, 
                   preProc = c("center", "scale"))
glm.train
## Fisher LDA
set.seed(12345)
lda.train <- train(cloud ~ ndai + sd + corr, 
                   data = train.data, 
                   method = "lda", 
                   trControl = control, 
                   preProc = c("center", "scale"))
lda.train
## resample and plot results
resample <- resamples(list(glm = glm.train, 
                           lda = lda.train))
pdf("xy_lda_glm.pdf")
xyplot(resample)
dev.off()
pdf("splom_lda_glm.pdf")
splom(resample)
dev.off()
## random forest 
set.seed(12345)
rf.train <- train(cloud ~ ndai + sd + corr, 
                  data = train.data, 
                  method = "rf", 
                  trControl = control,  
                  preProc = c("center", "scale"))
rf.train
pdf("rf_train_roc.pdf")
plot(rf.train)
dev.off()
resample <- resamples(list(glm = glm.train, 
                           lda = lda.train,
                           rf = rf.train))
pdf("xy_lda_glm_rf.pdf")
xyplot(resample)
dev.off()

resample2 <- resample(list(lda = lda.train, 
                            rf = rf.train))
pdf("xy_lda_rf.pdf")
xyplot(resample2)
dev.off()

## specify new control
control = trainControl(method = "repeatedcv",
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
## gradient boosting machine
set.seed(12345) 
gbm.train <- train(cloud ~ ndai + sd + corr,
                   data = train.data, 
                   method = "gbm",
                   metric = "ROC", 
                   verbose = FALSE, 
                   preProc = c("center", "scale"), 
                   trControl = control)
gbm.train
## partial least squares
set.seed(12345)
pls.train <- train(cloud ~ ndai + sd + corr, 
                   data = train.data, 
                   method = "pls", 
                   metric = "ROC", 
                   preProc = c("center", "scale"), 
                   trControl = control)
pls.train


## random forests
expert.image <- rbind(expert.image.1, expert.image.2, expert.image.3)
## recode cloud==1 if expert==1 in expert.image* data
expert.image$cloud[expert.image$expert==1] <- 1
expert.image$cloud[expert.image$expert==-1] <- 0
expert.image$cloud <- as.factor(expert.image$cloud)
expert.image <- subset(expert.image, select=c('cloud', 'ndai', 'sd', 'corr','x', 'y', 'DF', 'CF', 'BF', 'AF', 'AN'))

train <- createDataPartition(y = expert.image$cloud,                               
                             p = .5,
                             list = FALSE)
train.data <- expert.image[train,]
test.data <- expert.image[-train,]

rf <- randomForest(cloud ~ ndai + corr + sd, 
                            data = train.data,
                            ntree = 200,
                            mtry = 2)
rf.preds <- predict(rf, test.data,'vote')[,2]
rf.predictions <- prediction(rf.preds, test.data$cloud)
rf.fit <- performance(rf.predictions,"tpr","fpr")
pdf("ROC_rf.pdf")
plot(rf.fit,
     col = 1 ,
     lwd = 2)
dev.off()

prediction.clouds <- cbind(test.data, predictions = rf.preds)
pdf("prediction_spatial.pdf")
ggplot(data=prediction.clouds) + geom_point( aes(x=x, y=y, colour=predictions)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position="none", 
        axis.line = element_line(colour = "black"))
prediction.clouds$cloud<- as.numeric(prediction.clouds$cloud)
dev.off()
pdf("truth_cloud.pdf")
ggplot(data=prediction.clouds) + geom_point(aes(x=x, y=y, colour=cloud)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.background = element_blank(), 
        legend.position="none", 
        axis.line = element_line(colour = "black"))
dev.off()




  
