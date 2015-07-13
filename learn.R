library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(nnet)
library(MASS)
library(e1071)
library(corrplot)
library(randomForest)
library(gbm)
require(caret)

source('./functions.R')


dataToyota<-read.csv2("toyota.csv")
dataMercedes<-read.csv2("mercedes.csv")
dataNissan<-read.csv2("nissan.csv")
dataBMW<-read.csv2("bmw.csv")
dataKia<-read.csv2("kia.csv")
dataMazda<-read.csv2("mazda.csv")
dataFord<-read.csv2("ford.csv")
dataPeugeot<-read.csv2("peugeot.csv")
dataChevrolet<-read.csv2("chevrolet.csv")
dataHonda<-read.csv2("honda.csv")
dataHyundai<-read.csv2("hyundai.csv")
dataVolvo<-read.csv2("volvo.csv")
dataRenault<-read.csv2("renault.csv")
dataVolkswagen<-read.csv2("volkswagen.csv")
dataAudi<-read.csv2("audi.csv")
dataSubaru<-read.csv2("subaru.csv")


dataraw<-rbind(dataToyota,dataMercedes,dataNissan,dataBMW,dataKia,dataMazda,dataFord,dataPeugeot,dataChevrolet,dataHonda,dataHyundai,
            dataVolvo,dataRenault,dataVolkswagen,dataAudi,dataSubaru)

mileageBreaks <- c(0,10000,20000,50000,100000,120000,140000,160000,180000,
                   200000,300000,500000,1000000,2000000)
priceBreaks <- c(seq(from=0,by=20000, to=100000),seq(from=125000,by=25000, to=300000),
                 seq(from=350000,by=50000, to=600000),seq(from=700000,by=100000, to=3500000))


#clean the data
dataclean<-cleanData(dataraw)
#create data features
data<-featureCreation(dataclean,mileageBreaks,priceBreaks)
#separate the purchases from the training data now that they are both in the same format


#----------------create the model cars we want the price for 
purchaseVehicle<-data[1:13,]
purchaseVehicle$Price=c(0,0,0,0,0,0,0,0,0,0,0,0,0)
purchaseVehicle$Year=c(2001,2002,2003,2004,2005,2006,2007,2008,2009,2010,2013,2007,2007)
purchaseVehicle$Mileage=c(120000,150000,200000,120000,120000,120000,120000,
                          120000,120000,120000,68000,130000,125000)
purchaseVehicle$Make=c('AUDI','AUDI','AUDI','AUDI','AUDI','AUDI','AUDI','AUDI',
                       'AUDI','AUDI','AUDI','MERCEDES-BENZ','VOLKSWAGEN')
purchaseVehicle$Model=c('A4','A4','A4','A4','A4','A4','A4','A4','A4','A4','A4','C-CLASS','POLO')
purchaseVehicle$Engine=c(1.8,1.8,1.8,1.8,1.8,1.8,1.8,1.8,1.8,1.8,1.8,2.4,1.6)

purchaseVehicle<-featureCreation(purchaseVehicle,mileageBreaks,priceBreaks)

labelsMake <- levels(data$Make)
labelsModel <- levels(data$Model)
labelsEngine <- levels(data$Engine)
labelsMileageFeat <- levels(data$MileageFeat)
purchaseVehicle$Make<- factor(purchaseVehicle$Make, levels=labelsMake)
purchaseVehicle$Model<- factor(purchaseVehicle$Model, levels=labelsModel)
purchaseVehicle$Engine<- factor(purchaseVehicle$Engine, levels=labelsEngine)
purchaseVehicle$MileageFeat<- factor(as.character(purchaseVehicle$MileageFeat), levels=labelsMileageFeat)
#--------------------------------------------------


formula <-PriceFeat ~  Year + Make + Model+ MileageFeat + Engine
set.seed(1234)
ind <- sample(3, nrow(data), replace=TRUE, prob=c(0.7,0.2,0.1))
training <- data[ind==1, ]
validation<- data[ind==2, ]
test <- data[ind==3, ]

#interpolated value


#--------------BOOSTED FITTING
fitBoost<-gbm(formula,data= as.data.frame(training), n.trees=50,interaction.depth=9, shrinkage=0.1,n.minobsinnode=10,distribution="gaussian")
PredictionBoost <- predict(fitBoost, as.data.frame(training),n.trees=50)
ValidationBoost <- predict(fitBoost, as.data.frame(validation),n.trees=50)
PricesOfInterest <- predict(fitBoost, as.data.frame(purchaseVehicle),n.trees=50)

plot(as.numeric(ValidationBoost),validation$PriceFeat)


x<-rbind(floor(PricesOfInterest),ceiling(PricesOfInterest))
y<-rbind(priceBreaks[x[1,]],priceBreaks[x[2,]])

a<-predict(lm(y~x),newdata=list(x=PricesOfInterest))

tt<-1:length(priceBreaks)
b<-approx(x=tt, y=priceBreaks, PricesOfInterest, method = "linear")




compared<-as.data.frame(cbind(round(ValidationBoost),validation$PriceFeat,validation$Make))
colnames(compared)<-c("Predict","Actual","Make")
ggplot(compared, aes(x=Predict, y=Actual, colour=Make)) +
  geom_point(alpha=0.1) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("Predicted vs Actual Car Prices]") + xlab('Actual Price Range') + ylab('Predicted Price Range')






fitSVM  <- svm(formula, data = as.data.frame(training), 
               type="C-classification",
               kernel="radial",
               probability=T,
               gamma=0.1,
               cost=1) 
PredictionSVM <- predict(fitSVM, as.data.frame(training), type="C-classification")
ValidationSVM <- predict(fitSVM, as.data.frame(validation), type="C-classification")
PricesOfInterestSVM <- predict(fitSVM, as.data.frame(purchaseVehicle), type="C-classification")
results.matrix <- confusionMatrix((PredictionSVM), training$PriceFeat)
accuracySVM<-results.matrix$overall[1]



fitForest<-randomForest(formula, data=training, nTree=20000)
PredictionForest <- predict(fitForest, as.data.frame(trainFeatForest))
results.matrix <- confusionMatrix((PredictionForest), trainFeatForest$Survived)
accuracyForest<-results.matrix$overall[1]


compared<-as.data.frame(cbind(as.numeric(ValidationSVM),validation$PriceFeat,validation$Make))

plot(as.numeric(ValidationSVM),validation$PriceFeat)






purchaseCar<-as.data.frame(data[nrow(data),])
PurchasePrediction <- predict(fitBoost,purchaseCar,n.trees=50)







PredictionBoost <- predict(fitBoost, newdata=as.data.frame(testFeat),n.trees=50,interaction.depth=9, shrinkage=0.1,n.minobsinnode=10,distribution="gaussian")






#training2=training[1:1000,]
#fit <- rpart(formula, data=as.data.frame(training), method="class",control=rpart.control(minsplit=100,cp=0,minbucket=10,maxcompete=0,maxsurrogate=0,usesurrogate=0,xval=0))
fit <- rpart(formula, data=as.data.frame(training), method="class")

Prediction <- predict(fit, as.data.frame(training), type = "class")
results.matrix <- confusionMatrix(Prediction, training$Price)
accuracyRpart<-results.matrix$overall[1]

pdf("rTree1.pdf")
fancyRpartPlot(fit)
dev.off()