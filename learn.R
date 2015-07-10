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


data<-rbind(dataToyota,dataMercedes,dataNissan,dataBMW,dataKia,dataMazda,dataFord,dataPeugeot,dataChevrolet,dataHonda,dataHyundai,
            dataVolvo,dataRenault,dataVolkswagen,dataAudi,dataSubaru)

purchaseVehicle<-data[1,]
purchaseVehicle$Price=0
purchaseVehicle$Year=2002
purchaseVehicle$Mileage=120000
purchaseVehicle$Make="AUDI"
purchaseVehicle$Model="A4"
purchaseVehicle$Engine=1.8

data<-rbind(data,purchaseVehicle)



data<-na.omit(data)

data<-data[-which(data$Make=="Featured",),]


nUnique<-unique(data$Make)
for(i in 1:length(nUnique)){
  if(nrow(data[data$Make==nUnique[i],])<50){
    data<-data[-which(data$Make==nUnique[i],),]
  }
}
data$Make<-droplevels(data$Make)

mileageBreaks <- c(0,10000,20000,50000,100000,120000,140000,160000,180000,200000,300000,500000,1000000,2000000)
data$MileageFeat<-cut(data$Mileage,mileageBreaks,labels=FALSE,include.lowest = TRUE)

data$ModelFeat<-as.factor(data$Model)
nUnique<-unique(data$ModelFeat)
for(i in 1:length(nUnique)){
  if(nrow(data[data$ModelFeat==nUnique[i],])<50){
    data<-data[-which(data$ModelFeat==nUnique[i],),]
  }
}
data$ModelFeat<-droplevels(data$ModelFeat)


data$EngineFeat<-as.factor(data$Engine)
nUnique<-unique(data$EngineFeat)
for(i in 1:length(nUnique)){
  if(nrow(data[data$EngineFeat==nUnique[i],])<50){
    data<-data[-which(data$EngineFeat==nUnique[i],),]
  }
}

data$EngineFeat<-droplevels(data$EngineFeat)

priceBreaks <- c(seq(from=0,by=20000, to=100000),seq(from=125000,by=25000, to=300000),seq(from=350000,by=50000, to=600000),seq(from=700000,by=100000, to=3500000))
data$PriceFeat<-cut(data$Price,priceBreaks,labels=FALSE,include.lowest = TRUE)


formula <-PriceFeat ~  Year + Model+ MileageFeat + EngineFeat

set.seed(1234)
ind <- sample(3, nrow(data), replace=TRUE, prob=c(0.7,0.2,0.1))
training <- data[ind==1, ]
validation<- data[ind==2, ]
test <- data[ind==3, ]




#-----------------------------------
#fitNnet <- svm(formula, data = as.data.frame(training), 
#               type="C-classification",
#               size=5,
#               decay=0.005) 
#PredictionNnet <- predict(fitNnet, data=as.data.frame(trainFeat), type="C-classification")
#results.matrix <- confusionMatrix((PredictionNnet), trainFeat$Survived)
#accuracyNnet<-results.matrix$overall[1]



fitBoost<-gbm(formula,data= as.data.frame(training), n.trees=50,interaction.depth=9, shrinkage=0.1,n.minobsinnode=10,distribution="gaussian")
PredictionBoost <- predict(fitBoost, as.data.frame(training),n.trees=50)
ValidationBoost <- predict(fitBoost, as.data.frame(validation),n.trees=50)
#results.matrix <- confusionMatrix(round(PredictionBoost), validation$PriceFeat)
#accuracyBoost<-results.matrix$overall[1]

compared<-as.data.frame(cbind(round(ValidationBoost),validation$PriceFeat,validation$Make))
colnames(compared)<-c("Predict","Actual","Make")
ggplot(compared, aes(x=Predict, y=Actual, colour=Make)) +
  geom_point(alpha=0.1) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("Predicted vs Actual Car Prices]") + xlab('Actual Price Range') + ylab('Predicted Price Range')

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