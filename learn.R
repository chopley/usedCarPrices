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

data<-na.omit(data)
formula <-Price ~  Year + Make + Model+ Mileage + Engine 



fit <- rpart(formula, data=as.data.frame(trainFeat), method="class",control=rpart.control(minsplit=100,cp=0,minbucket=10,maxcompete=0,maxsurrogate=0,usesurrogate=0,xval=0))
Prediction <- predict(fit, as.data.frame(trainFeat), type = "class")
results.matrix <- confusionMatrix(Prediction, trainFeat$Survived)
accuracyRpart<-results.matrix$overall[1]
pdf("rTree1.pdf")
fancyRpartPlot(fit)
dev.off()