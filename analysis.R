library(rvest)
library(ggplot2)
library(doParallel)
library(foreach)



source('./functions.R')


nCores<-detectCores()
cl<-makeCluster(nCores)
registerDoParallel(cl)
getDoParWorkers()
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
getDoParName()


#First we pull the data from the autotrader website
#----------------------------------------------------------------------
toyota<-"http://www.autotrader.co.za/used-cars/toyota"
mercedes <- "http://www.autotrader.co.za/used-cars/mercedes-benz"
daewoo <- "http://www.autotrader.co.za/used-cars/daewoo"
nissan <- "http://www.autotrader.co.za/used-cars/nissan"
bmw <- "http://www.autotrader.co.za/used-cars/bmw"
renault <-"http://www.autotrader.co.za/used-cars/renault"
volvo <- "http://www.autotrader.co.za/used-cars/volvo"
hyundai <- "http://www.autotrader.co.za/used-cars/hyundai"
honda <- "http://www.autotrader.co.za/used-cars/honda"
chevrolet <- "http://www.autotrader.co.za/used-cars/chevrolet"
peugeot <- "http://www.autotrader.co.za/used-cars/peugeot"
ford <- "http://www.autotrader.co.za/used-cars/ford"
mazda <- "http://www.autotrader.co.za/used-cars/mazda"
kia <- "http://www.autotrader.co.za/used-cars/kia"
volkswagen <- "http://www.autotrader.co.za/used-cars/volkswagen"
audi <- "http://www.autotrader.co.za/used-cars/audi"
subaru <- "http://www.autotrader.co.za/used-cars/subaru"





webPages<-c(toyota,mercedes,daewoo,nissan,bmw,renault,volvo,hyundai,honda,chevrolet,peugeot,ford,mazda,kia,volkswagen,audi,subaru)
makeNames<-c("^[Toyota]","^[Mercedes]","^[Daewoo]","^[Nissan]","^[BMW]","^[Renault]","^[Volvo]","^[Hyundai]",
             "^[Honda]","^[Chevrolet]","^[Peugeot]","^[Ford]","^[Mazda]","^[Kia]","^[Volk]","^[Audi]","^[Subaru]")
fileNames<-c("toyota.csv","mercedes.csv","daewoo.csv","nissan.csv",'bmw.csv','renault.csv','volvo.csv','hyundai.csv','honda.csv',
             'chevrolet.csv','peugeot.csv','ford.csv','mazda.csv','kia.csv','volkswagen.csv','audi.csv','subaru.csv')
dataVariables<-c(dataToyota,dataMercedes,dataDaewoo,dataNissan,dataBmw,dataRenault,dataVolvo,dataHyundai,
                 dataHonda,dataChevrolet,dataPeugeot,dataFord,dataMazda,dataKia,dataVolkswagen,dataAudi,dataSubaru)


#lets go parallel and get all the data!
#remmeber foreach requires you to load packages again1
foreach(i=1:17, .options.multicore=mcoptions, .packages='rvest') %dopar% {
  dataFrame <- getWebPageData(webPages[i],makeNames[i],1,500)
  write.csv2(dataFrame,fileNames[i])
}
stopCluster(cl)

formula <-Survived ~  Pclass + Sex + FarePassenger+ Age + Embarked + EthnicFeat + TitleFeat + GroupFeat + MFCoupleFeat + FamilyFeat + CabinFeat




#define the make we will use! #Alfa needs some work because the make is two words...
#alfa <- html_session("http://www.autotrader.co.za/used-cars/alfa-romeo")
#dataAlfa<-getWebPageData(alfa,"^[Alfa]",2) 
#dataToyota<-getWebPageData(toyota,"^[Toyota]",1,2)
#dataMercedes<-getWebPageData(mercedes,"^[Mercedes]",1,500)
#dataDaewoo<-getWebPageData(daewoo,"^[Daewoo]",1,500)
#dataNissan<-getWebPageData(nissan,"^[Nissan]",1,500)
#dataBMW<-getWebPageData(bmw,"^[BMW]",1,500)
#dataRenault<-getWebPageData(renault,"^[Renault]",1,500)
#dataVolvo<-getWebPageData(volvo,"^[Volvo]",1,500)
#dataHyundai<-getWebPageData(hyundai,"^[Hyundai]",1,500)
#dataHonda<-getWebPageData(honda,"^[Honda]",1,500)
#dataChevrolet<-getWebPageData(chevrolet,"^[Chevrolet]",1,500)
#dataPeugeot<-getWebPageData(peugeot,"^[Peugeot]",1,500)
#dataFord<-getWebPageData(ford,"^[Ford]",1,500)
#dataMazda<-getWebPageData(mazda,"^[Mazda]",1,500)
#dataKia<-getWebPageData(kia,"^[Kia]",1,500)
#dataVolkswagen<-getWebPageData(volkswagen,"^[Volk]",1,500)
#dataAudi<-getWebPageData(audi,"^[Audi]",1,500)
#dataSubaru<-getWebPageData(subaru,"^[Subaru]",1,500)
#----------------------------------------------------------------------

#Next store the data in csv for now
#-------------------------------------------------------------------
#write.csv2(dataToyota,"toyota.csv")
#write.csv2(dataMercedes,"mercedes.csv")
#write.csv2(dataNissan,"nissan.csv")
#write.csv2(dataBMW,"bmw.csv")
#write.csv2(dataKia,"kia.csv")
#write.csv2(dataMazda,"mazda.csv")
#write.csv2(dataFord,"ford.csv")
#write.csv2(dataPeugeot,"peugeot.csv")
#write.csv2(dataChevrolet,"chevrolet.csv")
#write.csv2(dataHonda,"honda.csv")
#write.csv2(dataHyundai,"hyundai.csv")
#write.csv2(dataVolvo,"volvo.csv")
#write.csv2(dataRenault,"renault.csv")
#write.csv2(dataVolkswagen,"volkswagen.csv")
#write.csv2(dataAudi,"audi.csv")
#write.csv2(dataSubaru,"subaru.csv")

#---------------------------------------------------------------------------







#Now for plotting
#---------------------------------------------------------------------------
modelName<-'GOLF'
data<-dataVolkswagen
x<-data$Year[data$Model==modelName]
y<-data$Price[data$Model==modelName]/1000
qplot(x, y, geom=c("point", "smooth"),
      method="loess", formula=y~x, 
      main="Price with Age",
      xlab="Model Year", ylab="Price [R1000]",xlim=c(2016,2000))

data<-dataAudi[dataAudi$Model=='A3',]
data<-rbind(data,dataVolkswagen[dataVolkswagen$Model=='GOLF',])
#data<-rbind(data,dataBMW[dataBMW$Model=='3',])
data<-rbind(data,dataToyota[dataToyota$Model=='COROLLA',])
data<-rbind(data,dataToyota[dataToyota$Model=='ETIOS',])
data<-rbind(data,dataVolkswagen[dataVolkswagen$Model=='POLO',])
data<-rbind(data,dataMercedes[dataMercedes$Model=='C-CLASS',])

#data$Model[data$Model=='3']<-'3 Series'

ggplot(data, aes(x=Year, y=Price/1000, colour=Model)) +
  geom_point(alpha=0) +
  geom_smooth(alpha=.2, size=1) +
  ggtitle("Price of Car vs Year\n[Scraped from AutoTrader.co.za.]") + scale_x_reverse( lim=c(2016,2005)) + ylim(c(100,450)) + ylab('Price\n[R1000s]')

ggsave(filename="myPlot.pdf",width=12, height=8)
ggsave(filename="myPlot.png",width=12, height=8)

#Now for plotting
#---------------------------------------------------------------------------

data<-dataVolkswagen[dataVolkswagen$Model=='TIGUAN',]
data<-rbind(data,dataNissan[dataNissan$Model=='QASHQAI',])
data<-rbind(data,dataNissan[dataNissan$Model=='X-TRAIL',])
data<-rbind(data,dataToyota[dataToyota$Model=='FORTUNER',])
data<-rbind(data,dataBMW[dataBMW$Model=='X5',])
data<-rbind(data,dataVolkswagen[dataVolkswagen$Model=='POLO',])
#data<-rbind(data,dataSubaru[dataSubaru$Model=='FORESTER',])

#data$Model[data$Model=='3']<-'3 Series'

ggplot(data, aes(x=Year, y=Price/1000, colour=Model)) +
  geom_point(alpha=0.0) +
  geom_smooth(method='loess',alpha=.2, size=2) +
  ggtitle("Price of Car vs Year\n[Scraped from AutoTrader.co.za.]") + scale_x_reverse( lim=c(2016,2005)) + ylim(c(100,800)) + ylab('Price\n[R1000s]')

ggsave(filename="myPlot.pdf",width=12, height=8)
ggsave(filename="myPlot.png",width=12, height=8)
