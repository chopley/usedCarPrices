library(rvest)
library(ggplot2)


source('./functions.R')

#First we pull the data from the autotrader website
#----------------------------------------------------------------------
toyota <- html_session("http://www.autotrader.co.za/used-cars/toyota")
dataToyota<-getWebPageData(toyota,"^[Toyota]",500)

mercedes <- html_session("http://www.autotrader.co.za/used-cars/mercedes-benz")
dataMercedes<-getWebPageData(mercedes,"^[Mercedes]",500)

daewoo <- html_session("http://www.autotrader.co.za/used-cars/daewoo")
dataDaewoo<-getWebPageData(daewoo,"^[Daewoo]",500)

#define the make we will use!
nissan <- html_session("http://www.autotrader.co.za/used-cars/nissan")
dataNissan<-getWebPageData(nissan,"^[Nissan]",500)

#define the make we will use!
bmw <- html_session("http://www.autotrader.co.za/used-cars/bmw")
dataBMW<-getWebPageData(bmw,"^[BMW]",500)

#define the make we will use!
renault <- html_session("http://www.autotrader.co.za/used-cars/renault")
dataRenault<-getWebPageData(renault,"^[Renault]",500)

#define the make we will use!
volvo <- html_session("http://www.autotrader.co.za/used-cars/volvo")
dataVolvo<-getWebPageData(volvo,"^[Volvo]",500)

#define the make we will use!
hyundai <- html_session("http://www.autotrader.co.za/used-cars/hyundai")
dataHyundai<-getWebPageData(hyundai,"^[Hyundai]",500)

#define the make we will use!
honda <- html_session("http://www.autotrader.co.za/used-cars/honda")
dataHonda<-getWebPageData(honda,"^[Honda]",500)

#define the make we will use!
chevrolet <- html_session("http://www.autotrader.co.za/used-cars/chevrolet")
dataChevrolet<-getWebPageData(chevrolet,"^[Chevrolet]",500)

#define the make we will use!
peugeot <- html_session("http://www.autotrader.co.za/used-cars/peugeot")
dataPeugeot<-getWebPageData(peugeot,"^[Peugeot]",500)

#define the make we will use!
ford <- html_session("http://www.autotrader.co.za/used-cars/ford")
dataFord<-getWebPageData(ford,"^[Ford]",500)

#define the make we will use!
mazda <- html_session("http://www.autotrader.co.za/used-cars/mazda")
dataMazda<-getWebPageData(mazda,"^[Mazda]",500)

#define the make we will use!
kia <- html_session("http://www.autotrader.co.za/used-cars/kia")
dataKia<-getWebPageData(kia,"^[Kia]",500)

#define the make we will use!
volkswagen <- html_session("http://www.autotrader.co.za/used-cars/volkswagen")
dataVolkswagen<-getWebPageData(volkswagen,"^[Volk]",500)

audi <- html_session("http://www.autotrader.co.za/used-cars/audi")
dataAudi<-getWebPageData(audi,"^[Audi]",500)

subaru <- html_session("http://www.autotrader.co.za/used-cars/subaru")
dataSubaru<-getWebPageData(subaru,"^[Subaru]",500)
#define the make we will use!
#kia <- html_session("http://www.autotrader.co.za/used-cars/kia")
#dataKia<-getWebPageData(mazda,"^[Kia]",2)

#define the make we will use! #Alfa needs some work because the make is two words...
#alfa <- html_session("http://www.autotrader.co.za/used-cars/alfa-romeo")
#dataAlfa<-getWebPageData(alfa,"^[Alfa]",2) 

#----------------------------------------------------------------------

#Next store the data in csv for now
#-------------------------------------------------------------------
write.csv2(dataToyota,"toyota.csv")
write.csv2(dataMercedes,"mercedes.csv")
write.csv2(dataNissan,"nissan.csv")
write.csv2(dataBMW,"bmw.csv")
write.csv2(dataKia,"kia.csv")
write.csv2(dataMazda,"mazda.csv")
write.csv2(dataFord,"ford.csv")
write.csv2(dataPeugeot,"peugeot.csv")
write.csv2(dataChevrolet,"chevrolet.csv")
write.csv2(dataHonda,"honda.csv")
write.csv2(dataHyundai,"hyundai.csv")
write.csv2(dataVolvo,"volvo.csv")
write.csv2(dataRenault,"renault.csv")
write.csv2(dataVolkswagen,"volkswagen.csv")
write.csv2(dataAudi,"audi.csv")
write.csv2(dataSubaru,"subaru.csv")

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
