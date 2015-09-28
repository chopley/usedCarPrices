require(rvest)
require(ggplot2)
require(doParallel)
require(foreach)

#This script is used to read the data from the autotrader website in a specified format.
#The data are then written to text file for further analysis by the learn.R function

#open up the user specific functions defined in functions.R- this has definitions of the web page format etc.
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




#create all the webpage stem URLs into a vector
webPages<-c(toyota,mercedes,daewoo,nissan,bmw,renault,volvo,hyundai,honda,chevrolet,peugeot,ford,mazda,kia,volkswagen,audi,subaru)
#create the required appending
makeNames<-c("^[Toyota]","^[Mercedes]","^[Daewoo]","^[Nissan]","^[BMW]","^[Renault]","^[Volvo]","^[Hyundai]",
             "^[Honda]","^[Chevrolet]","^[Peugeot]","^[Ford]","^[Mazda]","^[Kia]","^[Volk]","^[Audi]","^[Subaru]")
#define the filenames of the output csvs
fileNames<-c("toyota.csv","mercedes.csv","daewoo.csv","nissan.csv",'bmw.csv','renault.csv','volvo.csv','hyundai.csv','honda.csv',
             'chevrolet.csv','peugeot.csv','ford.csv','mazda.csv','kia.csv','volkswagen.csv','audi.csv','subaru.csv')



#lets go parallel and get all the data!
#remmeber foreach requires you to load packages again1
foreach(i=1:17, .options.multicore=mcoptions, .packages='rvest') %dopar% {
  dataFrame <- getWebPageData(webPages[i],makeNames[i],1,500)
  write.csv2(dataFrame,fileNames[i])
}
stopCluster(cl)


