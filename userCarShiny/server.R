library(shiny)
load('predict.RData')
load('purchase.RData')
load('fitBoost.RData')
library('ggplot2')
library(gbm)
library(splines)
load('data.RData')

source('./functions.R')
mileageBreaks <- c(0,10000,20000,50000,100000,120000,140000,160000,180000,
                   200000,300000,500000,1000000,2000000)
priceBreaks <- c(seq(from=0,by=20000, to=100000),seq(from=125000,by=25000, to=300000),
                 seq(from=350000,by=50000, to=600000),seq(from=700000,by=100000, to=3500000))

shinyServer(
  function(input, output) {

    output$text1 <- renderText({input$text1})
    output$text2 <- renderText({input$text2})
   
    output$text3 <- renderText({
      ##First example
      input$goButton
      purchaseVehicle$Make <-toupper(input$text1)
      purchaseVehicle$Model <-toupper(input$text2)
      purchaseVehicle$Year<-input$text3
      purchaseVehicle$Engine<-input$text4
      purchaseVehicle$Mileage<-as.numeric(input$text5)

      purchaseVehicle<-featureCreation(purchaseVehicle,mileageBreaks,priceBreaks)
      
      labelsMake <- levels(data$Make)
      labelsModel <- levels(data$Model)
      labelsEngine <- levels(data$Engine)
      labelsMileageFeat <- levels(data$MileageFeat)
      purchaseVehicle$Make<- factor(purchaseVehicle$Make, levels=labelsMake)
      purchaseVehicle$Model<- factor(purchaseVehicle$Model, levels=labelsModel)
      purchaseVehicle$Engine<- factor(purchaseVehicle$Engine, levels=labelsEngine)
      purchaseVehicle$MileageFeat<- factor(as.character(purchaseVehicle$MileageFeat), levels=labelsMileageFeat)
      PricesOfInterest <- predict(fitBoost, as.data.frame(purchaseVehicle),n.trees=50)
      x<-rbind(floor(PricesOfInterest),ceiling(PricesOfInterest))
      y<-rbind(priceBreaks[x[1,]],priceBreaks[x[2,]])
      tt<-1:length(priceBreaks)   
      
      b<-approx(x=tt, y=priceBreaks, PricesOfInterest, method = "linear")
      isolate(paste('R',floor(b$y)))})
      
    #--------------------calculate the depreciation  
    output$depreciation <- renderPlot({
      input$goButton
#      plot(1,1)
      purchaseVehicle$Make <-toupper(input$text1)
      purchaseVehicle$Model <-toupper(input$text2)
      purchaseVehicle$Year<-input$text3
      purchaseVehicle$Engine<-input$text4
      purchaseVehicle$Mileage<-as.numeric(input$text5)
      
      depreciation<-purchaseVehicle
      
      for(i in 2:5){
        depreciation<-rbind(depreciation,purchaseVehicle)
        depreciation$Year[i]<-as.numeric(as.character(depreciation$Year[i-1]))-1
      }
#       
      depreciation<-featureCreation(depreciation,mileageBreaks,priceBreaks)
      labelsMake <- levels(data$Make)
      labelsModel <- levels(data$Model)
      labelsEngine <- levels(data$Engine)
      labelsMileageFeat <- levels(data$MileageFeat)
      depreciation$Make<- factor(depreciation$Make, levels=labelsMake)
      depreciation$Model<- factor(depreciation$Model, levels=labelsModel)
      depreciation$Engine<- factor(depreciation$Engine, levels=labelsEngine)
      depreciation$MileageFeat<- factor(as.character(depreciation$MileageFeat), levels=labelsMileageFeat)
      PricesOfInterest <- predict(fitBoost, as.data.frame(depreciation),n.trees=50)
      x<-rbind(floor(PricesOfInterest),ceiling(PricesOfInterest))
      y<-rbind(priceBreaks[x[1,]],priceBreaks[x[2,]])
      tt<-1:length(priceBreaks)   

      b<-approx(x=tt, y=priceBreaks, PricesOfInterest, method = "linear")
#       
      Year<-as.numeric(as.character(depreciation$Year))
      Price<-as.numeric(b$y)
      data<-data.frame(Year,Price)
#      plot(Year,Price)
       
       ggplot(data, aes(x=Year, y=Price)) +
                  geom_point(alpha=1) +
                  geom_line()+
                  stat_smooth(method = "rlm", formula = y ~ ns(x,3),size=2) +
                #  geom_smooth(alpha=.2, size=2,method = "rlm",formula = y ~ ns(x,3)) +
                  ggtitle("Price of Car vs Year\n[Scraped from AutoTrader.co.za.]") + scale_x_reverse()+ ylab('Price')

    })
       
       
#       
#       

   
#       output$depreciation <- renderPlot({
#       
#       
#       })
    
      ##Another example, uncomment to see        
      #  if (input$goButton == 0) "You have not pressed the button"
      #  else if (input$goButton == 1) "you pressed it once"
      #  else "OK quit pressing it"
    
  }
)