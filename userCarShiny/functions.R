getWebPageData <- function(webPageBase,makeInput,startPage,nPages){
  
  webPageBase<- paste(webPageBase,"?pageNumber=",as.character(startPage),sep="")
  webPage <- html_session(as.character(webPageBase))
 
 #function to get Data from Autotradr 
  #we get the ads from autoTrader for this model of Car
  print(webPageBase)
  aa<-webPage %>% 
    html_nodes(".searchResult") %>%
    html_text() 
  #store the Data
  #move to next page
  fail<-0
  data=data.frame()
  pageNumber<-startPage
  for(jPage in 1:nPages){
    print('here')
  #  aa<-rbind(aa,bb)
  #define the make we will use!
 for(i in 1:length(aa)){
    #split the listing by t
    bb<-strsplit(aa[i],"\t")
    #first we get the prices and clean them up a little
    price<-bb[[1]][grep("^[R :digit:]{2,}",bb[[1]])]
    price<-gsub("R ","",price) #remove the currency since all cars are in Rands
    price<-gsub("[\r\n ,]","",price) #remove the whitespace and nonsense
    price<-gsub("[[:alpha:]+]","",price) #remove any characters
    if(length(price)==0){
      price=NaN
    }
    
    year<- bb[[1]][grep("^[0-9]{4}",bb[[1]])]
    year<-gsub("[\r\n ]","",year) #remove the whitespace and nonsense
    year<- year[grep("^[0-9]{4}$",year)] # the year is always exactly 4 digits with nothing before or after
    if(length(year)==0){
      year=NaN
    }
    
    #we extract the make and model information using the model we are currently looking at
    makeModel<-bb[[1]][grep(makeInput,bb[[1]])]
    #assume that the first
    splitMakeModel<-strsplit(makeModel[1]," ")
    make<-splitMakeModel[[1]][1]
    make<-gsub("[\r\n ]","",make) #remove the whitespace and nonsense
    if(length(make)==0){
      make=NaN
    }
    
    
    model<-splitMakeModel[[1]][2]
    model<-gsub("[\r\n ]","",model) #remove the whitespace and nonsense
    if(length(model)==0){
      model=NaN
    }
    
    
    mileage<- bb[[1]][grep("km",bb[[1]])]
    mileage<-gsub("[\r\n ,]","",mileage) #remove the whitespace commas and km
    mileage<- mileage[grep("km$",mileage)]
    mileage<-gsub("[km$]","",mileage) #remove the whitespace commas and km
    if(length(mileage)==0){
      mileage=NaN
    }
    
    engine<-bb[[1]][grep("^[0-9]{1}.[0-9] L",bb[[1]])]  
    engine<-gsub("[ L \r\n]","",engine) #remove the whitespace commas and km
    if(length(engine)==0){
      engine=NaN
    }
    # enginecc<-bb[[1]][grep("^[0-9]{4}",bb[[1]])]  
    #  engine<-gsub("[ L \r\n]","",engine) #remove the whitespace commas and km
    #  if(length(engine)==0){
    #    engine=NaN
    #  }
    
    try(
      thisRow<-data.frame(as.numeric(price),as.numeric(year),as.character(make),as.character(model),as.numeric(mileage),as.numeric(engine),as.numeric(pageNumber))
    )
    
    data<-rbind(data,thisRow)
  }
 
 tryCatch({
   webPage<-webPage%>% follow_link("Next")},
   error= function(err){nPages=jPage}      )#horrible hack to get out of the for loop cause I couldn't work out how to do this with the try catch
 
   aa<-webPage %>% 
   html_nodes(".searchResult") %>%
   html_text()
   pageNumber=pageNumber+1
 
  

  
    }
 colnames(data)<-c("Price","Year","Make","Model","Mileage","Engine","AutoTraderPage")
 
 return(data)
}

cleanData <- function(data){
  #remove the data with na in in
  data<-na.omit(data)
  #remove the date that has Featured in it- this is a problem with the scraping function
  data<-data[-which(data$Make=="Featured",),]
  nUnique<-unique(data$Make)
  for(i in 1:length(nUnique)){
    if(nrow(data[data$Make==nUnique[i],])<50){
      data<-data[-which(data$Make==nUnique[i],),]
    }
  }
  data$Make<-droplevels(data$Make)
  
 
  
  data$Model<-as.factor(data$Model)
  nUnique<-unique(data$Model)
  for(i in 1:length(nUnique)){
    if(nrow(data[data$Model==nUnique[i],])<50){
      data<-data[-which(data$Model==nUnique[i],),]
    }
  }
  data$Model<-droplevels(data$Model)
  
  
  data$Engine<-as.factor(data$Engine)
  nUnique<-unique(data$Engine)
  for(i in 1:length(nUnique)){
    if(nrow(data[data$Engine==nUnique[i],])<50){
      data<-data[-which(data$Engine==nUnique[i],),]
    }
  }
  
  data$Engine<-droplevels(data$Engine)

  
  return(data)
  
}

featureCreation <-function(data,mileageBreaks,priceBreaks){
  
  data$MileageFeat<-as.factor(cut(data$Mileage,mileageBreaks,labels=FALSE,include.lowest = TRUE))
  data$PriceFeat<-as.factor(cut(data$Price,priceBreaks,labels=FALSE,include.lowest = TRUE))
  data$MileageFeat<-as.factor(data$MileageFeat)
  data$PriceFeat<-as.factor(data$PriceFeat)
  data$Make<-as.factor(data$Make)
  data$Model<-as.factor(data$Model)
  data$Engine<-as.factor(data$Engine)
  data$Year<-as.factor(data$Year)
  
  
  return(data)
  
}