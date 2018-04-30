
# 2.2. Create Time Series

Data_ByYearts<-Data[c(1,2,3,6,7,8,12)] %>%
  group_by(year(Date)) %>% summarise_all(funs(mean(., na.rm = TRUE))) %>% 
  ungroup() 

Data_ByMonthts<-Data[c(1,2,3,6,7,8,12)] %>%
  group_by(year(Date), month(Date)) %>% summarise_all(funs(mean(., na.rm = TRUE))) %>%
  ungroup()  

Data_ByWeekts<-Data[c(1,2,3,6,7,8,12)] %>%
  group_by(year(Date), week(Date)) %>% summarise_all(funs(mean(., na.rm = TRUE))) %>%
  ungroup() 

Data_ByDayts<-Data[c(1,2,3,6,7,8,12)] %>%
  group_by(year(Date), week(Date),day(Date)) %>% summarise_all(funs(mean(., na.rm = TRUE))) %>%
  ungroup()

tsYear<-ts(Data_ByYearts,frequency=1, start=2007, end=2010)
tsMonth<-ts(Data_ByMonthts,frequency =  12, start=c(2007,1),  end=c(2010,11))
tsWeek<-ts(Data_ByWeekts, frequency = 52, start=c(2007,1),    end=c(2010,48)) 
tsDay<-ts(Data_ByDayts, frequency = 356, start=c(2007,1),    end=c(2010,300)) 

# 2.3. Create Models
GraphsWeeks<-function(){
  
  Models<-list(ActiveEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),
               ReactiveEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),
               KitchenEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),                 
               LaundryEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),
               EWACEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())))
  
  Variables<-c("ActiveEnergy", "ReactiveEnergy", "Kitchen", "Laundry", "EWAC") 
  Methods<-c("naive", "snaive","holtwinters", "arima")
  
  for(i in 1:length(Variables)){ 
    for(j in 1:length(Methods)){
      train<-window(tsWeek[,(Variables[i])], end=c(2010,7))
      test<-window(tsWeek[,(Variables[i])], start=c(2010,8))  
      
      if(Methods[j]=="naive"){
        Fit<-naive(train)
        Forecast<-forecast(Fit, h=10)}
      
      if(Methods[j]=="snaive"){
        Fit<-snaive(train)
        Forecast<-forecast(Fit, h=10)}
      
      if(Methods[j]=="holtwinters"){
        Fit<-HoltWinters(train, seasonal="additive")
        Forecast<-forecast(Fit, h=10) }
      
      if(Methods[j]=="arima"){
        Fit<-auto.arima(train)
        Forecast<-forecast(Fit, h=10) }
      
      Models[[i]][[j]][[1]]<-autoplot(train, series="Training Data") +
        autolayer(fitted(Fit, h=37), series="Train Pred.") +
        autolayer(Forecast, series="Testing data") +
        autolayer(test, series="Test Pred.")
      
      Models[[i]][[j]][[2]]<-accuracy(Forecast, test)
    }}
  return(Models)
  
}

GraphsMonth<-function(){
  
  Models<-list(ActiveEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),
               ReactiveEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),
               KitchenEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),                 
               LaundryEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())),
               EWACEnergy=list(Naive=list(Model=list(), Error=list()), SNaive=list(Model=list(), Error=list()), HoltWinters=list(Model=list(), Error=list()), Arima=list(Model=list(), Error=list())))
  
  Variables<-c("ActiveEnergy", "ReactiveEnergy", "Kitchen", "Laundry", "EWAC") 
  Methods<-c("naive", "snaive","holtwinters", "arima")
  
  for(i in 1:length(Variables)){ 
    for(j in 1:length(Methods)){
      train<-window(tsMonth[,(Variables[i])], end=c(2010,2))
      test<-window(tsMonth[,(Variables[i])], start=c(2010,3))  
      
      if(Methods[j]=="naive"){
        Fit<-naive(train)
        Forecast<-forecast(Fit, h=10)}
      
      if(Methods[j]=="snaive"){
        Fit<-snaive(train)
        Forecast<-forecast(Fit, h=10)}
      
      if(Methods[j]=="holtwinters"){
        Fit<-HoltWinters(train, seasonal="additive")
        Forecast<-forecast(Fit, h=10) }
      
      if(Methods[j]=="arima"){
        Fit<-auto.arima(train)
        Forecast<-forecast(Fit, h=10) }
      
      Models[[i]][[j]][[1]]<-autoplot(train, series="Training Data") +
        autolayer(fitted(Fit, h=9), series="Train Pred.") +
        autolayer(Forecast, series="Testing data") +
        autolayer(test, series="Test Pred.")
      
      Models[[i]][[j]][[2]]<-accuracy(Forecast, test)
    }}
  return(Models)
  
}

ListGraphsMonhs<-GraphsMonth()
ListGraphsWeeks<-GraphsWeeks()



