
######## Load libraries and set working directory ######## 

#Load Libraries: p_load can install, load,  and update packages
if(require("pacman")=="FALSE"){
  install.packages("pacman")
} 


pacman::p_load(dplyr, tidyr, tibble, lubridate, zoo, ggplot2, scales, plyr, shiny, reshape2,
               sqldf, xlsx, data.table, forecast, imputeTS, timeDate, bsts, readxl, readr,shinydashboard,
               shiny, xts,
               highcharter
)

setwd("C:/Users/Patty/OneDrive/Documents/Ubiqum/Task 5/Data/")

HHPC<-read.csv("household_power_consumption.txt", sep=";", na.strings="?")

as_tibble(HHPC)

names(HHPC)

#Create DateTime column

HHPC$DateTime<-strptime(paste(as.character(HHPC$Date),as.character(HHPC$Time)),
                        "%d/%m/%Y %H:%M:%S", tz="UTC")

HHPC$DateTime<-as.POSIXct(HHPC$DateTime, tz="UTC", format="%d/%m/%Y %H:%M:%S")

############################# Daylight Savigs Correction #############################


#Daylight Savigs start and end dates

DST_dates<-data.frame(Start=c( "2007-3-25 02:00:00", "2008-3-30 02:00:00","2009-3-29 02:00:00","2010-3-28 02:00:00"),
                      End=c( "2007-10-28 1:59:00", "2008-10-26 1:59:00","2009-10-25 1:59:00", "2010-10-31 1:59:00") ,stringsAsFactors=FALSE)
DST_dates$Start<-as_datetime(DST_dates$Start)
DST_dates$End<-as_datetime(DST_dates$End)


#Add one hour to those dates falling within the DST_dates intervals

HHPC<-HHPC %>%
  mutate(DateTime = ifelse(HHPC$DateTime %in% unlist(Map(`:`, DST_dates$Start, DST_dates$End)),
                           DateTime +3600, DateTime))


HHPC$DateTime<-as_datetime(HHPC$DateTime,origin= "1970-01-01", tz="UTC")


str(HHPC$DateTime)#Check that DateTime has POSIXct format


# Check that timestamps have been re-labeled properly

library(stringr)
DST_check_start<-HHPC %>%
  filter(str_detect(DateTime, "2009-03-29")) #OK!!!

DST_check_end<-HHPC %>%
  filter(str_detect(DateTime, "2009-10-25")) #OK!!!


rm(DST_check_start,  DST_check_end)

############################# Create Date, Week Day and Season #############################

#Create Date again (it has changed because of daylight savings)and drop Time

HHPC$Date<-as_date(HHPC$DateTime)

HHPC<-HHPC[ , !(names(HHPC)=='Time')]

#Create week day

HHPC$Wday<-lubridate::wday(HHPC$DateTime, label=TRUE, abbr=TRUE)

#Create season

# Add a new column with the season

winter<-as_date("2008-12-21")
spring <- as_date("2008-03-20")
summer <- as_date("2008-06-21")
fall <- as_date("2008-09-22")


mydates <- as_date(format(HHPC_ts$Date, "%2008-%m-%d"))

HHPC_ts$Season<-ifelse(mydates >= as_date(spring) & mydates < as_date(summer), "spring",
                       ifelse (mydates >= summer & mydates < fall, "summer",
                               ifelse (mydates >= fall & mydates < winter, "fall",
                                       
                                       "winter")))
HHPC_ts<-HHPC_ts %>%
  mutate(Season= as.factor(Season))

############################### Variable transformation ###############################

# Rename sub-emeters

HHPC<-HHPC%>% dplyr::rename(ActiveEnergy=Global_active_power, ReactiveEnergy=Global_reactive_power, Intensity=Global_intensity, Kitchen=Sub_metering_1, Laundry=Sub_metering_2, EWAC=Sub_metering_3) 

#Convert kW to Watt-hour

HHPC<-HHPC %>% mutate (ActiveEnergy= ActiveEnergy*1000/60,
                       ReactiveEnergy=ReactiveEnergy *1000/60)

# We create two new variables: OtherRooms (the active energy consumed in other places) and Power_Factor (the efficiency of 
# electric system)

HHPC<-HHPC %>% mutate(OtherRooms=ActiveEnergy - Kitchen -Laundry - EWAC,
                      PowerFactor=ActiveEnergy/(Voltage*Intensity))

#Voltage*Intensity=Apparent Power


############################# Data quality #############################


### Negative Other submeters

nrow(HHPC)

#Check that sub_metering_4 does not take on negative values

length(HHPC$OtherRooms[which(HHPC$OtherRooms<0)])#1,050

length(HHPC$OtherRooms[which(HHPC$OtherRooms<0)])/nrow(HHPC)*100 #0.05% of the records

#Store negative residual sub-meters in a table and save it to a csv

negative_GAP<-HHPC[which(HHPC$OtherRooms<0),]

#write.csv(negative_GAP, "negative_GAP.csv")

rm(negative_GAP)

#Set negative residual sub-meters to 0

HHPC$OtherRooms<-ifelse(HHPC$OtherRooms<0, 0,HHPC$OtherRooms)#set negative residual sub-meters to 0.

#Explore the 10 residual sub-meter values preceding the negative value

### Missing data analysis

summary(HHPC)

#25,979 NA's


missing<-HHPC[is.na(HHPC$ActiveEnergy),]

nrow(missing)/nrow(HHPC)*100 # approximately 1.25% of the records


#missing$Time<-(format(missing$DateTime, "%H:%M:%S"))

###################### CalendarHeatmap ######################

missing_calendar<-HHPC

missing_calendar$d_miss<-ifelse(is.na(missing_calendar$ActiveEnergy), 1, 0)

missing_calendar<-missing_calendar %>% 
  group_by(Date) %>%
  dplyr::summarize(missing = sum(d_miss))


# Download the 'calendarHeat' function from revolutionanalytics.com
source("http://blog.revolutionanalytics.com/downloads/calendarHeat.R")

CalMissHeatMap<-calendarHeat(missing_calendar$Date, missing_calendar$missing, 
                             varname="Missing Data", color="w2b")

rm(calendarHeat)
rm(missing_calendar)

##################### Missing Analysis###################


missing_count<- sqldf("select distinct Date, Wday, count(*) as Minutes, round((count(Date)/60),2) as Hours,
                      min(DateTime) as Start, max(DateTime) as End
                      from missing
                      group by Date, Wday")

# Plot Missing distribution

#(]

ggplot(data=missing_count, aes(missing_count$Hours)) + 
  geom_histogram(aes(y =..count..), 
                 binwidth=1,
                 center=-0.5,
                 col="black", 
                 fill="#a9a9a9") + 
  xlab("Duration (in hours)")+
  ggtitle("Missing Distribution by Duration")+
  scale_x_continuous(breaks = seq(0, 24, by =1))+
  scale_y_continuous(breaks = scales::pretty_breaks(n = 5))+
  theme(plot.title = element_text(hjust = 0.5))+
  stat_bin(binwidth=1, size=3, center=-0.5, geom="text", aes(label=ifelse(..count..== 0, "", paste0(round(..count../sum(..count..) * 100, 0), "%"))),
           vjust=-0.5)


#Determine if power interruption was intentional:

missing_count<-missing_count %>%
  arrange(Date, Wday) %>%
  mutate(
    
    Start=as_datetime(Start),
    End=as_datetime(End),
    
    TimeDiff_lag =ifelse(is.na(difftime(Start , lag(End))), "",difftime(Start , lag(End))) ,
    TimeDiff_lead =ifelse(is.na(difftime(lead(Start) , End)), "",difftime(lead(Start) , End))
    
  )

#write.xlsx(missing_count, file = "missing_correction.xlsx", sheetName="Raw")


missing_count<-missing_count %>%
  
  dplyr:: mutate(Miss_trigger=ifelse(missing_count$Hours==24 |
                                       (missing_count$TimeDiff_lag==1 & (lag(missing_count$Hours)==24 | missing_count$Hours+lag(missing_count$Hours)>=24))|
                                       (missing_count$TimeDiff_lead==1 & (lead(missing_count$Hours)==24 | missing_count$Hours+lead(missing_count$Hours)>=24)),
                                     "Intentional", "Outage")) #%>%


#Split missing_count into two helper tables depending on whether the power interruption
#has been classified as intentional or random (outage).

split_missings<-split(missing_count, missing_count$Miss_trigger)


list2env(split_missings, envir = .GlobalEnv)


#write.xlsx(missing_count, file = "missing_correction.xlsx", sheetName="Trigger", append=TRUE)

#Perform a left join between the original HHPC table and missing_count table in order to
#set to 0, in the former, those power interruptions that have been determined to be
#caused deliberately in the later.

HHPC$Miss_trigger<-ifelse(HHPC$DateTime %in% unlist(Map(`:`, Intentional$Start, Intentional$End)),
                          'Intentional',
                          ifelse(HHPC$DateTime %in% unlist(Map(`:`, Outage$Start, Outage$End)),
                                 'Outage',
                                 ""))

#View(HHPC[which(HHPC$Miss_trigger=="Intentional"),])


rm(Intentional)
rm(Outage)
rm(missing)
rm(split_missings)

#Check that we have populated the Miss_trigger for the 82 distinct dates
#with missing Global active power.

length(unique(HHPC$Date[which(HHPC$Miss_trigger!="")]))#82


############################ Missing imputation ###########################

#Impute missings with the average of non-missing observations by group:
#Bank holiday, weekend, weekday or vigil (both Fridays and any day before a bank holiday)

HHPC_ts<-HHPC


#na.kalman(HHPC_ts, model="auto.arima", smooth=TRUE)


holidays <- c("FRAscension", "NewYearsDay", "FRFetDeLaVictoire1945", "FRBastilleDay","FRAssumptionVirginMary",
              "FRAllSaints", "FRArmisticeDay", "EasterMonday","LaborDay",
              "Pentecost", "PentecostMonday", "ChristmasDay", "EasterSunday")


#Get list of french public holidays from 2007 to 2010

FRholidayList<-do.call(rbind,
                       lapply(holidays, function(i){
                         foo <- match.fun(i)  
                         data.frame(Holiday = i,
                                    Dates = as.Date(foo(2007:2010)))
                         
                       }))

FRholidayList<-FRholidayList[order(FRholidayList$Dates),]

#Nota bene: Good Friday and St Stephen's Day (observed in Alsace and Moselle only)
#are not included

HHPC_ts<- HHPC_ts %>%
  
  mutate(Holiday_type= ifelse(Date %in% FRholidayList$Dates & !(Wday %in% c("Sat", "Sun")), "Bank Holiday",
                              ifelse(((Date+1) %in% FRholidayList$Dates & !(Wday %in% c("Sat", "Sun")))|Wday=="Fri", "Vigil",
                                     ifelse(Wday %in% c("Sat", "Sun"), "Weekend", "Weekday"))))


# Determine which column names contain NA values,
# except "Other" (residual energy), since it must be obtained
#by subtracting all sub-meterigs from Global active Power


vars <- names(HHPC_ts[,!(colnames(HHPC_ts) %in% c("OtherRooms", "Miss_trigger", "PowerFactor"))])[colSums(is.na(HHPC_ts[,!(colnames(HHPC_ts) %in% c("OtherRooms", "Miss_trigger", "PowerFactor"))])) != 0]


# Summary: mean value by column and Holiday_type

mean_imputation<-aggregate(HHPC_ts[vars], by=list(HHPC_ts$Holiday_type), FUN=mean, na.rm=TRUE) 

## replace with the mean by "Holiday_type"

HHPC_ts<-data.table(HHPC_ts)
HHPC_ts[, (vars) := lapply(vars, function(x) {
  x <- get(x)
  x[is.na(x)] <- mean(x, na.rm = TRUE)
  x
}), by = Holiday_type]

HHPC_ts<-data.frame(HHPC_ts)

#Calculate "Other" again (electrical equipment not measured in sub-meterings 1, 2 and 3)

HHPC_ts<-HHPC_ts %>% mutate(OtherRooms=ActiveEnergy - Kitchen -Laundry - EWAC)

HHPC_ts$OtherRooms[HHPC_ts$OtherRooms<0]<-0

HHPC_ts<-HHPC_ts %>% mutate (PowerFactor=ActiveEnergy/(Voltage*Intensity))


############ Electricity Cost ############

#load monthly energy prices

prices <- read_excel("Electricity_prices.xlsx", sheet="blue_option_base")
prices <-data.frame(prices)


HHPC_ts$Month<-month(HHPC_ts$Date)
HHPC_ts$Year<-year(HHPC_ts$Date)
HHPC_ts$Hour<-hour(HHPC_ts$Date)

#rm(list = setdiff(ls(), lsf.str()))


# 2.1. Create Datasets per Year, Quarter, Month, Week, Season and Day of the week

vars<-c("ActiveEnergy", "ReactiveEnergy", "Voltage", "Intensity", "Kitchen", "Laundry", "EWAC", "OtherRooms")

distinct<-distinct(HHPC_ts, Date, Year, Month)
distinct1<-dplyr::rename(distinct, Time=Month)

Data_ByDay<-HHPC_ts%>%
  mutate(ReactiveEnergy=ReactiveEnergy*60)%>%
  group_by(Date, Year, Month) %>%
  summarise_at(vars(vars), funs(sum, mean))%>%
  dplyr::select(Time=Date, Year, Month, ActiveEnergy=ActiveEnergy_sum,
                Kitchen=Kitchen_sum, Laundry=Laundry_sum, EWAC=EWAC_sum, OtherRooms=OtherRooms_sum,
                ReactiveEnergy=ReactiveEnergy_mean, Voltage=Voltage_mean, Intensity=Intensity_mean)

Data_ByDay<-merge(Data_ByDay, prices,  by=c("Year","Month"))

Data_ByDay$Date<-Data_ByDay$Time

Data_ByMonth<-Data_ByDay%>%
  group_by(Year, Month) %>%
  summarise_at(vars(vars), funs(sum, mean))%>%
  dplyr::select(Year, Time=Month, ActiveEnergy=ActiveEnergy_sum,
                Kitchen=Kitchen_sum, Laundry=Laundry_sum, EWAC=EWAC_sum,
                ReactiveEnergy=ReactiveEnergy_mean, Voltage=Voltage_mean, Intensity=Intensity_mean)

Data_ByMonth<- merge(Data_ByMonth, distinct1, by=c("Year","Time"))

prices1<-prices%>%
  dplyr::rename(Time=Month)

Data_ByMonth<-merge(Data_ByMonth, prices1,  by=c("Year","Time"))


Data_ByWeek<-Data_ByDay%>%
  group_by(Year, Month, Week=week(Data_ByDay$Date)) %>%
  summarise_at(vars(vars), funs(sum, mean))%>%
  dplyr::select(Year, Month, Time=Week, ActiveEnergy=ActiveEnergy_sum,
                Kitchen=Kitchen_sum, Laundry=Laundry_sum, EWAC=EWAC_sum,
                ReactiveEnergy=ReactiveEnergy_mean, Voltage=Voltage_mean, Intensity=Intensity_mean)


Data_ByWeek<-merge(Data_ByWeek, distinct, by=c("Year","Month"))
Data_ByWeek<-merge(Data_ByWeek, prices,  by=c("Year","Month"))


Data_ByWDay<-Data_ByDay%>%
  dplyr::rename(Date=Time)%>%
  mutate(Time=lubridate::wday(Date, label=TRUE, abbr=TRUE))


Data_ByHour<-HHPC_ts%>%
  group_by(Date,Year, Month, Hour) %>%
  summarise_at(vars(vars), funs(sum, mean))%>%
  dplyr::select(Date, Year, Month, Time=Hour, ActiveEnergy=ActiveEnergy_sum,
                Kitchen=Kitchen_sum, Laundry=Laundry_sum, EWAC=EWAC_sum,
                ReactiveEnergy=ReactiveEnergy_mean, Voltage=Voltage_mean, Intensity=Intensity_mean)

Data_ByHour<-merge(Data_ByHour, prices,  by=c("Year","Month"))


write_csv(Data_ByDay, "Data_ByDay.csv")
write_csv(Data_ByYear, "Data_ByYear.csv")
write_csv(Data_ByMonth, "Data_ByMonth.csv")
write_csv(Data_ByWeek, "Data_ByWeek.csv")
write_csv(Data_ByWDay, "Data_ByWDay.csv")
write_csv(Data_ByDay, "Data_ByHour.csv")


#Data_ByDay.csv<-read.csv("Data_ByDay.csv", sep=",", na.strings="?")
