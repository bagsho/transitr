#load libraries
library(readxl)
library(dplyr)
library(hms)
library(ggplot2)

#get data
path<-"C:/danışmanlık/metro istanbul/kesit hacimleri/kesit_sonuçlar/14.02.2019/3-son veri/Kesit_20180409_V3.xlsx"
data<-read_excel(path, col_types = c("guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess", "guess","numeric"))

#clean data
data<-data%>%
  mutate(
    LineName=as.factor(LineName),
    TimeTableTime=parse_hms(substr(TimeTableTime, 1, 8)),
    Direction=ifelse(is.na(FirstDirectionCount),1,2),
    Count=ifelse(is.na(FirstDirectionCount),SecondDirectionCount,FirstDirectionCount)
    )%>%
  select(LineName,FromStationOrder,ToStationOrder ,TimeTableTime,TimeTableOrderNo,Direction,Count)%>%
  arrange(LineName,Direction,TimeTableTime,FromStationOrder)
data_filtered<-data%>%
  filter(LineName=="M4" &
         Direction==2 &
           TimeTableTime==as_hms("08:33:32"))
ggplot(data_filtered,aes(x=FromStationOrder,y=Count))+geom_line()

