getwd()
setwd("C:/Users/lifei/OneDrive/Desktop/Havensight Consulting/Morton Project/R scripts/Final Data/No 6 Filter Wheel")
#install.packages("outliers")
#install.packages("dplyr")
#install.packages('gridExtra')
library(outliers)
library(dplyr)
library(lubridate)
library(hms)
library(scales)
library(ggplot2)
library(gridExtra)

A=getwd()
Filename="Filter 6 dsh valve postion"
input=read.csv(paste0(Filename,".csv",collapse=NULL), header=T, stringsAsFactors = FALSE)
summary(input)
head(input)
Data_1=select(input,c(1,3))#choose columns by column numbers
Data=na.omit(Data_1)#remove NA parts
dir.create(file.path(A,'Data_clean_records',Filename))
summary=summary(Data[2])
png(file.path(A,'Data_clean_records',Filename,'summary.png'), height = 100*nrow(summary), width = 200*ncol(summary))
grid.table(summary)#this is the step to plot the contents
dev.off()
Tags=names(Data)
Tag1='Timestamp'
Tag2='Opening'
#interval=300 #This part is revised below.

#in case there are missing values, replace them with the median value if needed
#Demiss=median(input$VRMS.Axial..ips., na.rm=TRUE)
#hybrd.ifelse     <- function(x) { mutate_all(x, funs(ifelse(is.na(.), Demiss, .))) }
#sensor_data=hybrd.ifelse(input)
#summary(sensor_data) 


V1=mdy_hm(Data[,1]) #critical step convert the date format
Data[,1]=as.character(V1)
Month=mutate(Data,month1=format(as.POSIXct(V1),"%m"))
Day=mutate(Data,day1=format(as.POSIXct(V1+1,tz="CST"),"%d"))#be careful of how the day is converted with '+1'
year=mutate(Data,year1=format(as.POSIXct(V1),"%y"))
if((V1[4]-V1[3])==(V1[3]-V1[2])&(V1[3]-V1[2])==(V1[2]-V1[1])){
  interval=as.numeric(V1[2]-V1[1],unit="secs")
} else {
    interval<- readline(prompt="Enter the records interval in seconds: ")
    }

#Add date, month and year to the table
Data2 = Data %>% 
  mutate(
    Date = format(as.POSIXct(V1),"%Y-%m-%d"),
    Month=format(as.POSIXct(V1),"%m"),
    Year = format(as.POSIXct(V1),"%y")
  )

# #plot the time-series data
# Date=as.Date(Data2$Date)
# #Tag2
# data_y=Data2[,2]
# p0 <- ggplot(data=NULL,aes(x=Date,y=data_y))+geom_point(color = "darkred")
# p0+scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+stat_smooth()+labs(y = Tag2)
# ggsave(file.path(A,'Data_clean_records',Filename,paste0(Tag2,".png",collapse=NULL)),width = 7, height = 5, units = "in")
# ggplot(data=NULL,aes(y=data_y))+geom_boxplot()+coord_flip()+labs(y = Tag2)
# ggsave(file.path(A,'Data_clean_records',Filename,paste0(Tag2,"_box.png",collapse=NULL)),width = 7, height = 5, units = "in")



#Check missing records
colnames(Data2)[1]=Tag1
colnames(Data2)[2]=Tag2


Data3 = group_by(Data2, Date)
Data_records=summarise(Data3,count=n())
V2=Data_records[,1]#need to convert the tibble to data frame at first
V3=as.data.frame(V2)
Num=dim(V3)[1]
MD=vector()
for (i in 2:Num){if((as.POSIXct(V3[i,1])-as.POSIXct(V3[i-1,1]))!=1){MD=append(MD,i)}}
if (length(MD)==0){
    print("There is no day is missing")
  }else{for(i in MD){
    print("The record of the following days are missing:")
    print(c(i,V3[i,1]))}
}

Abnormal_records=filter(Data_records,count!=(24*3600/interval))
png(file.path(A,'Data_clean_records',Filename,"Abnormal_records.png"), height = 50*nrow(Abnormal_records), width = 150*ncol(Abnormal_records))
grid.table(Abnormal_records)
dev.off()
Abnormal_data=filter(Data2,Date%in%Abnormal_records$Date) # important use of the %in% expression
Normal_data=filter(Data2,Date%in%Abnormal_records$Date==FALSE)
Abnormal_all=group_by(Abnormal_data, Date)
Abnormal_avg=summarise(Abnormal_all,count=n(),!!paste0(Tag2,'_avg') :=mean(!! rlang::sym(Tag2)))
Abnormal_data=Abnormal_data%>%distinct(!! rlang::sym(Tag1), .keep_all = TRUE)#remove diuplicated records

#check abnormal data

SQ_time_3<- function(x,y){
  m=1
  store=Abnormal_data[FALSE,]
  Start=as.POSIXct(x,format="%Y-%m-%d %H:%M")
  End=as.POSIXct(y,format="%Y-%m-%d %H:%M")
  date1 = format(as.POSIXct(y),"%Y-%m-%d")
  Avg=subset(Abnormal_avg,Date==date1)[paste0(Tag2,'_avg')]
  while (End>Start+interval){
    store[m,1]=as.character(End-interval)
    store[m,2]= Avg
    store[m,3]=date1
    m=m+1
    End=End-interval
  }
  return(store)
}

x='2017-03-25 23:44:00'
Y='2017-03-25 23:59:00'
# 
SQ_time_4<- function(x){
Amenda=filter(Abnormal_data,Date==Abnormal_records$Date[x])
#Add the line of timestamp 23:59:00 into the table
y=as.character(Abnormal_avg[x,1])
if(Amenda[nrow(Amenda),1]!=paste(y,'23:59:00')){
  store=Abnormal_data[FALSE,]
  date1 = format(as.POSIXct(y),"%Y-%m-%d")
  Avg=subset(Abnormal_avg,Date==date1)[paste0(Tag2,'_avg')]
  store[1,1]=paste(y,'23:59:00')
  store[1,2]= Avg
  store[1,3]=date1
  Amenda=merge(Amenda,store,all=TRUE)}
#
V2=as.POSIXct(as.character(Amenda[,1]))
time_L=length(V2)
First=as.POSIXct(paste(Abnormal_records$Date[x],'00:00:00'),format="%Y-%m-%d %H:%M")
End=as.POSIXct(paste(Abnormal_records$Date[x],'23:59:00'),format="%Y-%m-%d %H:%M")
if(time_L==1){
  Af=SQ_time_3(First,V2[1])
  Ae=SQ_time_3(V2[1],End)
  Amenda_m=merge(Af,Ae,all=TRUE)}
if(time_L!=1){
  Af=SQ_time_3(First,V2[1])
  Ae=SQ_time_3(V2[time_L],End)
  m=vector()
  for (i in 2:time_L){if((V2[i]-V2[i-1])!=(interval/60)){m=append(m,i)}}
  L2=length(m)
  
  if(L2==0){ Af=SQ_time_3(First,V2[1])
  Ae=SQ_time_3(V2[time_L],End)
  Amenda_m=merge(Af,Ae,all=TRUE)
  }
  
  if(L2==1){A1=SQ_time_3(V2[m[1]-1],V2[m[1]])
  Af=merge(Af,A1,all=TRUE)
  Amenda_m=merge(Af,Ae,all=TRUE)
  }
  
  if(L2>1){for (i in 1:L2){
    A1=SQ_time_3(V2[m[i]-1],V2[m[i]])
    Af=merge(Af,A1,all=TRUE)}
    Amenda_m=merge(Af,Ae,all=TRUE)
  }
}

  Amenda_all=merge(Amenda,Amenda_m,all=TRUE)
 Amenda_all=Amenda_all[1:(24*3600/interval),]
return(Amenda_all)
}


#Getting rid of all abnormal data here
Num_abnormal=dim(Abnormal_records)[1]
Total=SQ_time_4(1)

#there was a bug here

for (i in 2:Num_abnormal){
  Total2=SQ_time_4(i)
  Total=merge(Total,Total2,all=TRUE)
}

Normal_sorted=merge(Total,Normal_data,all=TRUE)
Normal_treated=Normal_sorted%>%distinct(!! rlang::sym(Tag1), .keep_all = TRUE)#remove diuplicated records


#check the data records again for interval
Data4 = group_by(Normal_sorted[,1:4], Date)#note the number of columns here
Data_records_2=summarise(Data4,count=n())
Abnormal_records_2=filter(Data_records_2,count!=3600/interval*24)

#calculate daily average value 
Avg1=summarise(Data4,mean=mean(!! rlang::sym(Tag2)))

#Add records of missing days with the average value of the previous day relatively.

#function to add records for the missing days
SQ_time_5<- function(x){
  m=1
  store=Data4[FALSE,]
  Start=as.POSIXct(paste(V3[x-1,1],'24:00:00'),format="%Y-%m-%d %H:%M")
  End=as.POSIXct(V3[x,1])-1
  while (End-Start>0){
    store[m,1]=as.character(Start)
    store[m,2]= as.numeric(Avg1[x,2])
    store[m,3]=format(Start,"%Y-%m-%d")
    store[m,4]=format(Start,"%m")
    m=m+1
    Start=Start+interval
    # print(Start)
  }
  return(store)
}

MD_added=Data4[FALSE,]


for (i in MD){
    MD_added=rbind(MD_added,SQ_time_5(i))
}

Normal_sorted=merge(MD_added,Normal_sorted,all=TRUE)

#check the data records again for interval
Data4 = group_by(Normal_sorted[,1:4], Date)#note the number of columns here
Data_records_2=summarise(Data4,count=n())
Abnormal_records_2=filter(Data_records_2,count!=3600/interval*24)


# #Plot the daily average values
# #Tag2
# Date=as.Date(Avg1$Date)
# data_y=Avg1$mean
# p1 <- ggplot(data=NULL,aes(x=Date,y=data_y))+geom_point(color = "darkred")
# p1+scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+stat_smooth()+labs(y=paste0(Tag2,'_avg'))
# ggsave(file.path(A,'Data_clean_records',Filename,paste0(Tag2,"_avg.png",collapse=NULL)),width = 7, height = 5, units = "in")
# ggplot(data=NULL,aes(y=data_y))+geom_boxplot()+coord_flip()+labs(y=paste0(Tag2,'_avg'))
# ggsave(file.path(A,'Data_clean_records',Filename,paste0(Tag2,"_avg_box.png",collapse=NULL)),width = 7, height = 5, units = "in")
# 

#outlier Detection
#Tag2
P1=as.vector(unlist(Normal_sorted[Tag2]))# important conversion from dataframe to vector
P1_1st=quantile(P1,.25)
P1_3rd=quantile(P1,.75)
IQR=P1_3rd-P1_1st
lower_P1 = ifelse(P1_1st-1.5*IQR<0,0,P1_1st-1.5*IQR)
higher_P1 = P1_3rd+1.5*IQR
#outlier_values_Current <- boxplot.stats(Normal_sorted$Amps)$out
#hist(outlier_values_Current)# wil receive warning when there is no outlier
Outlier1=filter(Normal_sorted,Normal_sorted[Tag2]>higher_P1 | Normal_sorted[Tag2]<lower_P1)
Count1=group_by(Outlier1,Date)
Abnormal1=summarise(Count1,Outlier_P1=n())


#data_clean

Cleaner<-function(x,y,z){  
  if(x>y){
    x=y
  }else if(x<z){
    x=z
  }
  return(x)
}  

#Tag2
C1=Normal_sorted[Tag2]
Num=dim(C1)[1]
for (i in 1:Num){C1[i,1]=Cleaner(C1[i,1],higher_P1,lower_P1)}


#plot cleaned data
#Tag2
data_y=C1[,1]
Date=as.Date(Normal_sorted$Date)
pA <- ggplot(data=NULL,aes(x=Date,y=data_y))+geom_point(color = "darkred")
pA+scale_x_date(date_breaks = "2 month", date_labels =  "%b %Y")+stat_smooth()+labs(y=paste0(Tag2,'_cleaned'),collapse=NULL)
ggsave(file.path(A,'Data_clean_records',Filename,paste0(Tag2,"_cleaned_avg.png",collapse=NULL)),width = 7, height = 5, units = "in")
ggplot(data=NULL,aes(y=data_y))+geom_boxplot()+coord_flip()+labs(y=paste0(Tag2,'_cleaned_box'),collapse=NULL)
ggsave(file.path(A,'Data_clean_records',Filename,paste0(Tag2,"_cleaned_avg_box.png",collapse=NULL)),width = 7, height = 5, units = "in")


#cleaned data output

Cleaned_data=select(Normal_sorted,c(1:3))
Cleaned_data[2]=C1


for (i in 1:41375){
  if(Cleaned_data[i,1]==Cleaned_data[i+1,1]){
    print(c(i,Cleaned_data[i,1],Cleaned_data[i+1,1]))
  }
}


write.csv(Cleaned_data, file.path(A,'Data_cleaned',paste0(Filename,"_cleaned.csv")))


  