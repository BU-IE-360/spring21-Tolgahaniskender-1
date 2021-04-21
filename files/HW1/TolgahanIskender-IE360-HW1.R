# IE 360 HW1

library("readxl")
library("ggplot2")
library("usethis")
library("devtools")
library("zoo")

setwd("C:/Users/tolga/Desktop/OKUL/IE 360/HWs/HW1")


####################################################################

#Get the monthly data between 2013-01 and 2021-03
#Convert dates to standard format

HouseSales<-read_excel("EVDS-KonutSatisi.xlsx")
HouseSales$Dates<-paste0(HouseSales$Dates, rep("-15",length(HouseSales$Dates)))
HouseSales$Dates<-as.Date(HouseSales$Dates)

InterestRates<-read_excel("EVDS-TuketiciKredisiFaizleri.xlsx")
InterestRates$Dates<-paste0(InterestRates$Dates, rep("-15",length(InterestRates$Dates)))
InterestRates$Dates<-as.Date(InterestRates$Dates)

CPI<-read_excel("EVDS-cpi.xlsx")
CPI$Dates<-paste0(CPI$Dates, rep("-15",length(CPI$Dates)))
CPI$Dates<-as.Date(CPI$Dates)

KrediSearch<-read.csv("Kredi.csv")
KrediSearch$Dates<-paste0(KrediSearch$Dates,rep("-15",length(KrediSearch$Dates)))
KrediSearch$Dates<-as.Date(KrediSearch$Dates)

SatilikDaire<-read.csv("SatilikDaire.csv")
SatilikDaire$Dates<-paste0(SatilikDaire$Dates,rep("-15",length(SatilikDaire$Dates)))
SatilikDaire$Dates<-as.Date(SatilikDaire$Dates)

alldata<-data.frame(cbind(CPI$Dates,CPI$CPI,HouseSales$HouseSales,InterestRates$InterestRate,KrediSearch$SearchVolume,SatilikDaire$SearchVolume))

alldata$X1<-as.Date(alldata$X1)
colnames(alldata)<-c("Dates","CPI", "HouseSales", "InterestRate", "KrediSearch", "SatilikDaire")
?as.Date()

#####################################################################
#Basic Plots of three datatypes

#HouseSales
ggplot(data=alldata)+geom_line(mapping=aes(x=Dates,y=HouseSales),size=1.1,colour="512FDC")+
  geom_point(mapping=aes(x=Dates,y=HouseSales),colour="#512FDC",size=3)+
  labs(title = "Number of Houses Sold in Turkey", x = "Date",y = "Number of Units",subtitle = "Data from Jan-2003 to Mar-2021",caption = "Source: EVDS")+
  theme_bw()+ theme(plot.title = element_text(color = "#512FDC", size = 20, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))+
  geom_smooth(mapping=aes(x=Dates,y=HouseSales),method = lm, color = "darkblue")

#Interest Rate 
ggplot(data=alldata)+geom_line(mapping=aes(x=Dates,y=InterestRate),size=1.1,colour="512FDC")+
  geom_point(mapping=aes(x=Dates,y=InterestRate),colour="#512FDC",size=3)+
  labs(title = "Interest Rates in Turkey", x = "Date",y = "Interest Rate %",subtitle = "Data from Jan-2003 to Mar-2021",caption = "Source: EVDS")+
  theme_bw()+ theme(plot.title = element_text(color = "#512FDC", size = 20, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))+
  geom_smooth(mapping=aes(x=Dates,y=InterestRate),method = lm, color = "darkblue")

#CPI
ggplot(data=alldata)+geom_line(mapping=aes(x=Dates,y=CPI),size=1.1,colour="512FDC")+
  geom_point(mapping=aes(x=Dates,y=CPI),colour="#512FDC",size=3)+
  labs(title = "CPI in Turkey", x = "Date",y = "Consumer Price Index",subtitle = "Data from Jan-2003 to Mar-2021",caption = "Source: EVDS")+
  theme_bw()+ theme(plot.title = element_text(color = "#512FDC", size = 20, face = "bold",hjust=0.5), 
                    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
                    plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))+
  geom_smooth(mapping=aes(x=Dates,y=CPI),method = lm, color = "darkblue")

#####################################################################################
#Kredi ve Satýlýk ev Google Search Volume karþýlaþtýrmasý

ggplot(data=alldata, aes(x=Dates)) + geom_line(aes(y=SatilikDaire),size=1.5, colour="#512FDC")+
  geom_line(aes(y=KrediSearch),size=1.5)+ theme_bw()+
  labs(title = "Google Search Volumes", x = "Date",y = "Search Volume",subtitle = "Data from Jan-2003 to Mar-2021",caption = "Black: Search Volume of 'Kredi'
Purple: Search Volume of 'Satilik Ev' 
Source: Google Trends")+
  theme(plot.title = element_text(color = "#512FDC", size = 20, face = "bold",hjust=0.5), axis.title.x=element_text(colour="Dark Green", face="bold"),
        axis.title.y=element_text(colour="Dark Green", face="bold"),
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5),
        plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))

#####################################################################################
#Satilik Daire keywordu ve House Sales sayýlarý

scale<- max(alldata$HouseSales)/max(alldata$SatilikDaire)

ggplot(data=alldata, aes(x=Dates))+geom_line(aes(y=SatilikDaire*scale),size=1.5,colour="#512FDC")+
  geom_line(aes(y=HouseSales),size=1.5)+ theme_bw()+ labs(title = "Search Volume of 'Satýlýk Daire' vs Number of Houses Sold in TUrkey", x = "Date",subtitle = "Data from Jan-2003 to Mar-2021",caption = "Black: Number of Houses Sold
Purple: Search Volume of 'Satýlýk Daire' 
Source: Google Trends, EVDS") + scale_y_continuous("Number of Houses Sold", sec.axis=sec_axis(~./scale, name="Google Search for 'Satýlýk Daire'"))+
  theme(axis.title.y = element_text(color = "black", face = "bold"),axis.title.y.right = element_text(colour="#512FDC",face="bold"),plot.title = element_text(color = "dark green", size = 15, face = "bold",hjust=0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5), axis.title.x=element_text(colour="Dark Green", face="bold"),
        plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))
  

####################################################################################
#Number of Houses sold vs Interest Rate

scale2<-max(alldata$HouseSales)/max(alldata$InterestRate)

ggplot(data=alldata, aes(x=Dates))+geom_line(aes(y=HouseSales),size=1.5,colour="#512FDC")+
  geom_line(aes(y=InterestRate*scale2),size=1.5)+ theme_bw()+ labs(title = "Interest Rates and Number of Houses Sold", x = "Date",subtitle = "Data from Jan-2003 to Mar-2021",caption = "Black: Interest Rate
Purple: Number of Houses Sold 
Source: EVDS")+ scale_y_continuous("Number of Houses Sold", sec.axis=sec_axis(~./scale2, name="Interest Rates (%)"))+
  theme(axis.title.y = element_text(color = "#512FDC", face = "bold"),axis.title.y.right = element_text(colour="black",face="bold"),plot.title = element_text(color = "dark green", size = 15, face = "bold",hjust=0.5), 
        plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5), axis.title.x=element_text(colour="Dark Green", face="bold"),
        plot.caption = element_text(face = "italic", hjust = 0))+scale_x_date(date_breaks = "1 year",labels=date_format("%Y"))

####################################################################################
#Interest Rate vs CPI Scatter plot

ggplot(data=alldata, aes(x=CPI,y=InterestRate))+geom_point(colour="#512FDC")+theme_bw()+
  labs(title = "Interest Rate vs CPI (% change)",y="Interest Rate (%)", x = "CPI (% change)",subtitle = "Data from Jan-2003 to Mar-2021",caption = "Source: EVDS")+
  geom_smooth(method = lm, color = "dark red")+ theme(axis.title.y = element_text(color = "#512FDC", face = "bold"),plot.title = element_text(color = "dark green", size = 15, face = "bold",hjust=0.5), 
       plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.5), axis.title.x=element_text(colour="#512FDC", face="bold"),
       plot.caption = element_text(face = "italic", hjust = 0))

####################################################################################