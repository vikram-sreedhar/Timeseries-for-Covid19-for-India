library(e1071)
library(readr)
library(caret)
library(caTools)
library(Boruta)
library(mlbench)
library(psych)
library(devtools)
library(graphics)
library(forecast)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(graphics)
library(tseries)
library(lubridate)
library(readxl)
library(googleVis)
library(reprex)
library(rpivotTable)
library(DescTools)

#####################################################################################

# Reading the data file and loading the directory

setwd("E:/Vikram & Associates/Fun with Data/Corona Pred")


data_corona<-read_xlsx("Final_corona_data.xlsx")

####  ExPLORATORY DATA ANALYSIS ON CORNAVIRUS DATA ##################################

str(data_corona)


data_corona$Date <-as.Date(data_corona$Date)
data_corona$Province<-as.factor(data_corona$Province)
data_corona$Country<-as.factor(data_corona$Country)
data_corona$`Country Code`<-as.factor(data_corona$`Country Code`)
data_corona$weekday<-as.factor(data_corona$weekday)
data_corona$Month<-as.factor(data_corona$Month)

data_corona<-plyr::rename(data_corona,replace = c("Country Code" = "country_code"))
 
str(data_corona)

#Classes 'tbl_df', 'tbl' and 'data.frame':	6189 obs. of  13 variables:
#$ Province           : Factor w/ 451 levels "Afghanistan",..: 8 8 8 8 8 8 8 8 8 8 ...
#$ Country            : Factor w/ 191 levels "Afghanistan",..: 4 4 4 4 4 4 4 4 4 4 ...
#$ country_code       : Factor w/ 180 levels "AD","AE","AF",..: 1 1 1 1 1 1 1 1 1 1 ...
#$ Date               : Date, format: "2020-03-02" "2020-03-03" "2020-03-04" "2020-03-05" ...
#$ Day1               : num  41 42 43 44 45 46 47 48 49 50 ...
#$ weekday            : Factor w/ 7 levels "FRIDAY","MONDAY",..: 2 6 7 5 1 3 4 2 6 7 ...
#$ Month              : Factor w/ 3 levels "FEBRUARY","JANUARY",..: 3 3 3 3 3 3 3 3 3 3 ...
#$ YTD_Confirmed_cases: num  1 1 1 1 1 1 1 1 1 1 ...
#$ YTD_death_cases    : num  0 0 0 0 0 0 0 0 0 0 ...
#$ YTD_recovered_Cases: num  0 0 0 0 0 0 0 0 0 0 ...
#$ Confirmed_cases    : num  1 0 0 0 0 0 0 0 0 0 ...
#$ Death_cases        : num  0 0 0 0 0 0 0 0 0 0 ...
#$ Recovered_cases    : num  0 0 0 0 0 0 0 0 0 0 ...


summary(data_corona)


#Province         Country      country_code       Date                 Day1            weekday          Month     
#Japan   :  62   China    :1955   CN     :1955   Min.   :2020-01-22   Min.   : 1.00   FRIDAY   :1170   FEBRUARY:2362  
#Macau   :  62   US       :1621   US     :1621   1st Qu.:2020-02-20   1st Qu.:30.00   MONDAY   :1033   JANUARY : 496  
#Qinghai :  62   Australia: 328   AU     : 328   Median :2020-03-07   Median :46.00   SATURDAY :1234   MARCH   :5074  
#Thailand:  62   Canada   : 255   CA     : 255   Mean   :2020-03-02   Mean   :41.33   SUNDAY   :1285                  
#Anhui   :  61   France   : 132   FR     : 139   3rd Qu.:2020-03-16   3rd Qu.:55.00   THURSDAY :1109                  
#Beijing :  61   Japan    :  62   GB     : 104   Max.   :2020-03-22   Max.   :61.00   TUESDAY  : 991                  
#(Other) :7561   (Other)  :3579   (Other):3530                                        WEDNESDAY:1110                  
#YTD_Confirmed_cases YTD_death_cases   YTD_recovered_Cases Confirmed_cases     Death_cases      Recovered_cases  
#Min.   :    0.0     Min.   :   0.00   Min.   :    0.0     Min.   :  -62.00   Min.   : -3.000   Min.   : -30.00  
#1st Qu.:    2.0     1st Qu.:   0.00   1st Qu.:    0.0     1st Qu.:    0.00   1st Qu.:  0.000   1st Qu.:   0.00  
#Median :   16.0     Median :   0.00   Median :    0.0     Median :    1.00   Median :  0.000   Median :   0.00  
#Mean   :  655.4     Mean   :  22.98   Mean   :  237.2     Mean   :   43.47   Mean   :  1.856   Mean   :  12.38  
#3rd Qu.:  131.0     3rd Qu.:   1.00   3rd Qu.:   10.0     3rd Qu.:    7.00   3rd Qu.:  0.000   3rd Qu.:   0.00  
#Max.   :67800.0     Max.   :5476.00   Max.   :59433.0     Max.   :14840.00   Max.   :793.000   Max.   :3418.00  
psych::pairs.panels(data_corona[5:13])

# Recovery on date and death on date is moderately positive correlated with correlation coefficient of 0.43

df1<-aggregate(list(data_corona$Confirmed_cases,data_corona$Death_cases,data_corona$Recovered_cases),by=list(data_corona$Date,data_corona$country_code,data_corona$Country),FUN=sum)
names(df1)<-list("Date","country_code","Country","Confirm_cases","Mortality_cases","Recovered_cases")


df2<-aggregate(list(data_corona$Confirmed_cases,data_corona$Death_cases,data_corona$Recovered_cases),by=list(data_corona$country_code,data_corona$Country),FUN=sum)
names(df2)<-list("country_code","Country","Confirm_cases","Mortality_cases","Recovered_cases")

str(df2)

chart_1<-gvisMotionChart(df1,idvar = "Country", timevar = "Date")
plot(chart_1)
print(chart_1,"chart",file="chart_1.html")
dput(chart_1,"chart_1.txt")


gdgt <- createGoogleGadget(chart_1)
cat(gdgt)
write(gdgt,"gdgt_co.html")

ch2<-gvisColumnChart(df2,xvar="country_code")
plot(ch2)

ints1<-gvisGeoChart(df2,locationvar = "Country","Confirm_cases","Mortality_cases","Recovered_cases",options = list(width=500,height=500))
tbl1<-gvisTable(df2,options=list(width=750,eight=500))

Map_tbl1<-gvisMerge(ints1,tbl1,horizontal = TRUE)

plot(Map_tbl1)
plot(ints1)
plot(tbl1)

print(Map_tbl1,file = "map_tbl1.html")

system("wkhtmltoimage --enable-plugins --javascript-delay 10000   map_tbl1.html")

df3<-aggregate(list(data_corona$Confirmed_cases,data_corona$Death_cases,data_corona$Recovered_cases),by=list(data_corona$Country,data_corona$weekday),FUN=sum)
names(df3)<-list("country","week_day","Confirm_cases","Mortality_cases","Recovered_cases")

df4<-aggregate(list(data_corona$Confirmed_cases,data_corona$Death_cases,data_corona$Recovered_cases),by=list(data_corona$Date),FUN=sum)
names(df4)<-list("Date","Confirm_cases","Mortality_cases","Recovered_cases")

rpivotTable(data_corona,
            rows = c("Country","Province"),cols = "Date", vals = "counts", aggregatorName = "Sum", rendererName = "Heatmap", width="100%", height="400px")

write.csv(df1,"df1.csv")
write.csv(df2,"df2.csv")
write.csv(df3,"df3.csv")
write.csv(df4,"df4.csv")

WorldData <- map_data('world') %>% filter(region != "Antarctica") %>% fortify

p <- ggplot() +
  geom_map(data = WorldData, map = WorldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = df2, map=WorldData,
           aes(fill=Mortality_cases, map_id=Country),
           colour="#7f7f7f", size=0.5) +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="thistle2", high="darkred", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="legend", title="Title", x="", y="") +
  geom_text(data=df2$Mortality_cases)+
  theme_bw()
p


r<-ggplot(df4,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
    geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)
  

r1<-ggplot(df4,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)


r2<-ggplot(df4,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)

grid.arrange(r,r1,r2)

df_IN<-subset(df1,df1$country_code=="IN")

IN1<-ggplot(df_IN,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

IN2<-ggplot(df_IN,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

IN3<-ggplot(df_IN,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(IN1,IN2,IN3)



df_CN<-subset(df1,df1$country_code=="CN")

CN1<-ggplot(df_CN,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

CN2<-ggplot(df_CN,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

CN3<-ggplot(df_CN,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(CN1,CN2,CN3)



df_IT<-subset(df1,df1$country_code=="IT")

IT1<-ggplot(df_IT,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

IT2<-ggplot(df_IT,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

IT3<-ggplot(df_IT,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(IT1,IT2,IT3)




df_FR<-subset(df1,df1$country_code=="FR")

FR1<-ggplot(df_FR,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

FR2<-ggplot(df_FR,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

FR3<-ggplot(df_FR,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(FR1,FR2,FR3)


df_DE<-subset(df1,df1$country_code=="DE")

DE1<-ggplot(df_DE,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

DE2<-ggplot(df_DE,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

DE3<-ggplot(df_DE,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(DE1,DE2,DE3)


df_ES<-subset(df1,df1$country_code=="ES")

ES1<-ggplot(df_ES,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

ES2<-ggplot(df_ES,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

ES3<-ggplot(df_ES,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(ES1,ES2,ES3)

df_US<-subset(df1,df1$country_code=="US")

US1<-ggplot(df_US,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

US2<-ggplot(df_US,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

US3<-ggplot(df_US,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(US1,US2,US3)

df_UK<-subset(df1,df1$country_code=="GB")

UK1<-ggplot(df_UK,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

UK2<-ggplot(df_UK,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

UK3<-ggplot(df_UK,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(UK1,UK2,UK3)


grid.arrange(US1,US2,US3)

df_IR<-subset(df1,df1$country_code=="IR")

IR1<-ggplot(df_IR,aes(x=Date, y = Confirm_cases))+
  geom_line(aes(y = Confirm_cases), color = "black")+
  geom_text(aes(label = Confirm_cases),size = 2,vjust = -3)

IR2<-ggplot(df_IR,aes(x=Date, y = Mortality_cases))+
  geom_line(aes(y = Mortality_cases), color = "darkred")+
  geom_text(aes(label = Mortality_cases),size = 2,vjust = -3)

IR3<-ggplot(df_IR,aes(x=Date, y = Recovered_cases))+
  geom_line(aes(y = Recovered_cases), color = "blue")+
  geom_text(aes(label = Recovered_cases),size = 2,vjust = -3)


grid.arrange(IR1,IR2,IR3)


DescTools::Desc(df1)


week_smmry<-aggregate(list(data_corona$Confirmed_cases,data_corona$Death_cases,data_corona$Recovered_cases),by=list(data_corona$weekday),FUN=sum)
names(week_smmry)<-list("weekday","Confirm_cases","Mortality_cases","Recovered_cases")

week_smmry


####  TIME SERIES AND PREDICTION OF CONFIRMATION, DEATH AND RECOVERY - INDIA  ##################################


df_IN_cs<-subset(df1,df1$country_code=="IN")

str(df_IN_cs)

df_IN_cs$country_code<-NULL
df_IN_cs$Country<-NULL
df_IN_cs$Mortality_cases<-NULL
df_IN_cs$Recovered_cases<-NULL
df_IN_cs$day1<-c(1:53)

timser_IN_cs<-ts(df_IN_cs$Confirm_cases)
plot_1<-plot(timser_IN_cs)




#Smoothing the series - Moving Average Smoothing - India

w <-1
smoothedseries_1 <- stats::filter(timser_IN_cs, 
                                  filter=rep(1/(2*w+1),(2*w+1)), 
                                  method='convolution', sides=2)

#Smoothing left end of the time series

diff <- smoothedseries_1[w+2] - smoothedseries_1[w+1]
for (i in seq(w,1,-1)) {
  smoothedseries_1[i] <- smoothedseries_1[i+1] - diff
}

#Smoothing right end of the time series

n <- length(timser_IN_cs)
diff <- smoothedseries_1[n-w] - smoothedseries_1[n-w-1]
for (i in seq(n-w+1, n)) {
  smoothedseries_1[i] <- smoothedseries_1[i-1] + diff
}

#Plot the smoothed time series

timevals_in <- df_IN_cs$day1
lines(smoothedseries_1, col="blue", lwd=2)

#Building a model on the smoothed time series using classical decomposition
#First, let's convert the time series to a dataframe

smootheddf1 <- as.data.frame(cbind(timevals_in, as.vector(smoothedseries_1)))
colnames(smootheddf1) <- c('day1', 'Confirmed_cases')


#Now, let's fit a multiplicative model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Confirmed_cases ~ sin(0.3885*day1) * poly(day1,3) + cos(0.3795*day1) * poly(day1,3)
            + day1, data=smootheddf1)

summary(lmfit)
global_pred <- predict(lmfit, day1=timevals_in)
summary(global_pred)
lines(timevals_in, global_pred, col='red', lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- timser_IN_cs-global_pred
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#Series: local_pred 
#ARIMA(0,0,1) with zero mean 

#Coefficients:
#          ma1
#      -0.9889
#s.e.   0.1863

#sigma^2 estimated as 18.17:  log likelihood=-153.28
#AIC=310.55   AICc=310.79   BIC=314.5

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")




#Augmented Dickey-Fuller Test

#data:  resi
#Dickey-Fuller = -5.0717, Lag order = 3, p-value = 0.01
#alternative hypothesis: stationary

kpss.test(resi)


#         KPSS Test for Level Stationarity

#data:  resi
#KPSS Level = 0.14004, Truncation lag parameter = 3, p-value = 0.1

# since we have p value < 0.05 in ADF test and p value > 0.05 residual series of Confirmed cases is stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 days

outdata_India <- df_IN_cs[42:53,]
timevals_out <- outdata_India$day1

global_pred_out <- predict(lmfit,data.frame(day1 =timevals_out))

fcast <- global_pred_out
fcast



# forecast values

#       1         2         3         4         5         6         7         8         9        10        11 
#9.025716  9.368717  9.656008 10.226683 11.595630 14.396041 19.273766 26.746346 37.050512 50.009584 64.954374 
#       12 
#80.726467 

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata_India[,2])[5]
MAPE_class_dec

# [1] 35.27596

accuracy(fcast,outdata_India[,2])

#                 ME     RMSE      MAE       MPE     MAPE
#Test set -0.2524871 9.199888 6.440145 -17.46467 35.27596


# Now predicting Sales for months 70 to 81 using classical decomposition method.

Pred_India_Confm_cases <- predict(lmfit, data.frame(day1=c(54:83)))
Pred_India_Confm_cases


#        1         2         3         4         5         6         7         8         9        10        11 
# 95.78114 108.38995 116.92300 120.17126 117.65487 109.85665  98.32420  85.59965  74.96121  69.99309  74.03322 
#       12        13        14        15        16        17        18        19        20        21        22 
# 89.57765 117.73870 157.85797 207.36150 261.91399 315.88476 363.08680 397.69956 415.24489 413.46202 392.92714 
#       23        24        25        26        27        28        29        30 
#357.28867 313.03876 268.80898 234.25433 218.66238 229.48166 270.99459 343.35767 

write.csv(df_IN_cs,"df_IN_cs.csv")

#****************************************************************************************************************************************************

