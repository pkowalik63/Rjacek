#packets loading

library(DBI)      #database
library(odbc)     #ODBC
library(ggplot2)  #rysowanie
library(tseries)  #przygotowanie danych
library(forecast) #prognoza

#DB Connection
con <- DBI::dbConnect(odbc::odbc(), driver = "PostgreSQL Unicode(x64)", 
                  database = "jacek",
                  UID = "postgres", 
                     PWD = "kufel",
                  host = "localhost", 
                     port = 5432)

#wykonanie zapytania
result <- dbSendQuery(con,"select s.\"StartDate\",s.\"Qty\" from salescor as s  where  s.\"DmdUnit\" =1933  and s.\"Location\"=2047 order by s.\"StartDate\"")
fetchRe <- dbFetch(result)
#pobranie danych z zapytania do wektora
wektor<-fetchRe

library(xts)

dates=as.Date(wektor$StartDate,"%Y-%m-%d")
xs=ts(wektor[,c('Qty')])

#cleaned data
wektor$clean_cnt=tsclean(xs)
# ma
wektor$cnt_ma=ma(wektor$clean_cnt,order=4) #średnia za 4 tyg
#plot(xs)
print(wektor)
ggplot()+
  geom_line(data=wektor,aes(x=dates,y=clean_cnt,colour="Weekly Counts"))+
  geom_line(data=wektor,aes(x=dates,y=cnt_ma,colour="Monthly Moving Average"))

#seasonal component
#count_ma=ts(na.omit(wektor$cnt_ma),frequency=52)
count_ma=ts(na.omit(wektor$clean_cnt),frequency=52)
decomp=stl(count_ma,s.window = "periodic")

deseasonal_cnt<-seasadj(decomp)
plot(decomp)
adf.test(count_ma,alternative="stationary")
acf(count_ma,main="")
pacf(count_ma,main="")
count_d1=diff(deseasonal_cnt,differences = 1)
plot(count_d1)
adf.test(count_d1,alternative="stationary")
#d <- ts(wektor$clean_cnt,start = c(2015,1/52),frequency = 52)
d <- ts(count_ma,start = c(2015,1/52),frequency = 52)
print(d)
plot(d)
#model ets
etsfit <- ets(d)
#plot(forecast(etsfit))
plot(stlf(d,method = "arima"))
plot(stlf(d,method = "ets"))
accuracy(stlf(d,method = "ets"))
forecast.ets(etsfit) 

# ARIMA model
arimafit <- auto.arima(d)
fcast <- forecast(arimafit)
plot(fcast)

accuracy(fcast)
forecast.ets(fcast)

library(tibble)
#as.tibble(d)

#as.tibble(wektor$Qty)

dbDisconnect(con)

