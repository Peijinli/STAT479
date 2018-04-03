
# 1. The top 10 companies in the US ---------------------------------------

library(rvest)
library(tidyverse)
library(stringr)
library(quantmod) #financial package
library(twitteR) #twitter from R
library(base64enc) #supplementary package to the above
library(lubridate)

url="https://www.motherjones.com/politics/2016/06/fully-loaded-ten-biggest-gun-manufacturers-america/"
company=read_html(url) %>% 
  html_nodes("p strong") %>% 
  html_text() %>% .[-c(11:18)] %>% 
  str_replace(.,"\\d+\\.[ ]","")
company

Abb=c("RGR",NA,"SWHC",NA,NA,NA,NA,NA,NA,NA)

# only two of the 10 companies has stock symbol

#from https://twocents.lifehacker.com/how-to-divest-from-gun-stocks-1823069001 
#four major comapnies have stock symbol. AOBC is the same as SWHC as per yahoo finance??
#none of them have s&p 500, they go public on NASDAQ or NYSE

data=tibble(company=c("American Outdoor Brands","Sturm, Ruger","Vista Outdoor","Olin"),
            symbol=c("AOBC","RGR","VSTO","OLN"),
            agency=c("NASDAQ","NYSE","NYSE","NYSE"))


# 2. Get the stock data ---------------------------------------------------

getSymbols("AOBC") #"AOBC" is the symbol for American Outdoor Brands
tail(AOBC) # the latest few reords
AOBCplot=chartSeries(AOBC,subset="2017::2018",theme="white") #plot the data

getSymbols(data$symbol,from="2017-01-01",adjust=TRUE) #all the four companies  
stock=Cl(get(data$symbol[1])) #close column of AOBC
for(i in 2:length(data$symbol))
  stock=merge(stock,Cl(get(data$symbol[i]))) #closure stock price for all four companies     
colnames(stock)=data$symbol
tail(stock)


# 3. Mass shooting with stock price ---------------------------------------


mass=read_csv("Mother Jones' Investigation_ US Mass Shootings, 1982-2018 - US mass shootings.csv") 
# from https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/
str(mass)
head(mass)
date=mdy(mass$Date) #covert the date to be the same format as in stock[]

mdays=function(a,x){
  day=matrix(nrow = x,ncol=length(a))
  for (i in 1:x){
    day[i,]=a+i}
  c(a,day)}
#create a function to extend the mass shooting date to the following x days

daterange=mdays(ymd("2018-02-14"),7) #Florida mass shoot date and the following 7 days
stock[daterange] %>% as.tibble() %>% 
  mutate(date=index(stock[daterange])) %>% 
  gather('AOBC','RGR','VSTO','OLN',key="Co",value="price") %>% 
  ggplot(aes(y=price,x=date))+
  geom_line()+
  facet_wrap(~Co, nrow = 2)

plot(stock[daterange]) 

#how the stock change in these 8 days


# 4. Calculate the stock change and tweet ---------------------------------

#try to tweet a sample from R
consumer_key="your key"
consumer_secret="your key"
access_token="your key"
access_secret="your key"
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#you need to set up a twitter App to get above keys, and link this code with your twitter account.
#in the field of required webiste(cannot remember the accurate name), just input http://example.com

tweet("Hello World")

#succeed!!!

post=function(df){
  pricechange=vector("double",ncol(df))
  for (i in ncol(df)){
    pricechange[i]=(coredata(df[nrow(df),i])-coredata(df[nrow(df)-1,i]))/coredata(df[nrow(df)-1,i])
    text=str_c("The stock price of",colnames(df)[i],"on",index(df)[nrow(df)],"decreased",abs(pricechange[i]),sep=" ")
    if (pricechange[i]<-0.02){
      tweet(text)
    }
  }
}
#if the stock price drops 2%, tweet.
#the data type in quantmod is xts, we need to use coredata() to convert to regular numeric data.

post(stock)
