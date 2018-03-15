library(xml2)
library(rvest)
library(XML)
library(choroplethr)
library(choroplethrMaps)
library(data.table)
library(tidyverse)
library(blscrapeR)
library(dplyr)
library(RCurl)
setwd("C:\\Users\\Peiji\\Desktop\\spring2018\\STAT479\\class\\hw3")

### get county list = (INCITS,County or equivalent, State or district, uniqname, url,	urlnodes) ########

webpage <- "https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents"
county <- webpage %>%
  html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>%
  html_table()
county  <- county[[1]] %>% select(INCITS,`County or equivalent`,`State or district`)
head(county)
str(county)

#### https://en.wikipedia.org/wiki/Weston_County,_Wyoming    ####
#### https://en.wikipedia.org/wiki/Baldwin_County,_Alabama   ####
#### above are the url address, thus we can form our own url ####

urladdress <- c()
unique_county_name<-c()
#us1 <- str_replace_all(county[72,]$`County or equivalent`,"(\\[[:digit:]\\])","")
for(i in 1:dim(county)[1]){
  usename <- county$`County or equivalent`[i]
  us1 <- str_replace_all(usename,"\\[(.*)\\]","")
  a = paste(strsplit(us1, split = ' ')[[1]],collapse = "_")
  b = paste(strsplit(county$`State or district`[i], split = ' ')[[1]],collapse = "_")
  uniq_name <- paste0(a,",_",b)
  address<-paste0("https://en.wikipedia.org/wiki/",a,",_",b)
  urladdress<-c(urladdress,address)
  unique_county_name<-c(unique_county_name,uniq_name)
}
county$url <- urladdress
county$uniqname<-unique_county_name
# county$`County or equivalent`[5]
# county$`County or equivalent`[2433]
#county[c(72,309,315,320),]
urladdress[320]<-"https://en.wikipedia.org/wiki/Washington,_D.C."
urladdress[70]<-"https://en.wikipedia.org/wiki/Anchorage,_Alaska"


#now we are gonna using regular expression to collect address from each county webpage
nodes<-c()
for(i in 1:length(county$url)){
  print(i)
  count1 <- urladdress[i]
  page   <- getURL(count1)
  parsed <- htmlParse(page)
  #str(parsed)
  links  <- xpathSApply(parsed, path="//a", xmlGetAttr, "href")
  linkdf <- links %>% unlist() 
  #str(linkdf)
  a<-str_match_all(linkdf,pattern = "/wiki/.+_County,_.*") %>% unlist()
  res1<-unique(gsub("(/wiki/)(.*)(_County,_)(.*)", "\\2\\3\\4", a, perl=TRUE))
  #i = 68
  #i = 309 : res1[15] == county$uniqname[312]
  connect <- c()
  if(length(res1)!=0){
    for(j in 1:length(res1)){
      if( res1[j] %in% county$uniqname){
        connect<-c(connect,res1[j])
      }
    }
  }
  result<-paste(connect, collapse=' ')
  nodes<-c(nodes,result)
}
county$urlnodes<-nodes
str(county)

#get county name and return as a list
write.csv(county,"county_connect_data.csv")