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

### get each county webpage link using select gadget to get--"td:nth-child(2) a" ###
webpage <- read_html("https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents")
allurl <- webpage %>% html_nodes("td:nth-child(2) a") %>% html_attr("href")
suburl <- str_match_all(allurl,pattern = "/wiki/.*") %>% unlist()
urladdress <- suburl[5:3146]

##### !!!mind!!!                                    #####
##### will occur time out error                     #####
##### change the starting number to the error i     #####
##### collect all the useful link from each webpage #####

nodes<-c()
for(i in 1:length(urladdress)){
  print(i)
  count1 <- paste0("https://en.wikipedia.org",urladdress[i])
  page   <- getURL(count1)
  parsed <- htmlParse(page)
  #str(parsed)
  linkdf  <- xpathSApply(parsed, path="//a", xmlGetAttr, "href") %>% unlist()
  #str(linkdf)
  res1<-str_match_all(linkdf,pattern = "/wiki/.*") %>% unlist()
  #filter useful link
  connect <- c()
  if(length(res1)!=0){
    for(j in 1:length(res1)){
      if( res1[j] %in% urladdress){
        connect<-c(connect,res1[j])
      }
    }
  }
  result<-paste(connect, collapse=' ')
  nodes<-c(nodes,result)
}
#str(nodes)
#save data as US_county.csv
US_county <- data.frame(urladdress,nodes)
write.csv(US_county,"US_county.csv")