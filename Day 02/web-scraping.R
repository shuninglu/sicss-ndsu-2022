#######################################
############SICSS-NDSU 2022############
#######################################

##Day 2 Collecting Digital Trace Data##
##        Part 1: Web Scraping       ## 
##        Author: Shuning Lu         ##

#install package::rvest, tidyverse, ggplot2
#load package
library(rvest)
library(tidyverse)
library(ggplot2)


##Example 1: Scraping Tables Using CSS Selector##
#extract table
url_data <- "https://en.wikipedia.org/wiki/COVID-19_pandemic_by_country_and_territory"
url_data %>% 
  read_html()  #read HTML into R
css_selector <- "#covid-19-cases-deaths-and-rates-by-location" #using inspection tool
data<-url_data %>% 
  read_html() %>% 
  html_element(css = css_selector) %>% 
  html_table()

#basic wrangling
head(data)  #understand data
newdata <- data[2:218, 2:5] #subset data
newdata <- rename(newdata, Deaths_per_m = 'Deaths / million') #rename variables
newdata <- newdata %>%   #convert data type
  mutate(Deaths = as.numeric(gsub(",","",Deaths)))%>% 
  mutate(Cases = as.numeric(gsub(",","",Cases)))%>% 
  mutate(Deaths_per_m = as.numeric(gsub(",","",Deaths_per_m)))
newdata <- mutate(newdata,
       Cases_per_m = Cases/(Deaths/Deaths_per_m)) #calculation
newdata$Country[2]

##Example 2: Scraping Tables Using HTML node##
#extract table
gdp <- read_html("https://en.wikipedia.org/wiki/List_of_countries_by_GDP_(PPP)_per_capita")
gdp <- html_node(gdp, ".wikitable") #extract table
gdp <- html_table(gdp, fill = TRUE) #convert table into dataframe

#basic wrangling
head(gdp)
dim(gdp)
newgdp <- gdp[2:231, ]#subset data
newgdp <- newgdp[, !duplicated(colnames(newgdp))] #remove duplicated columns
newgdp <- rename(newgdp, Country = 'Country/Territory') #rename variable
newgdp <- rename(newgdp, GDP_WB = 'World Bank[7]') #rename variable

newgdp <- newgdp %>% 
  mutate(GDP_WB = as.numeric(gsub(",","",GDP_WB)))%>% #covert data type
  mutate(Country = gsub("\\*|\\(|\\)","",Country))%>% #remove * in country names
  mutate(Country = gsub("[^0-9A-Za-z///' ]","",Country)) #remove ugly stff
newgdp$Country[1]

#merge datasets from Example 1 &2
merge <- merge(newdata, newgdp, by="Country") 
merge$UNRegion <- as.factor(merge$'UN Region')

#simple plotting
ggplot(merge,aes(x = GDP_WB,y = Deaths_per_m))+
  geom_point(aes(col = UNRegion))

#filtering data by range
myplot <- ggplot(merge,aes(x = GDP_WB,y = Deaths_per_m))+
  geom_point(aes(col = UNRegion))
myplot %+% subset(merge, GDP_WB %in% c(0: 50000)) 

#filtering data by category
myplot<- ggplot(merge,aes(x = GDP_WB,y = Deaths_per_m))+
  geom_point(aes(col = UNRegion))
myplot %+% subset(merge, UNRegion %in% c("Americas"))
