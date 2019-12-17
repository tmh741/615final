library(tidyverse)
library(magrittr)
library(readxl)

atk <- read.csv("https://ndownloader.figshare.com/files/14480672")

atk %>% group_by(country) %>% summarize(n=n())
usa <- atk[atk$country=="USA",]
usa2010 <- usa[usa$year>=2010,]

usa2010 %>% group_by(state) %>% summarize(n=n())
View(usa2010 %>% group_by(state,year) %>% summarize(n=n()))

why <- read_xls("GSAF5-2.xls")
huh <- why[1:6483,1:16]
huh <- huh[-which(is.na(huh$`Case Number...1`)),]
huh$Year <- as.numeric(huh$Year)
huh <- huh[-which(is.na(huh$Year)),]
huh$Hurt <- ifelse(grepl("No injury",huh$Injury)==T,0,1)
huh$GSAF <- ifelse(grepl("GSAF",huh$`Investigator or Source`)==T,1,0)

huhyr <- subset(huh,Year>=2010)

huhusa <- subset(huhyr,Country=="USA")


huhusa %>% group_by(Type) %>% summarize(n=n())
huhusa %>% subset(Hurt==1) %>% group_by(Year) %>% summarize(n=n())
huhusa %>%  group_by(Area) %>% summarize(n=n())
huhusa %>% subset(Area=="Florida") %>% group_by(Area,Year) %>% summarize(n=n())
huhusa %>% group_by(Hurt) %>% summarize(n=n())
huhusa %>% group_by(GSAF) %>% summarize(n=n())

huhusa %>% subset(Year==2018) %>% group_by(Type) %>% summarize(n=n())


mmm <- huhusa %>% subset(Year==2018) %>% subset(Type=="Unprovoked" | Type=="Provoked")
length(unique(mmm$Date))

mm <- mmm %>% group_by(Date, Location) %>% summarize(n=n())
nrow(mm)


View(subset(huhusa,GSAF=1))

ifelse(grepl("No injury",huhusa$Injury)==T,0,1)

test <- huhusa %>% separate(`Investigator or Source`, c("Test1","Test2"),sep=", ")
test$Test2 <- ifelse(grepl("/",test$Test2)==T,NA,test$Test2)

tab1  <- test %>% group_by(Test1) %>% summarize(count1=n())
tab2 <- test %>% group_by(Test2) %>% summarize(count2=n())

test2 <- full_join(tab1,tab2,by=c("Test1"="Test2"))
test2[is.na(test2)] <- 0
test3 <- test2 %>% transmute(source=Test1,count = count1 + count2)

library(newsanchor)


