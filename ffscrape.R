

setwd("C:/Users/rcarder/Documents/dev/ffcheatsheet")

#install.packages("rvest")
install.packages("jsonlite")
#install.packages("readr")

library(rvest)
library(BAMMtools)
library(RODBC)           # Provides database connectivity
library(dplyr)           # only used for nice format of Head() function here
library(gridExtra)
library(forcats) 
library(grid)
library(RGraphics)
library(ggforce) # for 'geom_arc_bar'
library(reshape)
library(stringr)
library(tidyr)
library(timeDate)
library(lubridate)
library(RJSONIO)
library(maps)
library(mapdata)
library(jsonlite)
library(geosphere)
library(ggmap)
library(ggplot2)
library(tools)
library(mapplots)
library(viridis)
library(ggrepel)
library(formattable)
library(extrafont)
library(alluvial)
library(readr)


##Scrape from FantasyPros

qburl <- "https://www.fantasypros.com/nfl/projections/qb.php?week=draft"
rburl <- "https://www.fantasypros.com/nfl/projections/rb.php?week=draft"
wrurl <- "https://www.fantasypros.com/nfl/projections/wr.php?week=draft"
teurl <- "https://www.fantasypros.com/nfl/projections/te.php?week=draft"

xpathpath<-'//*[@id="data"]'



QB <- qburl %>%
read_html() %>%
html_nodes(xpath=xpathpath) %>%
html_table()
QB <- QB[[1]]
names(QB) <- c("Name","passAtt_fp","passcomp_fp","pass_yds","pass_tds","pass_int","rushAtt_fp","rush_yds","rush_tds","FL","pts")
QB<-QB[-c(1,2), ] 
QB<-head(QB,n=35)
QB$POS<-'QB'
QB$POSRANK<-seq.int(nrow(QB))


RB <- rburl %>%
  read_html() %>%
  html_nodes(xpath=xpathpath) %>%
  html_table()
RB <- RB[[1]]
names(RB) <- c("Name","rushAtt_fp","rush_yds","rush_tds","rec","rec_yds","rec_tds","FL","pts")
RB<-RB[-c(1,2), ] 
RB<-head(RB,n=60)
RB$POS<-'RB'
RB$POSRANK<-seq.int(nrow(RB))

WR <- wrurl %>%
  read_html() %>%
  html_nodes(xpath=xpathpath) %>%
  html_table()
WR <- WR[[1]]
names(WR) <- c("Name","rec","rec_yds","rec_tds","rushAtt_fp","rush_yds","rush_tds","FL","pts")
WR<-WR[-c(1,2), ] 
WR<-head(WR,n=70)
WR$POS<-'WR'
WR$POSRANK<-seq.int(nrow(WR))

TE <- teurl %>%
  read_html() %>%
  html_nodes(xpath=xpathpath) %>%
  html_table()
TE <- TE[[1]]
names(TE) <- c("Name","rec","rec_yds","rec_tds","FL","pts")
TE<-TE[-c(1,2), ] 
TE<-head(TE,n=25)
TE$POS<-'TE'
TE$POSRANK<-seq.int(nrow(TE))

FFPROS<-bind_rows(QB,RB,WR,TE)

FFPROS<-FFPROS %>%
  separate(Name, sep = -3, into = c("Name", "Team")) %>%
  mutate_all(funs(str_trim(.)))

# 
# ##Scrape from ESPN
# 
# espnurl1<-'https://fantasy.espn.com/football/players/projections'
# espnurl2<-'http://games.espn.com/ffl/tools/projections?&startIndex=40'
# espnurl3<-'http://games.espn.com/ffl/tools/projections?&startIndex=80'
# espnurl4<-'http://games.espn.com/ffl/tools/projections?&startIndex=120'
# espnurl5<-'http://games.espn.com/ffl/tools/projections?&startIndex=160'
# espnurl6<-'http://games.espn.com/ffl/tools/projections?&startIndex=200'
# espnxpath<-'//*[@id="playertable_0"]'
# 
# ESPN1 <- espnurl1 %>%
#   html() %>%
#   html_nodes(xpath=espnxpath) %>%
#   html_table()
# ESPN1 <- ESPN1[[1]]
# 
# ESPN2 <- espnurl2 %>%
#   html() %>%
#   html_nodes(xpath=espnxpath) %>%
#   html_table()
# ESPN2 <- ESPN2[[1]]
# 
# ESPN3 <- espnurl3 %>%
#   html() %>%
#   html_nodes(xpath=espnxpath) %>%
#   html_table()
# ESPN3 <- ESPN3[[1]]
# 
# ESPN4 <- espnurl4 %>%
#   html() %>%
#   html_nodes(xpath=espnxpath) %>%
#   html_table()
# ESPN4 <- ESPN4[[1]]
# 
# ESPN5 <- espnurl5 %>%
#   html() %>%
#   html_nodes(xpath=espnxpath) %>%
#   html_table()
# ESPN5 <- ESPN5[[1]]
# 
# ESPN6 <- espnurl6 %>%
#   html() %>%
#   html_nodes(xpath=espnxpath) %>%
#   html_table()
# ESPN6 <- ESPN6[[1]]
# 
# ESPN<-rbind(ESPN1,ESPN2,ESPN3,ESPN4,ESPN5,ESPN6)
# ESPN<-ESPN[-c(1), ] 
# names(ESPN) <- c("Rank","Name","compatt","ESPN_pass_yds","ESPN_pass_tds","ESPN_pass_ints","rush","ESPN_rush_yds","ESPN_rush_tds","ESPN_rec","ESPN_rec_yds","ESPN_rec_tds","pts")
# 
# ESPN<-ESPN %>%
#   separate(Name, sep =",",into = c("Name","teamposition")) %>%
#   separate(teamposition, sep =4,into = c("Teamno","positionno")) %>%
#   mutate_all(funs(str_trim(.)))
# 
# 
# ESPN$Name[ESPN$Name=="Todd Gurley II"]<-"Todd Gurley"
# ESPN$Name[ESPN$Name=="Will Fuller V"]<-"Will Fuller"
# ESPN$Name[ESPN$Name=="Duke Johnson Jr."]<-"Duke Johnson"
# ESPN$Name[ESPN$Name=="Marvin Jones Jr."]<-"Marvin Jones"
# ESPN$Name[ESPN$Name=="Alshon Jeffery*."]<-"Alshon Jeffery"
# ESPN$Name[ESPN$Name=="DeVante Parker"]<-"Devante Parker"
# ESPN$Name[ESPN$Name=="Mark Ingram II"]<-"Mark Ingram"
# 
# 
# ##Bind
Alldata<-FFPROS


#Alldata <- left_join(FFPROS,ESPN, by=c("Name","Name"))


##Remove columns and set to numeric
Alldata[,3:12]<-sapply(Alldata[,3:12],function(x) as.numeric(gsub(",", "", x)))
Alldata[,14:17]<-sapply(Alldata[,14:17],function(x) as.numeric(gsub(",", "", x)))
Alldata[,21:31]<-sapply(Alldata[,21:31],function(x) as.numeric(gsub(",", "", x)))

Alldata[is.na(Alldata)] <- 0


##Apply Scoring Syatem




Alldata$EXPPTS <- (Alldata$pass_yds)/25+
  (Alldata$pass_tds*4)+
  (Alldata$rush_yds/10)+
  (Alldata$rush_tds*6)+
  (Alldata$rec/2)+
  (Alldata$rec_yds)/10+
  (Alldata$rec_tds)*6-
  ((Alldata$FL)*2+(Alldata$pass_int*2))

# 
# Alldata$ESPNPTS <- (Alldata$pass_yds)/25+
#   (Alldata$pass_tds*4)+
#   (Alldata$rush_yds/10)+
#   (Alldata$rush_tds*6)+
#   (Alldata$rec/4)+
#   (Alldata$rec_yds)/10+
#   (Alldata$rec_tds)*6-
#   ((Alldata$FL)*2+(Alldata$pass_int*2))
# 
# Alldata$ESPN <- (Alldata$ESPN_pass_yds)/25+
#   (Alldata$ESPN_pass_tds*4)+
#   (Alldata$ESPN_rush_yds/10)+
#   (Alldata$ESPN_rush_tds*6)+
#   (Alldata$ESPN_rec/4)+
#   (Alldata$ESPN_rec_yds)/10+
#   (Alldata$ESPN_rec_tds)*6-
#   ((Alldata$FL)*2+(Alldata$ESPN_pass_int*2))


AlldataQB<-Alldata%>%
  filter(POS=="QB")%>%
  arrange(-EXPPTS)%>%
  mutate(POSRANK=row_number())
QBRep<-AlldataQB$EXPPTS[30]
AlldataQB$VoRP<-AlldataQB$EXPPTS-QBRep

AlldataRB<-Alldata%>%
  filter(POS=="RB")%>%
  arrange(-EXPPTS)%>%
  mutate(POSRANK=row_number())
RBRep<-AlldataRB$EXPPTS[38]
AlldataRB$VoRP<-AlldataRB$EXPPTS-RBRep

#10*12-30-38-52

AlldataWR<-Alldata%>%
  filter(POS=="WR")%>%
  arrange(-EXPPTS)%>%
  mutate(POSRANK=row_number())
WRRep<-AlldataWR$EXPPTS[52]
AlldataWR$VoRP<-AlldataWR$EXPPTS-WRRep

AlldataTE<-Alldata%>%
  filter(POS=="TE")%>%
  arrange(-EXPPTS)%>%
  mutate(POSRANK=row_number())
TERep<-AlldataTE$EXPPTS[15]
AlldataTE$VoRP<-AlldataTE$EXPPTS-TERep


AllData<-rbind(AlldataQB,AlldataRB,AlldataWR,AlldataTE)

breaks<-getJenksBreaks(AllData$VoRP,11)

AllData$Jenks<-cut(AllData$VoRP, breaks = breaks, labels=as.character(1:10))

#AllData$ESPNOverUnder<-AllData$EXPPTS/AllData$ESPN

#AllData[,18:24]<-sapply(AllData[,18:24],function(x) as.numeric(gsub(",", "", x)))
AllData$Auction[AllData$Jenks==10]<-80
AllData$Auction[AllData$Jenks==9]<-70
AllData$Auction[AllData$Jenks==8]<-60
AllData$Auction[AllData$Jenks==7]<-50
AllData$Auction[AllData$Jenks==6]<-40
AllData$Auction[AllData$Jenks==5]<-30
AllData$Auction[AllData$Jenks==4]<-20
AllData$Auction[AllData$Jenks==3]<-10
AllData$Auction[AllData$Jenks==2]<-8
AllData$Auction[AllData$Jenks==1]<-6

AllData$Tier[AllData$Jenks==10]<-1
AllData$Tier[AllData$Jenks==9]<-2
AllData$Tier[AllData$Jenks==8]<-3
AllData$Tier[AllData$Jenks==7]<-4
AllData$Tier[AllData$Jenks==6]<-5
AllData$Tier[AllData$Jenks==5]<-6
AllData$Tier[AllData$Jenks==4]<-7
AllData$Tier[AllData$Jenks==3]<-8
AllData$Tier[AllData$Jenks==2]<-9
AllData$Tier[AllData$Jenks==1]<-10

AllData<-AllData%>%
  filter(!is.na(Jenks))
  

path<-"C:/Users/rcarder/Documents/dev/ffcheatsheet/datatest.json"

AllData %>% 
  toJSON() %>%
  write_lines(path)

write.csv(AllData,"AllData.csv")



