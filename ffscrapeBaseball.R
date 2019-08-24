

setwd("C:/Users/rcarder/Documents/dev/ffcheatsheet")

install.packages("rvest")
install.packages("BAMMtools")
install.packages("readr")

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

standings<-read.csv("Standings.csv",header = TRUE)

standings<-standings %>%
  arrange(R) %>%
  mutate(Rrank=seq.int(nrow(standings)))%>%
  arrange(AVG) %>%
  mutate(AVGrank=seq.int(nrow(standings)))%>%
  arrange(HR) %>%
  mutate(HRrank=seq.int(nrow(standings)))%>%
  arrange(RBI) %>%
  mutate(RBIrank=seq.int(nrow(standings)))%>%
  arrange(SB) %>%
  mutate(SBrank=seq.int(nrow(standings)))


Rfit <- lm(standings$R ~ standings$Rrank)
RSDG<-Rfit$coefficients[2]

HRfit <- lm(standings$HR ~ standings$HRrank)
HRSDG<-HRfit$coefficients[2]

RBIfit <- lm(standings$RBI ~ standings$RBIrank)
RBISDG<-RBIfit$coefficients[2]

#OBPfit <- lm(standings$OBP ~ standings$OBPrank)
#OBPSDG<-OBPfit$coefficients[2]

SBfit <- lm(standings$SB ~ standings$SBrank)
SBSDG<-SBfit$coefficients[2]




##Scrape from FantasyPros

positions<-c("1b","2b","3b","ss","of","c")
numplayers<-c(20,20,20,20,60,20)

xpathpath<-'//*[@id="data"]'

j<-0

for (i in positions){
  url<-paste("https://www.fantasypros.com/mlb/projections/",i,".php",sep='')
  
  j<-j+1
  n<-numplayers[j]
  
  hitters <- url %>%
    html() %>%
    html_nodes(xpath=xpathpath) %>%
    html_table(fill=TRUE)
  hitters <- hitters[[1]]
  hitters<-head(hitters,n=n)
  hitters$POSRANK<-seq.int(nrow(hitters))
  hitters<-hitters[,-c(17,18)]
  
  
  hitters<-hitters %>%
    mutate(POS=paste(i,sep=''),
      Rscaled=scale(R,center=TRUE, scale=TRUE)[,],
           HRscaled=scale(HR,center=TRUE, scale=TRUE)[,],
           RBIscaled=scale(RBI,center=TRUE, scale=TRUE)[,],
           SBscaled=scale(SB,center=TRUE, scale=TRUE)[,],
           OBPscaled=scale((OBP),center=TRUE, scale=TRUE)[,],
           #OBPSGP=OBP/OBPSDG,
           RSGP=R/RSDG,
           HRSGP=HR/HRSDG,
           RBISGP=RBI/RBISDG,
           SBSGP=SB/SBSDG) 
  hitters<- hitters %>%
    mutate(totalScaled=Rscaled+HRscaled+RBIscaled+OBPscaled+SBscaled,
           totalSGP=RSGP+HRSGP+RBISGP+SBSGP)
  

  nam <- paste("pos",i, sep = "")
  assign(nam, hitters)
  
}


Overall<-bind_rows(pos1b,pos2b,pos3b,posss,posc,posof)





##Apply Scoring Syatem
AllData<-Overall

breaks<-getJenksBreaks(AllData$totalScaled,11)

AllData$Jenks<-cut(AllData$totalScaled, breaks = breaks, labels=as.character(1:10))

AllData<-AllData %>%
  arrange(-totalScaled) %>%
  mutate(OverallRank=seq.int(nrow(AllData)), label=paste(Player, " as ",toupper(POS),". Total: ",round(totalScaled,2), sep=''))


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


write.csv(AllData,"AllData.csv", row.names = FALSE)

path<-"C:/Users/rcarder/Documents/dev/ffcheatsheet/baseballdatatest.json"

AllData %>% 
  toJSON() %>%
  write_lines(path)

write.csv(AllData,"AllData.csv")




##Heatmap

heattheme<-  theme(
  axis.title = element_text( color="#000000",family="Montserrat Light", size=7),
  text = element_text( color="#000000",family="Montserrat Light", size=5),
  legend.position="none",
  axis.text.x = element_text(angle=35,color="#000000",family="Montserrat Light", hjust=1,size=7),
  axis.text.y = element_text(color="#000000",family="Montserrat Light",size=7),
  axis.title.x=element_blank(),
  axis.title.y=element_blank(),
  panel.background = element_blank(),
  plot.margin = margin(10, 10, 10, 40),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank())


##Wide to Long
p<-AllData
p<-p[!is.na(p$Tier),]
p$label<-fct_rev(p$label)
p$label<-fct_reorder(p$label,p$totalScaled,.desc=FALSE)

p<-p %>%
  gather("Stat","value",Rscaled:AVGscaled)

##Plot
StratHeat<-ggplot(data = p, aes( y=label,x=Stat, fill=value)) + 
  geom_tile()+
  geom_text(aes(label=round(value,2)), color="#000000", family="Montserrat Light",size=3)+
  scale_fill_distiller(palette = "RdYlGn", limits=c(-3,3),direction = 1)+heattheme


ggsave("HeatStrat.pdf", plot = StratHeat, device = NULL, path = NULL,
       scale = 1, width = 10, height = 16)
embed_fonts("HeatStrat.pdf", outfile="HeatStrat.pdf")




##Pitchers


##Scrape from FantasyPros

positions<-"sp"
numplayers<-c(100,40)

xpathpath<-'//*[@id="data"]'

j<-0

for (i in positions){
  url<-paste("https://www.fantasypros.com/mlb/projections/",i,".php",sep='')
  
  j<-j+1
  n<-numplayers[j]
  
  pitchers <- url %>%
    html() %>%
    html_nodes(xpath=xpathpath) %>%
    html_table(fill=TRUE)
  pitchers <- pitchers[[1]]
  pitchers<-head(pitchers,n=n)
  pitchers$POSRANK<-seq.int(nrow(pitchers))
  pitchers<-pitchers[,-c(17,18)]
  
  
  pitchers<-pitchers %>%
    mutate(POS=paste(i,sep=''),
           Wscaled=scale(W,center=TRUE, scale=TRUE)[,],
           ERAscaled=-scale(ERA,center=TRUE, scale=TRUE)[,],
           Kscaled=scale(K,center=TRUE, scale=TRUE)[,],
           WHIPscaled=-scale(WHIP,center=TRUE, scale=TRUE)[,]
        ) 
  pitchers<- pitchers %>%
    mutate(totalScaled=Wscaled+ERAscaled+Kscaled+WHIPscaled)
  
  
  nam <- paste("pos",i, sep = "")
  assign(nam, pitchers)
  
}


OverallPitchers<-possp





AllDataPitchers<-OverallPitchers

breaks<-getJenksBreaks(AllDataPitchers$totalScaled,11)

AllDataPitchers$Jenks<-cut(AllDataPitchers$totalScaled, breaks = breaks, labels=as.character(1:10))

AllDataPitchers<-AllDataPitchers %>%
  arrange(-totalScaled) %>%
  mutate(OverallRank=seq.int(nrow(AllDataPitchers)), label=paste(Player, " as ",toupper(POS),". Total: ",round(totalScaled,2), sep=''))


AllDataPitchers$Tier[AllDataPitchers$Jenks==10]<-1
AllDataPitchers$Tier[AllDataPitchers$Jenks==9]<-2
AllDataPitchers$Tier[AllDataPitchers$Jenks==8]<-3
AllDataPitchers$Tier[AllDataPitchers$Jenks==7]<-4
AllDataPitchers$Tier[AllDataPitchers$Jenks==6]<-5
AllDataPitchers$Tier[AllDataPitchers$Jenks==5]<-6
AllDataPitchers$Tier[AllDataPitchers$Jenks==4]<-7
AllDataPitchers$Tier[AllDataPitchers$Jenks==3]<-8
AllDataPitchers$Tier[AllDataPitchers$Jenks==2]<-9
AllDataPitchers$Tier[AllDataPitchers$Jenks==1]<-10


write.csv(AllDataPitchers,"AllDataPitchers.csv", row.names = FALSE)






