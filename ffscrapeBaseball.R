

setwd("C:/Users/RichardCarder/Documents/dev/ffcheatsheet")

#install.packages("rvest")
#install.packages("BAMMtools")
#install.packages("readr")

library(rvest)
library(BAMMtools)
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
  arrange(OBP) %>%
  mutate(OBPrank=seq.int(nrow(standings)))%>%
  arrange(HR) %>%
  mutate(HRrank=seq.int(nrow(standings)))%>%
  arrange(RBI) %>%
  mutate(RBIrank=seq.int(nrow(standings)))%>%
  arrange(SB) %>%
  mutate(SBrank=seq.int(nrow(standings)))%>%
  arrange(W) %>%
  mutate(Wrank=seq.int(nrow(standings)))%>%
  arrange(K) %>%
  mutate(Krank=seq.int(nrow(standings)))%>%
  arrange(ERA) %>%
  mutate(ERArank=seq.int(from=10,to=1))%>%
  arrange(WHIP) %>%
  mutate(WHIPrank=seq.int(from=10,to=1))%>%
  arrange(SV) %>%
  mutate(SVrank=seq.int(nrow(standings)))


Rfit <- lm(standings$R ~ standings$Rrank)
RSDG<-Rfit$coefficients[2]

HRfit <- lm(standings$HR ~ standings$HRrank)
HRSDG<-HRfit$coefficients[2]

RBIfit <- lm(standings$RBI ~ standings$RBIrank)
RBISDG<-RBIfit$coefficients[2]

OBPfit <- lm(standings$OBP ~ standings$OBPrank)
OBPSDG<-OBPfit$coefficients[2]

SBfit <- lm(standings$SB ~ standings$SBrank)
SBSDG<-SBfit$coefficients[2]

Wfit <- lm(standings$W ~ standings$Wrank)
WSDG<-Wfit$coefficients[2]

ERAfit <- lm(standings$ERA ~ standings$ERArank)
ERASDG<-ERAfit$coefficients[2]

WHIPfit <- lm(standings$WHIP ~ standings$WHIPrank)
WHIPSDG<-WHIPfit$coefficients[2]

Kfit <- lm(standings$K ~ standings$Krank)
KSDG<-Kfit$coefficients[2]

SVfit <- lm(standings$SV ~ standings$SVrank)
SVSDG<-SVfit$coefficients[2]


##Scrape from FantasyPros

positions<-c("1b","2b","3b","ss","of","c")
numplayers<-c(30,30,30,30,70,20)

positionspitchers<-c("sp","rp")
numpitchers<-c(100,40)

xpathpath<-'//*[@id="data"]'

j<-0




for (i in positions){
  
  url<-paste("https://www.fantasypros.com/mlb/projections/",i,".php",sep='')
  
  j<-j+1
  n<-numplayers[j]
  

  
  hitters <- url %>%
    read_html() %>%
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
           mutate(totalScaled=Rscaled+HRscaled+RBIscaled+OBPscaled+SBscaled)
  

  nam <- paste("pos",i, sep = "")
  assign(nam, hitters)
  
}



  url<-paste("https://www.fantasypros.com/mlb/projections/rp.php",sep='')
  
  n<-40
  
  pitchers <- url %>%
    read_html() %>%
    html_nodes(xpath=xpathpath) %>%
    html_table(fill=TRUE)
  pitchers <- pitchers[[1]]
  pitchers<-head(pitchers,n=n)
  pitchers$POSRANK<-seq.int(nrow(pitchers))
  pitchers<-pitchers[,-c(17,18)]%>%
    mutate(Hits=H,
           Walks=BB)%>%
    dplyr::select(-H,-BB,-HR)
  
  
  
  pitchers<-pitchers %>%
    mutate(POS=paste(i,sep=''),
           ERAscaled=-scale(ERA,center=TRUE, scale=TRUE)[,],
           WHIPscaled=-scale(WHIP,center=TRUE, scale=TRUE)[,],
           Kscaled=scale(K,center=TRUE, scale=TRUE)[,],
           SVscaled=scale(SV,center=TRUE, scale=TRUE)[,],
           #OBPSGP=OBP/OBPSDG,
           SVSGP=SV/as.numeric(SVSDG)) 
  posrp<- pitchers %>%
    mutate(totalScaled=ERAscaled+WHIPscaled+Kscaled+SVscaled)
  
  url<-paste("https://www.fantasypros.com/mlb/projections/sp.php",sep='')
  
  n<-100
  
  SPpitchers <- url %>%
    read_html() %>%
    html_nodes(xpath=xpathpath) %>%
    html_table(fill=TRUE)
  SPpitchers <- SPpitchers[[1]]
  SPpitchers<-head(SPpitchers,n=n)
  SPpitchers$POSRANK<-seq.int(nrow(SPpitchers))
  SPpitchers<-SPpitchers[,-c(17,18)]%>%
    mutate(Hits=H,
           Walks=BB)%>%
    dplyr::select(-H,-BB,-HR)
  
  
  
  SPpitchers<-SPpitchers %>%
    mutate(POS=paste(i,sep=''),
           ERAscaled=-scale(ERA,center=TRUE, scale=TRUE)[,],
           WHIPscaled=-scale(WHIP,center=TRUE, scale=TRUE)[,],
           Kscaled=scale(K,center=TRUE, scale=TRUE)[,],
           QSscaled=scale(QS,center=TRUE, scale=TRUE)[,],
           SV=0,
           SVscaled=0,
           SVSGP=0,
           #OBPSGP=OBP/OBPSDG,
           KSGP=K/as.numeric(KSDG),
           QSSGP=QS/as.numeric(WSDG))
  possp<- SPpitchers %>%
    mutate(totalScaled=ERAscaled+WHIPscaled+Kscaled+QSscaled)
  
OverallPitchers<-bind_rows(possp,posrp)

OverallPitchers<-bind_rows(pitchers,SPpitchers)


Overall<-bind_rows(pos1b,pos2b,pos3b,posss,posc,posof)%>%
  arrange(-totalScaled)%>%
  bind_rows(OverallPitchers)


ForCalcs<-Overall[!duplicated(Overall$Player),]%>%
  head(90)

totalAB<-mean(ForCalcs$AB+ForCalcs$BB)*9
totalOB<-mean(ForCalcs$H+ForCalcs$BB)*9
totalOB/totalAB
mean(ForCalcs$OBP)


playerAB<-mean(ForCalcs$AB)
playerOB<-mean(ForCalcs$AB)*(mean(standings$OBP))

calcAB<-totalAB-playerAB
calcOB<-totalOB-playerOB

Overall<-Overall%>%
  mutate(calcOBP=(H+BB)/(AB+BB),
    OBPSGP=((H+BB+totalOB)/(BB+totalAB+AB)-mean(standings$OBP))/OBPSDG,
    totalSGP=RSGP+HRSGP+RBISGP+SBSGP+OBPSGP)

Overall<-Overall[!duplicated(Overall$Player),]





##Apply Scoring Syatem
AllData<-Overall

breaks<-getJenksBreaks(AllData$totalSGP,11)

AllData$Jenks<-cut(AllData$totalSGP, breaks = breaks, labels=as.character(1:10))

AllData<-AllData %>%
  arrange(-totalSGP) %>%
  mutate(OverallRank=seq.int(nrow(AllData)), label=paste(Player, " as ",toupper(POS),". Total: ",round(totalSGP,2), sep=''))


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
write.csv(OverallPitchers,"Pitchers.csv", row.names = FALSE)

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
  mutate(TotalSGP=QSSGP+KSGP+ERASGP+WHIPSGP+SVSGP) %>%
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










# Load necessary libraries
library(dplyr)

# Load last year's standings and player projections
standings <- read.csv("league_standings_last_year.csv")  # Replace with actual file
projections <- Overall[!duplicated(Overall$Player),]      # Replace with actual file

# Define categories
counting_stats <- c("R", "HR", "RBI", "SB", "K", "QS", "SV")  # Standard counting stats
rate_stats <- c("OBP", "ERA", "WHIP")                         # Rate stats

# Function to calculate SGP for counting stats
calculate_sgp <- function(stat, standings_data) {
  sorted_values <- sort(standings_data[[stat]], decreasing = TRUE)
  sgp_factor <- mean(diff(sorted_values))  # Average gap between ranks
  return(sgp_factor)
}

# Compute SGP values for counting stats
sgp_values <- sapply(counting_stats, function(cat) calculate_sgp(cat, standings))

# ---- Handling Rate Stats ----
# OBP = (H + BB + HBP) / (AB + BB + HBP + SF)
# ERA = (ER * 9) / IP
# WHIP = (BB + H) / IP

# Calculate SGP for rate stats using their underlying components
calculate_rate_sgp <- function(rate_stat, num_col, denom_col, standings_data) {
  # Compute rate stats for last year’s teams
  rate_values <- standings_data[[num_col]] / standings_data[[denom_col]]
  sorted_values <- sort(rate_values, decreasing = (rate_stat == "OBP"))  # OBP higher is better
  sgp_factor <- mean(diff(sorted_values))  # Average gap between ranks
  return(sgp_factor)
}

# Compute SGP factors for rate stats
sgp_values["OBP"] <- calculate_rate_sgp("OBP", "H + BB", "AB + BB", standings)
sgp_values["ERA"] <- calculate_rate_sgp("ERA", "ER * 9", "IP", standings)
sgp_values["WHIP"] <- calculate_rate_sgp("WHIP", "Walks + Hits", "IP", standings)

# ---- Apply SGP calculations to player projections ----
rank_players <- function(projections, sgp_values) {
  projections %>%
    mutate(
      across(all_of(counting_stats), ~ . / sgp_values[cur_column()], .names = "SGP_{.col}"),
      SGP_OBP = ((H + BB) / (AB + BB)) / sgp_values["OBP"],
      SGP_ERA = ((ER * 9) / IP) / sgp_values["ERA"],
      SGP_WHIP = ((Walks + Hits) / IP) / sgp_values["WHIP"]
    ) %>%
    rowwise() %>%
    mutate(Total_SGP = sum(c_across(starts_with("SGP_")))) %>%
    arrange(desc(Total_SGP))
}

# Rank players
ranked_players <- rank_players(projections, sgp_values)

# Save results
write.csv(ranked_players, "SGP_ranked_players.csv", row.names = FALSE)

# Display top players
print(head(ranked_players, 10))






#####################################################################
# Comprehensive SGP (Standings Gain Points) System for Fantasy Baseball
#####################################################################

#' Calculate SGP values from league standings
#' @param standings_data Data frame with league standings data
#' @param positive_cats Vector of column names where higher values are better
#' @param negative_cats Vector of column names where lower values are better
#' @return Data frame with SGP values for each category
calculate_sgp_values <- function(standings_data, positive_cats, negative_cats) {
  # Calculate average difference between teams for each category
  sgp_values <- data.frame(Category = c(positive_cats, negative_cats), 
                           SGP_Value = numeric(length(positive_cats) + length(negative_cats)))
  
  # Process all categories
  for (cat in c(positive_cats, negative_cats)) {
    # Sort values appropriately (ascending for negative cats, descending for positive cats)
    if (cat %in% positive_cats) {
      sorted_values <- sort(standings_data[[cat]], decreasing = TRUE)
    } else {
      sorted_values <- sort(standings_data[[cat]], decreasing = FALSE)
    }
    
    # Calculate differences between adjacent teams
    diffs <- abs(diff(sorted_values))
    
    # Use median difference as SGP value
    sgp_values[sgp_values$Category == cat, "SGP_Value"] <- median(diffs)
  }
  
  return(sgp_values)
}

#' Calculate replacement level SGP values based on roster requirements
#' @param player_projections Data frame with player projections and SGP values
#' @param roster_requirements Named list with number of players required by position
#' @param teams Number of teams in the league
#' @return Data frame with replacement level SGP values by position
calculate_replacement_levels <- function(player_projections, roster_requirements, teams) {
  replacement_levels <- data.frame(Position = names(roster_requirements),
                                   Replacement_SGP = numeric(length(roster_requirements)))
  
  for (pos in names(roster_requirements)) {
    # Calculate how many players at this position will be rostered
    num_rostered <- roster_requirements[[pos]] * teams
    
    # Filter players by position
    pos_players <- player_projections[player_projections$Position == pos, ]
    
    # If there aren't enough players at this position, use the minimum available
    if (nrow(pos_players) <= num_rostered) {
      replacement_levels[replacement_levels$Position == pos, "Replacement_SGP"] <- 
        min(pos_players$Total_SGP, na.rm = TRUE)
    } else {
      # Sort by Total_SGP and get the replacement level (first player not rostered)
      pos_players <- pos_players[order(-pos_players$Total_SGP), ]
      replacement_levels[replacement_levels$Position == pos, "Replacement_SGP"] <- 
        pos_players$Total_SGP[num_rostered + 1]
    }
  }
  
  return(replacement_levels)
}

#' Calculate SGP for individual players
#' @param player_projections Data frame with player projections
#' @param sgp_values Data frame with SGP values needed to gain 1 point in standings
#' @param replacement_levels Data frame with replacement level values by position
#' @param league_totals Optional list with league totals for rate stats calculations
#' @return Data frame with player projections and SGP values
calculate_player_sgp <- function(player_projections, sgp_values, replacement_levels = NULL, 
                                 league_totals = NULL) {
  # Create results data frame
  results <- player_projections
  
  # Convert sgp_values from data frame to named vector for easier access
  sgp_vector <- setNames(sgp_values$SGP_Value, sgp_values$Category)
  
  # Initialize SGP columns
  for (cat in sgp_values$Category) {
    results[[paste0("SGP_", cat)]] <- 0
  }
  
  # Process counting stats (positive categories except OBP)
  counting_cats <- setdiff(sgp_values$Category[sgp_values$Category %in% names(player_projections)], 
                           c("OBP", "ERA", "WHIP"))
  
  for (cat in counting_cats) {
    results[[paste0("SGP_", cat)]] <- ifelse(!is.na(results[[cat]]), 
                                             results[[cat]] / sgp_vector[[cat]], 0)
  }
  
  # Process rate stats
  
  # Calculate league totals if not provided
  if (is.null(league_totals)) {
    league_totals <- list()
    
    # OBP components
    if (all(c("PA", "OBE") %in% names(results)) || all(c("AB", "BB", "H") %in% names(results))) {
      if (all(c("PA", "OBE") %in% names(results))) {
        league_totals$total_pa <- sum(results$PA, na.rm = TRUE)
        league_totals$total_obe <- sum(results$OBE, na.rm = TRUE)
      } else {
        # Calculate PA and OBE from components
        results$PA_calc <- results$AB + results$BB
        results$OBE_calc <- results$H + results$BB
        league_totals$total_pa <- sum(results$PA_calc, na.rm = TRUE)
        league_totals$total_obe <- sum(results$OBE_calc, na.rm = TRUE)
      }
    }
    
    # Pitching components
    if (all(c("IP", "ER") %in% names(results))) {
      league_totals$total_ip <- sum(results$IP, na.rm = TRUE)
      league_totals$total_er <- sum(results$ER, na.rm = TRUE)
    }
    
    if (all(c("IP", "H_allowed", "BB_allowed") %in% names(results))) {
      if (!("total_ip" %in% names(league_totals))) {
        league_totals$total_ip <- sum(results$IP, na.rm = TRUE)
      }
      league_totals$total_h <- sum(results$H_allowed, na.rm = TRUE)
      league_totals$total_bb <- sum(results$BB_allowed, na.rm = TRUE)
    }
  }
  
  # OBP calculation
  if ("OBP" %in% sgp_values$Category) {
    if (all(c("PA", "OBE") %in% names(results))) {
      # Use provided OBE and PA
      pa_col <- "PA"
      obe_col <- "OBE"
    } else if (all(c("AB", "BB", "H") %in% names(results))) {
      # Calculate PA and OBE
      results$PA_calc <- results$AB + results$BB
      results$OBE_calc <- results$H + results$BB
      pa_col <- "PA_calc"
      obe_col <- "OBE_calc"
    } else {
      warning("Cannot calculate OBP SGP without PA/OBE or AB/BB/H")
      pa_col <- NULL
    }
    
    if (!is.null(pa_col)) {
      # Calculate league average OBP
      league_obp <- league_totals$total_obe / league_totals$total_pa
      
      # Calculate OBP impact for each player
      results$SGP_OBP <- ifelse(results[[pa_col]] > 0, 
                                ((league_totals$total_obe + results[[obe_col]]) / 
                                   (league_totals$total_pa + results[[pa_col]]) - 
                                   (league_totals$total_obe - results[[obe_col]]) / 
                                   (league_totals$total_pa - results[[pa_col]])) / 
                                  sgp_vector[["OBP"]], 0)
    }
  }
  
  # ERA calculation
  if ("ERA" %in% sgp_values$Category && all(c("IP", "ER") %in% names(results))) {
    # Calculate league ERA
    league_era <- (league_totals$total_er * 9) / league_totals$total_ip
    
    # Calculate ERA impact (negative because lower is better)
    results$SGP_ERA <- ifelse(results$IP > 0, 
                              -1 * ((((league_totals$total_er - results$ER) * 9) / 
                                       (league_totals$total_ip - results$IP)) - 
                                      ((league_totals$total_er * 9) / league_totals$total_ip)) / 
                                sgp_vector[["ERA"]], 0)
  }
  
  # WHIP calculation
  if ("WHIP" %in% sgp_values$Category && 
      all(c("IP", "H_allowed", "BB_allowed") %in% names(results))) {
    # Calculate league WHIP
    league_whip <- (league_totals$total_h + league_totals$total_bb) / league_totals$total_ip
    
    # Calculate WHIP impact (negative because lower is better)
    results$SGP_WHIP <- ifelse(results$IP > 0, 
                               -1 * (((league_totals$total_h + league_totals$total_bb - 
                                         results$H_allowed - results$BB_allowed) / 
                                        (league_totals$total_ip - results$IP)) - 
                                       ((league_totals$total_h + league_totals$total_bb) / 
                                          league_totals$total_ip)) / 
                                 sgp_vector[["WHIP"]], 0)
  }
  
  # Calculate total SGP
  sgp_columns <- grep("^SGP_", names(results), value = TRUE)
  results$Total_SGP <- rowSums(results[, sgp_columns], na.rm = TRUE)
  
  # Apply replacement level adjustments if provided
  if (!is.null(replacement_levels)) {
    results <- merge(results, replacement_levels, by = "Position", all.x = TRUE)
    results$SGP_VAR <- results$Total_SGP - results$Replacement_SGP
  } else {
    results$SGP_VAR <- results$Total_SGP
  }
  
  # Return results sorted by SGP_VAR (value above replacement)
  results <- results[order(-results$SGP_VAR), ]
  return(results)
}

#' Complete SGP analysis pipeline
#' @param standings_data Data frame with league standings
#' @param player_projections Data frame with player projections
#' @param positive_cats Vector of column names where higher values are better
#' @param negative_cats Vector of column names where lower values are better
#' @param roster_requirements Named list with number of players required by position
#' @param teams Number of teams in the league
#' @return List with SGP values, replacement levels, and player rankings
run_sgp_analysis <- function(standings_data, player_projections, 
                             positive_cats, negative_cats,
                             roster_requirements, teams) {
  # 1. Calculate SGP values from standings
  sgp_values <- calculate_sgp_values(standings_data, positive_cats, negative_cats)
  
  # 2. Calculate initial SGP for each player (without replacement level adjustment)
  initial_player_sgp <- calculate_player_sgp(player_projections, sgp_values)
  
  # 3. Calculate replacement levels based on roster requirements
  replacement_levels <- calculate_replacement_levels(initial_player_sgp, 
                                                     roster_requirements, teams)
  
  # 4. Calculate final player SGP values with replacement level adjustment
  final_player_sgp <- calculate_player_sgp(player_projections, sgp_values, replacement_levels)
  
  # 5. Return results
  return(list(
    sgp_values = sgp_values,
    replacement_levels = replacement_levels,
    player_rankings = final_player_sgp
  ))
}

#####################################################################
# Example usage with user's standings data
#####################################################################

# Parse the standings data
standings_text <- "
R       HR      RBI     SB      OBP     K       QS      SV      ERA     WHIP
700     211     698     126     0.3238  1720    138     126     3.419   1.146
854     256     815     106     0.3346  1965    126     57      3.874   1.205
743     239     733     171     0.346   1855    131     109     3.641   1.18
750     183     650     109     0.3219  2033    150     89      3.838   1.219
824     251     776     129     0.3422  2080    135     77      4.198   1.281
741     212     722     99      0.336   2010    145     70      3.79    1.241
762     228     740     183     0.3116  1781    103     101     4.339   1.258
783     213     729     175     0.3369  1544    106     50      4.08    1.251
722     208     705     156     0.3309  1898    132     72      4.025   1.228
731     199     742     124     0.3162  1975    141     82      3.852   1.214
"

# Convert to data frame
data_lines <- strsplit(trimws(standings_text), "\n")[[1]]
header <- strsplit(trimws(data_lines[1]), "\\s+")[[1]]
data_rows <- lapply(data_lines[-1], function(line) {
  as.numeric(strsplit(trimws(line), "\\s+")[[1]])
})
standings <- as.data.frame(do.call(rbind, data_rows))
colnames(standings) <- header

# Define positive and negative categories
positive_cats <- c("R", "HR", "RBI", "SB", "OBP", "K", "QS", "SV")
negative_cats <- c("ERA", "WHIP")

# Example function to create mock player projections
create_mock_player_projections <- function(n_players = 300) {
  # Define possible positions and their distribution
  positions <- c("C", "1B", "2B", "3B", "SS", "OF", "SP", "RP")
  position_weights <- c(0.08, 0.08, 0.08, 0.08, 0.08, 0.24, 0.24, 0.12) # Approximate distribution
  
  # Create player IDs and names
  player_ids <- paste0("ID", 1:n_players)
  player_names <- paste0("Player", 1:n_players)
  
  # Assign positions with realistic distribution
  player_positions <- sample(positions, n_players, replace = TRUE, prob = position_weights)
  
  # Initialize data frame
  players <- data.frame(
    PlayerID = player_ids,
    Name = player_names,
    Position = player_positions,
    stringsAsFactors = FALSE
  )
  
  # Add hitting stats for non-pitchers
  hitter_idx <- which(!players$Position %in% c("SP", "RP"))
  n_hitters <- length(hitter_idx)
  
  # Generate hitting stats with realistic distributions
  players$AB <- 0
  players$H <- 0
  players$BB <- 0
  players$R <- 0
  players$HR <- 0
  players$RBI <- 0
  players$SB <- 0
  players$OBP <- 0
  
  # Generate stats for hitters
  for (i in hitter_idx) {
    # Playing time varies by quality and position
    quality_factor <- runif(1, 0.5, 1.5)
    position_factor <- ifelse(players$Position[i] == "C", 0.8, 1.0) # Catchers play less
    
    # Generate at-bats
    players$AB[i] <- round(runif(1, 300, 600) * quality_factor * position_factor)
    
    # Generate other stats based on quality factor
    avg <- runif(1, 0.220, 0.330) * quality_factor
    bb_rate <- runif(1, 0.05, 0.15) * quality_factor
    hr_rate <- runif(1, 0.01, 0.07) * quality_factor
    sb_factor <- ifelse(runif(1) > 0.7, runif(1, 1, 3), runif(1, 0.2, 1)) # Some players steal more
    
    players$H[i] <- round(players$AB[i] * avg)
    players$BB[i] <- round(players$AB[i] * bb_rate)
    players$HR[i] <- round(players$AB[i] * hr_rate)
    players$R[i] <- round((players$H[i] + players$BB[i]) * runif(1, 0.3, 0.5))
    players$RBI[i] <- round(players$HR[i] * runif(1, 3, 4) + 
                              (players$H[i] - players$HR[i]) * runif(1, 0.1, 0.2))
    players$SB[i] <- round(quality_factor * sb_factor * runif(1, 0, 40))
    
    # Calculate OBP
    players$OBP[i] <- round((players$H[i] + players$BB[i]) / (players$AB[i] + players$BB[i]), 4)
  }
  
  # Add pitching stats for pitchers
  pitcher_idx <- which(players$Position %in% c("SP", "RP"))
  n_pitchers <- length(pitcher_idx)
  
  # Initialize pitching columns
  players$IP <- 0
  players$ER <- 0
  players$H_allowed <- 0
  players$BB_allowed <- 0
  players$K <- 0
  players$QS <- 0
  players$SV <- 0
  players$ERA <- 0
  players$WHIP <- 0
  
  # Generate stats for pitchers
  for (i in pitcher_idx) {
    # Pitching time varies by role and quality
    quality_factor <- runif(1, 0.7, 1.3)
    role_factor <- ifelse(players$Position[i] == "SP", 3, 1) # Starters pitch more
    
    # Generate innings pitched
    players$IP[i] <- round(runif(1, 40, 80) * quality_factor * role_factor)
    
    # Generate other stats based on quality and role
    era <- runif(1, 3.0, 5.0) / quality_factor
    whip <- runif(1, 1.1, 1.5) / quality_factor
    k_per_9 <- runif(1, 7, 11) * quality_factor
    
    players$ER[i] <- round(players$IP[i] * era / 9)
    players$BB_allowed[i] <- round(players$IP[i] * runif(1, 2, 4) / 9)
    players$H_allowed[i] <- round(players$IP[i] * whip) - players$BB_allowed[i]
    players$K[i] <- round(players$IP[i] * k_per_9 / 9)
    
    # QS for starters only
    if (players$Position[i] == "SP") {
      # Quality starts - roughly 50-60% of games for good starters
      players$QS[i] <- round(players$IP[i] / 6 * runif(1, 0.4, 0.7) * quality_factor)
    }
    
    # Saves for relievers only
    if (players$Position[i] == "RP") {
      # Higher saves for closers (randomly assigned)
      is_closer <- runif(1) > 0.7
      if (is_closer) {
        players$SV[i] <- round(runif(1, 15, 40) * quality_factor)
      } else {
        players$SV[i] <- round(runif(1, 0, 5))
      }
    }
    
    # Calculate ERA and WHIP
    players$ERA[i] <- round(9 * players$ER[i] / players$IP[i], 3)
    players$WHIP[i] <- round((players$H_allowed[i] + players$BB_allowed[i]) / players$IP[i], 3)
  }
  
  # Add PA and OBE columns (for OBP calculation)
  players$PA <- players$AB + players$BB
  players$OBE <- players$H + players$BB
  
  return(players)
}

# Create mock player projections
player_projections <- create_mock_player_projections(300)

# Define roster requirements (adjust to match your league)
roster_requirements <- list(
  "C" = 1,    # 2 catchers per team
  "1B" = 1,   # 1 first baseman per team
  "2B" = 1,   # 1 second baseman per team
  "3B" = 1,   # 1 third baseman per team
  "SS" = 1,   # 1 shortstop per team
  "OF" = 5,   # 5 outfielders per team
  "SP" = 10,   # 5 starting pitchers per team
  "RP" = 4    # 3 relief pitchers per team
)

# Number of teams in the league
num_teams <- 10

# Run the complete SGP analysis
sgp_results <- run_sgp_analysis(
  standings_data = standings,
  player_projections = player_projections,
  positive_cats = positive_cats,
  negative_cats = negative_cats,
  roster_requirements = roster_requirements,
  teams = num_teams
)

# Display SGP values
cat("SGP Values (how much of each stat needed to gain 1 point in standings):\n")
print(sgp_results$sgp_values)

# Display replacement levels
cat("\nReplacement Level SGP by Position:\n")
print(sgp_results$replacement_levels)

# Display top 20 players by value above replacement
cat("\nTop 20 Players by Value Above Replacement (SGP_VAR):\n")
top_players <- sgp_results$player_rankings[, c("Name", "Position", "Total_SGP", "SGP_VAR")]
print(head(top_players, 20))

#####################################################################
# Function to create a draft cheat sheet with target rounds
#####################################################################

create_draft_cheatsheet <- function(player_rankings, num_teams, num_rounds, adp_data = NULL) {
  # Add draft round based on SGP_VAR ranking
  player_rankings$DraftRound <- ceiling(match(1:nrow(player_rankings), 
                                              order(-player_rankings$SGP_VAR)) / num_teams)
  
  # Filter to players likely to be drafted
  draftable_players <- player_rankings[player_rankings$DraftRound <= num_rounds, ]
  
  # If ADP data is provided, add it and calculate value difference
  if (!is.null(adp_data)) {
    draftable_players <- merge(draftable_players, adp_data, by = "PlayerID", all.x = TRUE)
    draftable_players$ADP_Round <- ceiling(draftable_players$ADP / num_teams)
    draftable_players$ValueDiff <- draftable_players$DraftRound - draftable_players$ADP_Round
  }
  
  # Sort by draft round and SGP_VAR within round
  draftable_players <- draftable_players[order(draftable_players$DraftRound, 
                                               -draftable_players$SGP_VAR), ]
  
  return(draftable_players)
}

# Example: Create a draft cheat sheet for a 23-round draft
draft_cheatsheet <- create_draft_cheatsheet(
  player_rankings = sgp_results$player_rankings,
  num_teams = num_teams,
  num_rounds = 23
)

# Display the cheat sheet by round
cat("\nDraft Cheat Sheet (sample):\n")
for (round in 1:5) {  # Just showing first 5 rounds as example
  cat(paste0("\nRound ", round, ":\n"))
  round_players <- draft_cheatsheet[draft_cheatsheet$DraftRound == round, ]
  print(round_players[, c("Name", "Position", "SGP_VAR")])
}











claude_player_data<-Overall[!duplicated(Overall$Player),]






# Fantasy Baseball SGP Calculator - Updated for your data format

# Load necessary libraries
library(dplyr)

# Function to calculate SGP values
calculate_sgp <- function(player_projections, league_standings, num_teams) {
  # Calculate the SGP values (points per stat)
  sgp_values <- calculate_sgp_values(league_standings)
  
  # Calculate SGP for each player
  player_sgp <- calculate_player_sgp(player_projections, sgp_values, num_teams)
  
  # Calculate replacement level for each position
  replacement_levels <- calculate_replacement_levels(player_sgp, num_teams)
  
  # Calculate value above replacement (VAR)
  player_var <- calculate_var(player_sgp, replacement_levels)
  
  return(list(
    sgp_values = sgp_values,
    player_sgp = player_sgp,
    replacement_levels = replacement_levels,
    player_var = player_var
  ))
}

# Function to calculate SGP values from league standings
calculate_sgp_values <- function(standings) {
  # For each category, calculate the average gain needed to move up one position
  # This is typically done by looking at the middle teams
  
  # We'll use the middle 50% of teams to calculate the SGP values
  middle_start <- floor(nrow(standings) * 0.25) + 1
  middle_end <- floor(nrow(standings) * 0.75)
  middle_teams <- standings[middle_start:middle_end, ]
  
  # Calculate the average difference between adjacent teams for each category
  sgp_values <- list()
  
  # Counting stats (R, HR, RBI, SB, K, QS, SV)
  counting_stats <- c("R", "HR", "RBI", "SB", "K", "QS", "SV")
  for (stat in counting_stats) {
    if (stat %in% colnames(standings)) {
      # Sort by the stat
      sorted_teams <- middle_teams[order(middle_teams[[stat]]), ]
      # Calculate differences between adjacent teams
      diffs <- diff(sorted_teams[[stat]])
      # Average difference is the SGP value
      sgp_values[[stat]] <- mean(diffs)
    }
  }
  
  # Rate stats (OBP, ERA, WHIP)
  # For rate stats, we need to calculate differently
  rate_stats <- c("OBP", "ERA", "WHIP")
  for (stat in rate_stats) {
    if (stat %in% colnames(standings)) {
      # Sort by the stat (ascending for OBP, descending for ERA, WHIP)
      if (stat == "OBP") {
        sorted_teams <- middle_teams[order(middle_teams[[stat]], decreasing = TRUE), ]
      } else {
        sorted_teams <- middle_teams[order(middle_teams[[stat]]), ]
      }
      # Calculate differences between adjacent teams
      diffs <- diff(sorted_teams[[stat]])
      # Average difference is the SGP value
      sgp_values[[stat]] <- mean(abs(diffs))
    }
  }
  
  return(sgp_values)
}

# Function to calculate SGP for each player
calculate_player_sgp <- function(players, sgp_values, num_teams) {
  # Create a copy of the player data
  player_sgp <- players
  
  # Calculate SGP for counting stats
  counting_stats <- c("R", "HR", "RBI", "SB", "K", "QS", "SV")
  for (stat in counting_stats) {
    if (stat %in% colnames(players) && stat %in% names(sgp_values)) {
      player_sgp[[paste0(stat, "_SGP")]] <- players[[stat]] / sgp_values[[stat]]
      
      # If the SGP column already exists in the data, overwrite it
      existing_sgp_col <- paste0(stat, "SGP")
      if (existing_sgp_col %in% colnames(player_sgp)) {
        player_sgp[[existing_sgp_col]] <- player_sgp[[paste0(stat, "_SGP")]]
        player_sgp[[paste0(stat, "_SGP")]] <- NULL  # Remove the temporary column
      }
    }
  }
  
  # Calculate SGP for rate stats - improved version
  
  # For OBP
  if ("OBP" %in% names(sgp_values) && "OBP" %in% colnames(players) && "AB" %in% colnames(players) && "BB" %in% colnames(players)) {
    # Calculate league average OBP (properly weighted by AB)
    player_pa <- players$AB + players$BB  # simplified PA calculation
    total_pa <- sum(player_pa, na.rm = TRUE)
    weighted_obp_sum <- sum(players$OBP * player_pa, na.rm = TRUE)
    league_obp <- weighted_obp_sum / total_pa
    
    # Calculate effect on league OBP
    # For a rate stat like OBP, the player's contribution is proportional to:
    # 1. The difference between their rate and league average
    # 2. The number of opportunities they have (PA)
    player_sgp$OBP_SGP <- (players$OBP - league_obp) * (player_pa / total_pa) / sgp_values$OBP
    
    # Scale the OBP_SGP values to have similar magnitude as counting stats
    scaling_factor <- median(player_sgp$R_SGP, na.rm = TRUE) / median(player_sgp$OBP_SGP, na.rm = TRUE)
    player_sgp$OBP_SGP <- player_sgp$OBP_SGP * scaling_factor
    
    # If the SGP column already exists in the data, overwrite it
    if ("OBPSGP" %in% colnames(player_sgp)) {
      player_sgp$OBPSGP <- player_sgp$OBP_SGP
      player_sgp$OBP_SGP <- NULL  # Remove the temporary column
    }
  }
  
  # For ERA
  if ("ERA" %in% names(sgp_values) && "ERA" %in% colnames(players) && "IP" %in% colnames(players)) {
    # Calculate league average ERA (weighted by IP)
    total_ip <- sum(players$IP, na.rm = TRUE)
    weighted_era_sum <- sum(players$ERA * players$IP, na.rm = TRUE)
    league_era <- weighted_era_sum / total_ip
    
    # For ERA, lower is better so we negate
    player_sgp$ERA_SGP <- -1 * (players$ERA - league_era) * (players$IP / total_ip) / sgp_values$ERA
    
    # Scale the ERA_SGP values to have similar magnitude as counting stats
    scaling_factor <- median(player_sgp$K_SGP, na.rm = TRUE) / median(abs(player_sgp$ERA_SGP), na.rm = TRUE)
    player_sgp$ERA_SGP <- player_sgp$ERA_SGP * scaling_factor
    
    # If the SGP column already exists in the data, overwrite it
    if ("ERASGP" %in% colnames(player_sgp)) {
      player_sgp$ERASGP <- player_sgp$ERA_SGP
      player_sgp$ERA_SGP <- NULL  # Remove the temporary column
    }
  }
  
  # For WHIP
  if ("WHIP" %in% names(sgp_values) && "WHIP" %in% colnames(players) && "IP" %in% colnames(players)) {
    # Calculate league average WHIP (weighted by IP)
    total_ip <- sum(players$IP, na.rm = TRUE)
    weighted_whip_sum <- sum(players$WHIP * players$IP, na.rm = TRUE)
    league_whip <- weighted_whip_sum / total_ip
    
    # For WHIP, lower is better so we negate
    player_sgp$WHIP_SGP <- -1 * (players$WHIP - league_whip) * (players$IP / total_ip) / sgp_values$WHIP
    
    # Scale the WHIP_SGP values to have similar magnitude as counting stats
    scaling_factor <- median(player_sgp$K_SGP, na.rm = TRUE) / median(abs(player_sgp$WHIP_SGP), na.rm = TRUE)
    player_sgp$WHIP_SGP <- player_sgp$WHIP_SGP * scaling_factor
    
    # If the SGP column already exists in the data, overwrite it
    if ("WHIPSGP" %in% colnames(player_sgp)) {
      player_sgp$WHIPSGP <- player_sgp$WHIP_SGP
      player_sgp$WHIP_SGP <- NULL  # Remove the temporary column
    }
  }
  
  # Calculate total SGP
  sgp_columns <- c("RSGP", "HRSGP", "RBISGP", "SBSGP", "OBPSGP", "KSGP", "QSSGP", "SVSGP", "ERASGP", "WHIPSGP")
  sgp_columns <- sgp_columns[sgp_columns %in% colnames(player_sgp)]
  
  player_sgp$Total_SGP <- rowSums(player_sgp[, sgp_columns], na.rm = TRUE)
  
  return(player_sgp)
}

# Function to calculate replacement levels
calculate_replacement_levels <- function(player_sgp, num_teams) {
  # Map the position codes in your data to standard positions
  # Adjust based on your actual position coding
  position_map <- list(
    "C" = "C",
    "1B" = "1B",
    "2B" = "2B",
    "3B" = "3B",
    "SS" = "SS",
    "OF" = "OF",
    "DH" = "UTIL",
    "SP" = "P",
    "RP" = "P"
  )
  
  # Extract positions from the POS column
  player_sgp$Position <- player_sgp$POS
  
  # Handle players with multiple positions
  for (i in 1:nrow(player_sgp)) {
    pos_string <- player_sgp$Position[i]
    # If position has slash, take the first position (most valuable typically)
    if (!is.na(pos_string) && grepl("/", pos_string)) {
      primary_pos <- strsplit(pos_string, "/")[[1]][1]
      player_sgp$Position[i] <- primary_pos
    }
  }
  
  # Determine how many players of each position are starters
  # For a standard league with 2 catchers:
  positions <- c("C", "1B", "2B", "3B", "SS", "OF", "P")
  starters_per_position <- c(
    C = 2 * num_teams,  # 2 catchers per team
    `1B` = num_teams,
    `2B` = num_teams,
    `3B` = num_teams,
    SS = num_teams,
    OF = 5 * num_teams, # 5 outfielders per team
    P = 9 * num_teams   # 9 pitchers per team
  )
  
  # Calculate replacement level for each position
  replacement_levels <- list()
  
  for (pos in positions) {
    # Get players at this position
    pos_players <- player_sgp[player_sgp$Position == pos, ]
    if (nrow(pos_players) > 0) {
      # Sort by Total_SGP
      pos_players <- pos_players[order(pos_players$Total_SGP, decreasing = TRUE), ]
      # Replacement level is the SGP of the first player not starting
      replacement_idx <- starters_per_position[pos] + 1
      if (replacement_idx <= nrow(pos_players)) {
        replacement_levels[[pos]] <- pos_players$Total_SGP[replacement_idx]
      } else {
        replacement_levels[[pos]] <- 0  # If not enough players at position
      }
    } else {
      replacement_levels[[pos]] <- 0  # If no players at this position
    }
  }
  
  return(replacement_levels)
}

# Function to calculate value above replacement (VAR)
calculate_var <- function(player_sgp, replacement_levels) {
  # Create a copy of the player data
  player_var <- player_sgp
  
  # Calculate VAR for each player
  player_var$VAR <- sapply(1:nrow(player_sgp), function(i) {
    pos <- player_sgp$Position[i]
    if (!is.na(pos) && pos %in% names(replacement_levels)) {
      return(player_sgp$Total_SGP[i] - replacement_levels[[pos]])
    } else {
      return(player_sgp$Total_SGP[i])  # If position not in replacement_levels
    }
  })
  
  # Sort by VAR
  player_var <- player_var[order(player_var$VAR, decreasing = TRUE), ]
  
  return(player_var)
}

# Example usage:
# 1. Load your league standings data
# standings <- read.csv("league_standings.csv")
# 
# 2. Load your player projections
# players <- read.csv("player_projections.csv")
# 
# 3. Calculate SGP values
sgp_results <- calculate_sgp(claude_player_data, standings, num_teams = 10)
# 
# 4. View results
# View(sgp_results$player_var)  # Sorted player list with VAR
# sgp_results$sgp_values        # SGP values for each category

# Function to manually calculate SGP for a league when you already know the SGP values
calculate_sgp_with_values <- function(player_projections, sgp_values, num_teams) {
  # Example SGP values if you already have them:
  # sgp_values <- list(
  #   R = 20,
  #   HR = 5,
  #   RBI = 20,
  #   SB = 7,
  #   OBP = 0.006,
  #   K = 40,
  #   QS = 8,
  #   SV = 5,
  #   ERA = 0.15,
  #   WHIP = 0.03
  # )
  
  # Calculate SGP for each player
  player_sgp <- calculate_player_sgp(player_projections, sgp_values, num_teams)
  
  # Calculate replacement level for each position
  replacement_levels <- calculate_replacement_levels(player_sgp, num_teams)
  
  # Calculate value above replacement (VAR)
  player_var <- calculate_var(player_sgp, replacement_levels)
  
  return(list(
    sgp_values = sgp_values,
    player_sgp = player_sgp,
    replacement_levels = replacement_levels,
    player_var = player_var
  ))
}



colnames(claude_player_data)


