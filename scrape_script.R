# Harvest script for fantasy football
#
# Created: 5/7/15
# Author: Aidan Boland

# Use the following command in terminal to run this script....
# cd Google\ Drive/Fantasy\ Football/
# R --slave --args file_name < scrape_script.R

#library(stringr)
library(RCurl)
library(jsonlite)
library(plyr)
library(XML)

#args <- commandArgs(trailingOnly = TRUE)
#file_name <- args[1]

file_name<-paste(strsplit(as.character(Sys.time()),split=" ")[[1]][1],collapse = "")

pb <- txtProgressBar(min = 0, max = 534, style = 3)
full_scrape = list()
for(i in 1:534){
  # 534 is the no. of players as of 5th August
  Sys.sleep(2)
  setTxtProgressBar(pb, i)
  url = sprintf("http://fantasy.premierleague.com/web/api/elements/%s/?format=json", i)
  json = fromJSON(getURL(url))
  full_scrape[[i]] <- json
}
close(pb)

save(full_scrape,file=paste0("/Users/aidanboland/Google Drive/Fantasy Football/Data/",file_name,".RData"))
save(full_scrape,file="/Users/aidanboland/Google Drive/Fantasy Football/FF15-16/current_web_data.RData")

fields <- c("id", "web_name", "event_total", "type_name", "team_name", "selected_by", "total_points", "current_fixture", 
            "next_fixture", "team_code", "news", "team_id", "status", "code", "first_name", "second_name", "now_cost", 
            #"chance_of_playing_next_round", 
            "value_form", "value_season", "cost_change_start", "cost_change_event", 
            "cost_change_start_fall", "cost_change_event_fall", "in_dreamteam", "dreamteam_count", "selected_by_percent", 
            "form", "transfers_out", "transfers_in", "transfers_out_event", "transfers_in_event", "loans_in", "loans_out", 
            "loaned_in", "loaned_out", "event_points", "points_per_game", "ep_next", "special", "minutes", "goals_scored", 
            "assists", "clean_sheets", "goals_conceded", "own_goals", "penalties_saved", "penalties_missed", "yellow_cards", 
            "red_cards", "saves", "bonus", "ea_index", "bps", "element_type", "team")

field_names <- c("id", "Name", "Event_total", "Position", "Team", "% selected by", "Total points", "Current fixture", 
                 "Next fixture", "Team code", "News", "Team id", "Status", "Code", "First name", "Second name", "Cost", 
                 #"chance_of_playing_next_round", 
                 "Value form", "Value season", "Cost change start", "Cost change event", 
                 "Cost change start fall", "Cost change event fall", "in dreamteam", "Dreamteam count", "% selected by2", 
                 "Form", "Transfers out", "Transfers in", "Transfers out week", "Transfers in week", "Loans in", "Loans out", 
                 "Loaned in", "Loaned out", "Event points", "Points per game", "ep_next", "Special", "Minutes", "Goals scored", 
                 "Assists", "Clean sheets", "Goals conceded", "Own goals", "Penalties saved", "Penalties missed", "Yellow cards", 
                 "Red cards", "Saves", "Bonus", "EA index", "bps", "Element type", "Team2")

available_fields <- c("Name", "Position", "Team", "Cost", "Status","% selected by", "Total points", "Current fixture", 
                      "Next fixture", "News", "Minutes", "Goals scored", 
                      "Form", "Transfers out", "Transfers in", "Transfers out week", "Transfers in week",
                      "Assists", "Clean sheets", "Goals conceded", "Own goals", "Penalties saved", "Penalties missed", "Yellow cards", 
                      "Red cards", "Saves", "Bonus", "% selected by2","id")

player_data <- ldply(full_scrape, function(x) data.frame(x [names(x) %in% fields]))
names(player_data) <- field_names
player_data[,1]<-as.character(player_data[,1])
player_data$Cost <- player_data$Cost / 10
player_data[,"% selected by"] <- as.numeric(as.character(player_data[,"% selected by"]))
player_data$Status<-as.character(player_data$Status)
player_data$Status[which(player_data$Status=="u")]<-"Unavailable"
player_data$Status[which(player_data$Status=="a")]<-"Available"
player_data$Status[which(player_data$Status=="n")]<-"(see news)"
player_data$Status[which(player_data$Status=="d")]<-"Doubtful"
player_data$Status[which(player_data$Status=="i")]<-"Injured"
player_data$Status[which(player_data$Status=="s")]<-"Suspended"

save(player_data,fields,field_names,available_fields, file="/Users/aidanboland/Google Drive/Fantasy Football/FF15-16/current_web_data_tidy.RData")

fix_res <- list()
for(i in 1:38)
  fix_res[[i]]<-readHTMLTable(paste0("http://fantasy.premierleague.com/fixtures/",i,"/"))[[1]]
save(fix_res,file=paste0("/Users/aidanboland/Google Drive/Fantasy Football/Data/FixRes",file_name,".RData"))


#library(shinyapps)
shinyapps::deployApp('/Users/aidanboland/Google Drive/Fantasy Football/FF15-16')
