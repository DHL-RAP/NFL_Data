rm(list=ls())

library(plyr)
library(XML)
library(rvest)
library(lubridate)


### Historical Players
historical_players <- data.frame(player_id = as.character(),
                                 position = as.character(), 
                                 player_name= as.character(), 
                                 status = as.character(), 
                                 draft_year = as.numeric(),
                                 years_played = as.numeric())


# Loop through positions

pos_data <- data.frame(position = c("quarterback","runningback","widereceiver","tightend",
                                    "offensiveline","defensivelineman","linebacker",
                                    "defensiveback","kicker","punter"),
                       pages = c(16,55,39,19,83,54,54,66,6,5),
                       abbr_pos = c("QB","RB","WR","TE","OL","DL","LB","CB","K","P"))
p <- 1
for (i in (1:length(pos_data$position))) {
  for (j in (1:pos_data$pages[i])) {
    print(paste0("Scraping page ",p," of ",sum(pos_data$pages),"..."))
    nfl_url <- paste0("http://www.nfl.com/players/search?category=position&filter=",
                      pos_data$position[i], 
                      "&playerType=historical&conference=ALL&d-447263-p=", 
                      j)
    nfl_tmp <- tryCatch(readHTMLTable(nfl_url, 
                                      stringsAsFactors=FALSE, 
                                      header=FALSE)$result[,1:3],
                        error= function(e) {
                          print(paste0("Error Scraping ",pos_data$abbr_pos[i],
                                       ", pg ",j," of ",pos_data$pages[i]),"...")
                        })
    names(nfl_tmp) <- c("player_name","years_played","years")
    
    # Go into HTML links to get player_id
    pge_links <- getHTMLLinks(nfl_url)
    nfl_tmp$player_id <- unique(gsub("[^0-9]","",
                                     pge_links[grep("/profile$", pge_links)]))
    
    # Clean & standardize columns
    nfl_tmp$years_played <- as.numeric(ifelse(nchar(nfl_tmp$years_played)==6,1,
                                              substr(nfl_tmp$years_played,1,
                                                     nchar(nfl_tmp$years_played)-6)))
    nfl_tmp$draft_year <- as.numeric(substr(nfl_tmp$years,1,4))
    nfl_tmp$position <- pos_data$abbr_pos[i]
    nfl_tmp$status <- "RET"
    nfl_tmp$years <- NULL
    
    # Turn 'LastName, FirstName' to 'FirstName LastName'
    name_matrix <- matrix(unlist(strsplit(nfl_tmp$player_name, ", ", fixed=TRUE)), 
                          nrow=nrow(nfl_tmp), byrow=TRUE)
    first_name <- name_matrix[,2]
    last_name <- name_matrix[,1]
    nfl_tmp$player_name <- paste(first_name, last_name, sep = " ")
    
    # Append to Master List
    historical_players <- rbind(historical_players,nfl_tmp)
    
    p <- p + 1
  }
}


### Active Players
current_players <- data.frame(player_id = as.character(),
                                 position = as.character(), 
                                 player_name= as.character(), 
                                 status = as.character(), 
                                 draft_year = as.numeric(),
                                 years_played = as.numeric())

# Loop through positions

pos_data <- data.frame(position = c("quarterback","runningback","widereceiver","tightend",
                                    "offensiveline","defensivelineman","linebacker",
                                    "defensiveback","kicker","punter"),
                       pages = c(2,4,6,3,8,6,5,8,1,1),
                       abbr_pos = c("QB","RB","WR","TE","OL","DL","LB","CB","K","P"))

p <- 1
for (i in (1:length(pos_data$position))) {
  for (j in (1:pos_data$pages[i])) {
    nfl_url <- paste0("http://www.nfl.com/players/search?category=position&filter=",
                      pos_data$position[i], 
                      "&playerType=current&conference=ALL&d-447263-p=", 
                      j)
    nfl_tmp <- tryCatch(readHTMLTable(nfl_url, 
                                      stringsAsFactors=FALSE, 
                                      header=FALSE)$result[,c(1,3,4)],
                        error= function(e) {
                          print(paste0("Error Scraping ",pos_data$abbr_pos[i],
                                       ", pg ",j," of ",pos_data$pages[i]),"...")
                        })
    names(nfl_tmp) <- c("position","player_name","status")
    
    # Go into HTML links to get player_id
    pge_links <- getHTMLLinks(nfl_url)
    nfl_tmp$player_id <- unique(gsub("[^0-9]","",
                                     pge_links[grep("/profile$", pge_links)]))
    
    # Turn 'LastName, FirstName' to 'FirstName LastName'
    name_matrix <- matrix(unlist(strsplit(nfl_tmp$player_name, ", ", fixed=TRUE)), 
                          nrow=nrow(nfl_tmp), byrow=TRUE)
    first_name <- name_matrix[,2]
    last_name <- name_matrix[,1]
    nfl_tmp$player_name <- paste(first_name, last_name, sep = " ")
    
    # Go into each player's page
    for (k in 1:length(nfl_tmp$player_name)){
      print(paste0("Scraping ",nfl_tmp$player_name[k],"'s profile..."))
      player_url <- paste0("http://www.nfl.com/player/",
                           gsub(" ","",nfl_tmp$player_name[k]),"/",
                           nfl_tmp$player_id[k],"/profile")
      
      
      player_html <- read_html(player_url)
      player_nodes <- html_nodes(player_html,"p:nth-child(6)")
      player_test <- html_text(player_nodes)
      nfl_tmp$years_played[k] <- as.numeric(ifelse(substr(player_test,13,
                                                   nchar(player_test)-1)=="Rookie",
                                                   0,
                                                   substr(substr(player_test,13,
                                                                 nchar(player_test)-1),
                                                          1,nchar(substr(player_test,
                                                            13,nchar(player_test)-10)))))
      nfl_tmp$draft_year[k] <- year(now())-nfl_tmp$years_played[k]
    }
      
      # Append to Master List
    current_players <- rbind(current_players,nfl_tmp)
    
    p <- p + 1
  }
}

# Combine historical & current lists
master_nfl_list <- rbind(historical_players, current_players)

# Eliminate Special characters & Jr/Sr/II
master_nfl_list$player_name <- gsub("'","",master_nfl_list$player_name)
master_nfl_list$player_name <- gsub("\\.","",master_nfl_list$player_name)
master_nfl_list$player_name <- gsub("-","",master_nfl_list$player_name)
master_nfl_list$player_name <- gsub(" IV","",master_nfl_list$player_name)
master_nfl_list$player_name <- gsub(" III","",master_nfl_list$player_name)
master_nfl_list$player_name <- gsub(" II","",master_nfl_list$player_name)
master_nfl_list$player_name <- gsub(" Jr","",master_nfl_list$player_name)
master_nfl_list$player_name <- gsub(" Sr","",master_nfl_list$player_name)

# Reorder columns
master_nfl_list <- master_nfl_list[,c(3,1,5,6,4,2)]
master_nfl_list$trim_name <- toupper(gsub(" ","",master_nfl_list$player_name))

# Export dataframe
write.csv(master_nfl_list, "master_nfl_player_list.csv", row.names = FALSE)
