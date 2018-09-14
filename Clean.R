rm(list=ls())

library(dplyr)
library(Amelia)

play_data <- read.csv("NFL Play by Play 2009-2017 (v4).csv",
                      stringsAsFactors= FALSE)

# Clean column names
names(play_data) <- tolower(names(play_data))
colnames(play_data)[colnames(play_data)=="yards.gained"] <- "yards_gained"
colnames(play_data)[colnames(play_data)=="challenge.replay"] <- "challenge_replay"
colnames(play_data)[colnames(play_data)=="accepted.penalty"] <- "accepted_penalty"
colnames(play_data)[colnames(play_data)=="ï..date"] <- "game_date"

# Clean structure of columns
table(play_data$hometeam,
      play_data$season)

play_data[play_data == "JAC"] <- "JAX"
play_data[play_data == "STL"] <- "LAR"
play_data[play_data == "LA"]  <- "LAR"
play_data[play_data == "SD"]  <- "LAC"
play_data$game_date <- as.Date(play_data$game_date,"%Y-%m-%d")

table(play_data$hometeam,
      play_data$season)

# Create data frame of teams
teams <- data.frame(teams = unique(play_data$hometeam))

### Create Data frame of unique players ###
# Passers
players <- data.frame(player_id = unique(play_data$passer_id))
players$player_id <- as.character(players$player_id)
players <- merge(x = players,
                 y = unique(play_data[,c("passer_id","passer")]),
                 by.x = "player_id",
                 by.y = "passer_id")
players <- players[!is.na(players$player_id),]
colnames(players)[colnames(players)=="passer"] <- "player"
players2 <- merge(x = filter(players,
                             player_id=="None"),
                  y = players,
                  by = "player")
players2$player_id.x <- NULL
colnames(players2)[colnames(players2)=="player_id.y"] <- "player_id"
players <- players2[players2$player_id != "None",]

tmp <- players %>% 
  group_by(player) %>% 
  summarise(id_count = n())
tmp <- tmp %>% 
  group_by(player) %>% 
  filter(id_count > 1)
tmp <- merge(x = tmp,
             y = players,
             by = "player")
tmp_count <- merge(x = play_data[,c("passer","passer_id","gameid")],
                   y = tmp,
                   by.x = c("passer","passer_id"),
                   by.y = c("player","player_id"))
tmp_count <- tmp_count %>% 
  group_by(passer,passer_id) %>% 
  summarise(id_count = n())
tmp_count2 <- tmp_count %>% 
  group_by(passer) %>% 
  summarise(id = max(id_count))
tmp_count <- merge(x = tmp_count,
                   y = tmp_count2,
                   by.x = c("passer","id_count"),
                   by.y = c("passer","id"))
colnames(tmp_count) <- c("player","id","player_id")
players <- players[!(players$player %in% tmp_count$player),]

players <- rbind(players, tmp_count[,c("player","player_id")])



# Rushers
players_tmp <- data.frame(player_id = unique(play_data$rusher_id))
players_tmp$player_id <- as.character(players_tmp$player_id)
players_tmp <- merge(x = players_tmp,
                 y = unique(play_data[,c("rusher_id","rusher")]),
                 by.x = "player_id",
                 by.y = "rusher_id")
players_tmp <- players_tmp[!is.na(players$player_id),]
colnames(players_tmp)[colnames(players_tmp)=="rusher"] <- "player"
players_tmp <- players_tmp[!(players_tmp$player_id %in% players$player_id),]
players_tmp <- players_tmp[!(players_tmp$player %in% players$player),]
players2 <- merge(x = filter(players_tmp,
                             player_id=="None"),
                  y = players_tmp,
                  by = "player")
players2$player_id.x <- NULL
colnames(players2)[colnames(players2)=="player_id.y"] <- "player_id"
players_tmp <- players2[players2$player_id != "None",]

tmp <- players_tmp %>% 
  group_by(player) %>% 
  summarise(id_count = n())
tmp <- tmp %>% 
  group_by(player) %>% 
  filter(id_count > 1)
tmp <- merge(x = tmp,
             y = players_tmp,
             by = "player")
tmp_count <- merge(x = play_data[,c("rusher","rusher_id","gameid")],
                   y = tmp,
                   by.x = c("rusher","rusher_id"),
                   by.y = c("player","player_id"))
tmp_count <- tmp_count %>% 
  group_by(rusher,rusher_id) %>% 
  summarise(id_count = n())
tmp_count2 <- tmp_count %>% 
  group_by(rusher) %>% 
  summarise(id = max(id_count))
tmp_count <- merge(x = tmp_count,
                   y = tmp_count2,
                   by.x = c("rusher","id_count"),
                   by.y = c("rusher","id"))
colnames(tmp_count) <- c("player","id","player_id")
players_tmp <- players_tmp[!(players_tmp$player %in% tmp_count$player),]
players_tmp <- rbind(players_tmp, tmp_count[,c("player","player_id")])

players <- rbind(players, players_tmp)


# Receivers
players_tmp <- data.frame(player_id = unique(play_data$receiver_id))
players_tmp$player_id <- as.character(players_tmp$player_id)
players_tmp <- merge(x = players_tmp,
                     y = unique(play_data[,c("receiver_id","receiver")]),
                     by.x = "player_id",
                     by.y = "receiver_id")
players_tmp <- players_tmp[!is.na(players$player_id),]
colnames(players_tmp)[colnames(players_tmp)=="receiver"] <- "player"
players_tmp <- players_tmp[!(players_tmp$player_id %in% players$player_id),]
players_tmp <- players_tmp[!(players_tmp$player %in% players$player),]
players2 <- merge(x = filter(players_tmp,
                             player_id=="None"),
                  y = players_tmp,
                  by = "player")
players2$player_id.x <- NULL
colnames(players2)[colnames(players2)=="player_id.y"] <- "player_id"
players_tmp <- players2[players2$player_id != "None",]

tmp <- players_tmp %>% 
  group_by(player) %>% 
  summarise(id_count = n())
tmp <- tmp %>% 
  group_by(player) %>% 
  filter(id_count > 1)
tmp <- merge(x = tmp,
             y = players_tmp,
             by = "player")
tmp_count <- merge(x = play_data[,c("receiver","receiver_id","gameid")],
                   y = tmp,
                   by.x = c("receiver","receiver_id"),
                   by.y = c("player","player_id"))
tmp_count <- tmp_count %>% 
  group_by(receiver,receiver_id) %>% 
  summarise(id_count = n())
tmp_count2 <- tmp_count %>% 
  group_by(receiver) %>% 
  summarise(id = max(id_count))
tmp_count <- merge(x = tmp_count,
                   y = tmp_count2,
                   by.x = c("receiver","id_count"),
                   by.y = c("receiver","id"))
colnames(tmp_count) <- c("player","id","player_id")
players_tmp <- players_tmp[!(players_tmp$player %in% tmp_count$player),]
players_tmp <- rbind(players_tmp, tmp_count[,c("player","player_id")])

players <- rbind(players, players_tmp)

rm(players_tmp,players2,tmp,tmp_count,tmp_count2)
testplayers <- unique(rbind(data.frame(player = unique(play_data$passer)),
                     data.frame(player = unique(play_data$rusher)),
                     data.frame(player = unique(play_data$receiver))))
testplayers <- merge(x = testplayers,
                     y = players,
                     by = "player",
                     all.x = TRUE)

player_count <- players %>% 
  group_by(player) %>% 
  summarise(nbr = n())

id_count <- players %>% 
  group_by(player_id) %>% 
  summarise(nbr = n())
id_count <- merge(x = id_count,
                  y = players,
                  by = "player_id")

### Pass Plays ###
pass_plays <- play_data[play_data$playtype=="Pass",]

pass_game_summary <- pass_plays %>%
  group_by(gameid, hometeam, awayteam, passer_id, season) %>% 
  summarise(att = sum(passattempt),
            comp = sum(reception),
            yards = sum(yards_gained),
            yac = sum(yardsaftercatch),
            pass_length = sum(airyards),
            td = sum(touchdown),
            int = sum(interceptionthrown))


