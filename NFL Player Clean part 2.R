library(sqldf)

players2 <- sqldf("SELECT 
                  player_id,
                  trim_name,
                  player_name
                  position,
                  status,
                  draft_year,
                  max(years_played) as years_played,
                  gsis_id,
                  last_year
                  
                  FROM players
                  
                  GROUP BY
                  player_id, trim_name, position, status, player_name,
                  draft_year, gsis_id, last_year")

players2 <- unique(players)
players2 <- aggregate(position ~ player_id , data = players2, paste, collapse = ",")

players2 <- merge(players2, players,
                  by = "player_id")
players2$position <- players2$position.x
players2$position.x <- NULL
players2$position.y <- NULL
players2 <- unique(players2)

players_count <- players2 %>% 
  group_by(player_id) %>% 
  summarise(count = n())
players_count <- players_count[players_count$count > 1,]

players2 <- sqldf("SELECT 
                  player_id,
                  trim_name,
                  player_name,
                  position,
                  status,
                  min(draft_year) as draft_year,
                  max(years_played) as years_played,
                  gsis_id
                  
                  FROM players2
                  
                  GROUP BY
                  player_id, player_name, trim_name, position, status,
                  gsis_id")

players2 <- players2[-c(334,1918,1933,6803,12452,12457,12485, 12614,12518,12554,12652,12719),]
players2$trim_name[players2$player_id=="2500439"] <- "SANTANADOTSON"
players2$player_name[players2$player_id=="2500439"] <- "Santana Dotson"
players2$trim_name[players2$player_id=="2512759"] <- "STEVEDELONG"
players2$player_name[players2$player_id=="2512759"] <- "Steve Delong"
players2$trim_name[players2$player_id=="2506115"] <- "DARNELLDOCKETT"
players2$player_name[players2$player_id=="2506115"] <- "Darnell Dockett"
players2$trim_name[players2$player_id=="496778"] <- "BRANDONDEADERICK"
players2$player_name[players2$player_id=="496778"] <- "Brandon Deaderick"
players2$trim_name[players2$player_id=="218"] <- "GLENNDORSEY"
players2$player_name[players2$player_id=="218"] <- "Glenn Dorsey"
players2$trim_name[players2$player_id=="2504892"] <- "CHRISDEMAREE"
players2$player_name[players2$player_id=="2504892"] <- "Chris Demaree"

players <- players2

write.csv(players, "master_nfl_player_list.csv",row.names = FALSE)

qb_match <- grep("^QB.*",players2$position, perl=T)
players2$fantasy_pos <- ifelse(grepl("QB",players2$position, perl=T),"QB",
                        ifelse(grepl("RB",players2$position, perl = T),"RB",
                        ifelse(grepl("WR",players2$position, perl = T),"WR",
                        ifelse(grepl("TE",players2$position, perl = T), "TE",
                        ifelse(grepl("FB", players2$position, perl = T), "RB",
                               players2$position))))