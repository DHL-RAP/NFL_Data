
library(jsonlite)
library(sqldf)
library(plyr)


## v2 Test
# Breaks out QB, Rb, WR, TE, K, DST
rm(nfl_players)
nfl_base_url <- "http://api.fantasy.nfl.com/v2/players/advanced?season="

for (i in seq(from=2009, to=2018, by=1)) {
  for (w in seq(from=1, to=17, by=1)) {
    for (j in seq(from=0, to=1000, by=1000)) {
      print(paste0("scraping ",i," Season, Week ",w," ..."))
      nfl_url <- paste0(nfl_base_url,i,"&week=",w,"&offset=",j,"&count=1000&format=json")
      scrape_nfl <- fromJSON(nfl_url)
      tryCatch(if (exists("nfl_players")==FALSE) {
        nfl_players <- scrape_nfl$QB[,c("id", "esbid", "gsisPlayerId","firstName",
                                        "lastName","teamAbbr","position")]
        nfl_players <- rbind.fill(nfl_players, 
                                  scrape_nfl$RB[,c("id", "esbid", "gsisPlayerId","firstName",
                                                   "lastName","teamAbbr","position")])
        nfl_players <- rbind.fill(nfl_players, 
                                  scrape_nfl$WR[,c("id", "esbid", "gsisPlayerId","firstName",
                                                   "lastName","teamAbbr","position")])
        nfl_players <- rbind.fill(nfl_players, 
                                  scrape_nfl$TE[,c("id", "esbid", "gsisPlayerId","firstName",
                                                   "lastName","teamAbbr","position")])
        nfl_players <- rbind.fill(nfl_players, 
                                  scrape_nfl$K[,c("id", "esbid", "gsisPlayerId","firstName",
                                                  "lastName","teamAbbr","position")])
        nfl_players <- rbind.fill(nfl_players, 
                                  scrape_nfl$DEF[,c("id", "esbid", "gsisPlayerId","firstName",
                                                    "lastName","teamAbbr","position")])
        nfl_players$season <- i
      } else {
        nfl_temp <- scrape_nfl$players
        nfl_temp <- rbind.fill(nfl_temp, 
                               scrape_nfl$RB[,c("id", "esbid", "gsisPlayerId","firstName",
                                                "lastName","teamAbbr","position")])
        nfl_temp <- rbind.fill(nfl_temp, 
                               scrape_nfl$WR[,c("id", "esbid", "gsisPlayerId","firstName",
                                                "lastName","teamAbbr","position")])
        nfl_temp <- rbind.fill(nfl_temp, 
                               scrape_nfl$TE[,c("id", "esbid", "gsisPlayerId","firstName",
                                                "lastName","teamAbbr","position")])
        nfl_temp <- rbind.fill(nfl_temp, 
                               scrape_nfl$K[,c("id", "esbid", "gsisPlayerId","firstName",
                                               "lastName","teamAbbr","position")])
        nfl_temp <- rbind.fill(nfl_temp, 
                               scrape_nfl$DEF[,c("id", "esbid", "gsisPlayerId","firstName",
                                                 "lastName","teamAbbr","position")])
        nfl_temp$season <- i
        nfl_players <- rbind.fill(nfl_players, nfl_temp)
      },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    }
  }
}


## Stats
# Breaks out All functions
rm(nfl_players)
nfl_base_url <- "http://api.fantasy.nfl.com/v1/players/stats?season="

for (i in seq(from=2009, to=2018, by=1)) {
    for (j in seq(from=0, to=3000, by=1000)) {
      print(paste0("scraping ",i," Season ..."))
      nfl_url <- paste0(nfl_base_url,i,"&offset=",j,"&count=1000")
      scrape_nfl <- fromJSON(nfl_url)
      tryCatch(if (exists("nfl_players")==FALSE) {
        nfl_players <- scrape_nfl$players[,c("id", "esbid", "gsisPlayerId","name",
                                             "teamAbbr","position")]
        
        nfl_players$season <- i
      } else {
        nfl_temp <- scrape_nfl$players[,c("id", "esbid", "gsisPlayerId","name",
                                          "teamAbbr","position")]
        nfl_temp <- rbind.fill(nfl_temp, 
                               scrape_nfl$players[,c("id", "esbid", "gsisPlayerId","name",
                                                "teamAbbr","position")])
        nfl_temp$season <- i
        nfl_players <- rbind.fill(nfl_players, nfl_temp)
      },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    }
}


# 2018 Players
nfl_base_url <- "http://api.fantasy.nfl.com/v1/players/researchinfo?season="

rm(nfl_players18)
for (i in seq(from=2009, to=2018, by=1)) {
  for (j in seq(from=0, to=5000, by=1000)) {
    print(paste0("scraping ",i," Season..."))
    nfl_url <- paste0(nfl_base_url,i,"&offset=",j,"&count=1000&format=json")
    scrape_nfl <- fromJSON(nfl_url)
    tryCatch(if (exists("nfl_players18")==FALSE) {
      nfl_players18 <- scrape_nfl$players
      nfl_players18$season <- i
    } else {
      nfl_temp <- scrape_nfl$players
      nfl_temp$season <- i
      nfl_players18 <- rbind(nfl_players18, nfl_temp)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }
}

nfl_players18$name <- paste0(nfl_players18$firstName,' ',nfl_players18$lastName)
nfl_players18 <- nfl_players18[,c("id","esbid","gsisPlayerId","name",
                                  "teamAbbr","position", "season")]

nfl_players <- rbind(nfl_players, nfl_players18)

nfl_players_tmp <- unique(nfl_players[,c("id","esbid","gsisPlayerId","name",
                                         "position")])
nfl_players_tmp <- sqldf("SELECT 
                         id, esbid, gsisPlayerId, name,
                         position, Max(season) as season
                         FROM
                         nfl_players
                         GROUP BY
                         id, esbid, gsisPlayerId, name,
                         position")

gsis <- unique(nfl_players[,c("id","gsisPlayerId")])

players <- merge(players,
                 gsis,
                 by.x = "player_id",
                 by.y = "id",
                 all.x = TRUE)

cleaned_gsis <- read.csv("players_to_clean.csv",header = TRUE)

cleaned_gsis$X.6 <- gsub("'","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub("\\.","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub("-","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub(" IV","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub(" III","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub(" II","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub(" Jr","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub(" Sr","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- gsub(" ","",cleaned_gsis$X.6)
cleaned_gsis$X.6 <- toupper(cleaned_gsis$X.6)


players <- merge(players,
                 cleaned_gsis[,c("X.6","X.2")],
                 by.x = "trim_name",
                 by.y = "X.6",
                 all.x = TRUE)
players$gsis_id <- ifelse(is.na(players$gsisPlayerId),
                          as.character(players$X.2),
                          players$gsisPlayerId)
players$X.2 <- NULL
players$gsisPlayerId <- NULL
