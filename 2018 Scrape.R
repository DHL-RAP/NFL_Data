# rm(list=ls())

#library(ffanalytics)
library(stringdist)
library(RCurl)
library(XML)
library(httr)
library(rvest)
library(tidyr)
library(dplyr)
library(reshape2)

scoring <- read.csv("scoring.csv")
analysts <- read.csv("analysts.csv")
players <- read.csv("master_nfl_player_list.csv")
season <- 2018

# Instr function
instr <- function(str1,str2,startpos=1,n=1){
  aa=unlist(strsplit(substring(str1,startpos),str2))
  if(length(aa) < n+1 ) return(0);
  return(sum(nchar(aa[1:n])) + startpos+(n-1)*nchar(str2) )
}  

## Scrape 2018 Projections

#### ESPN ####

df_tmp <- NULL
df_proj <- NULL
proj_base_url <- "http://games.espn.com/ffl/tools/projections?startIndex=" 
analyst <- 4
pg <- 1

for (i in seq(from=0,to = 1000, by=40)) {
  print(paste0("Scraping ",analysts$analystName[analysts$analystId == analyst], 
               ", Page ", pg, " ..."))
  proj_url <- paste0(proj_base_url,
                     i)
  scrape_proj <- GET(proj_url)
  df_tmp <- data.frame(readHTMLTable(rawToChar(scrape_proj$content),
                                     as.data.frame = TRUE,
                                     stringsAsFactors=FALSE)
                       $playertable_0)      
  df_tmp <- df_tmp[-1,]
  df_tmp[c("V1","V13")] <- NULL
  colnames(df_tmp) <- c("player","pass_comp","pass_yds","pass_tds","pass_int",
                        "rush_att","rush_yds","rush_tds","rec","rec_yds","rec_tds")
  df_tmp$season <- season
  df_tmp$analyst <- analyst
  df_tmp$two_pts <- 0
  df_tmp$fumbles <- 0
  df_tmp$return_tds <- 0

  for (r in 1:nrow(df_tmp)) {
    # Split Completions and Attempts
    slash_pos <- instr(df_tmp$pass_comp[r],"/")
    
    df_tmp$pass_att[r] <- substr(df_tmp$pass_comp[r],
                                 slash_pos+1,
                                 nchar(df_tmp$pass_comp[r]))
    df_tmp$pass_comp[r] <- substr(df_tmp$pass_comp[r],
                                 1,
                                 slash_pos-1)
    # Split Name from Team
    comma_pos <- instr(df_tmp$player[r],",")
    df_tmp$team[r] <- substr(df_tmp$player[r],
                             comma_pos+2,
                             comma_pos+4)
    df_tmp$pos[r] <- ifelse(nchar(gsub("\\s+$","",df_tmp$team[r]))==2,
                            substr(df_tmp$player[r],
                                   comma_pos+5,
                                   comma_pos+6),
                            substr(df_tmp$player[r],
                                   comma_pos+6,
                                   comma_pos+7))
    df_tmp$player[r] <- substr(df_tmp$player[r],
                               1,
                               comma_pos-1)  
    
  }
  tryCatch(if (exists("df_proj")==FALSE) {
    df_proj <- df_tmp
  } else {
    df_proj <- rbind(df_proj, df_tmp)
  },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  pg <- pg + 1
}

# Remove DST from df_proj
df_proj <- df_proj[df_proj$player!="",]


#### CBS SPORTS ####

proj_base_url <- "https://www.cbssports.com/fantasy/football/stats/sortable/points/" 
proj_base_url1 <- "/standard/projections/"
proj_base_url2 <- "/ytd?&start_row=" 
analyst <- -1
pg <- 1
positions <- c("QB","RB","WR","TE") 

for (p in 1:length(positions)) {
  for (i in seq(from=0,to = 300, by=50)) {
    
    print(paste0("Scraping ",analysts$analystName[analysts$analystId == analyst], 
                 ", Page ", pg, ", Position: ",positions[p]," ..."))
    proj_url <- paste0(proj_base_url,
                       positions[p],
                       proj_base_url1,
                       season,
                       proj_base_url2,
                       i)
    webpage <- read_html(proj_url)
    webpage_nodes <- html_nodes(webpage, 
                                '.row2+ .row1 td , .row2 td , .label td , .label+ .row1 td')
    webpage_nodes <- html_nodes(webpage, 
                                '.label td , .row2+ .row1 td , .row2 td , .label+ .row1 td')
    
    webpage_test <- html_text(webpage_nodes)

    # Scrape Result is One Giant Column - Parse into Columns
    df_tmp <- data.frame(player = character(),
                         pass_att = numeric(),
                         pass_comp = numeric(),
                         pass_yds = numeric(),
                         pass_tds = numeric(),
                         pass_int = numeric(),
                         pass_rating = numeric(),
                         rush_att = numeric(),
                         rush_yds = numeric(),
                         rush_avg = numeric(),
                         rush_tds = numeric(),
                         targets = numeric(),
                         rec = numeric(),
                         rec_yds = numeric(),
                         rec_avg = numeric(),
                         rec_tds = numeric(),
                         fumbles = numeric(),
                         pts = numeric())
    
    for (w in (ncol(df_tmp) + 1):length(webpage_test)) {
      r <- w %/% ncol(df_tmp)
      c <- w %% ncol(df_tmp)
      if (r > 0) {
        if (c == 0) {
          c <- ncol(df_tmp)
          r <- r - 1
        }
        if (nrow(df_tmp) < r) {
          df_tmp[r,1] <- "None"
          df_tmp[r,2:ncol(df_tmp)] <- 0
          df_tmp[,1] <- as.character(df_tmp[,1])
        }
        
        df_tmp[r,c] <- webpage_test[w]
      }
      
    }

    df_tmp$season <- season
    df_tmp$analyst <- analyst
    df_tmp$two_pts <- 0
    df_tmp$return_tds <- 0
    df_tmp$pos <- positions[p]
    df_tmp[,c("pass_rating","rush_avg","rec_avg")] <- NULL
    
    for (r in 1:nrow(df_tmp)) {
      # Split Name from Team
      slash_pos <- instr(df_tmp$player[r],",")
      
      df_tmp$team[r] <- substr(df_tmp$player[r],
                               slash_pos+2,
                               nchar(df_tmp$player[r]))
      df_tmp$player[r] <- substr(df_tmp$player[r],
                                 1,
                                 slash_pos-1)
      
    }
    tryCatch(if (exists("df_proj")==FALSE) {
      df_proj <- df_tmp
    } else {
      df_proj <- plyr::rbind.fill(df_proj, df_tmp)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    pg <- pg + 1
  }
}


#### NFL.com ####

proj_base_url <- "http://fantasy.nfl.com/research/projections?offset="
proj_base_url1 <- "&position=O&sort=projectedPts&statCategory=projectedStats&statSeason="
proj_base_url2 <- "&statType=seasonProjectedStats&statWeek=1"
analyst <- 5
pg <- 1

for (i in seq(from=0,to = 1000, by=25)) {
  
  print(paste0("Scraping ",analysts$analystName[analysts$analystId == analyst], 
               ", Page ", pg, " ..."))
  proj_url <- paste0(proj_base_url,
                     i,
                     proj_base_url1,
                     season,
                     proj_base_url2)
  webpage <- read_html(proj_url)
  webpage_nodes <- html_nodes(webpage, 
                              '.numeric , .playerNameAndInfo')
  
  webpage_test <- html_text(webpage_nodes)
  webpage_test <- webpage_test[-1]
  
  # Scrape Result is One Giant Column - Parse into Columns
  df_tmp <- data.frame(player = character(),
                       games = numeric(),
                       pass_yds = numeric(),
                       pass_tds = numeric(),
                       pass_int = numeric(),
                       rush_yds = numeric(),
                       rush_tds = numeric(),
                       rec_yds = numeric(),
                       rec_tds = numeric(),
                       fumbles_td = numeric(),
                       two_pts = numeric(),
                       fumbles = numeric(),
                       pts = numeric())
  
  for (w in (ncol(df_tmp) + 1):length(webpage_test)) {
    r <- w %/% ncol(df_tmp)
    c <- w %% ncol(df_tmp)
    if (r > 0) {
      if (c == 0) {
        c <- ncol(df_tmp)
        r <- r - 1
      }
      if (nrow(df_tmp) < r) {
        df_tmp[r,1] <- "None"
        df_tmp[r,2:ncol(df_tmp)] <- 0
        df_tmp[,1] <- as.character(df_tmp[,1])
      }
      
      df_tmp[r,c] <- webpage_test[w]
    }
    
  }
  
  df_tmp$season <- season
  df_tmp$analyst <- analyst
  for (r in 1:nrow(df_tmp)) {
    # Split Name from Team
    slash_pos <- instr(df_tmp$player[r],"-")
    
    df_tmp$team[r] <- substr(df_tmp$player[r],
                             slash_pos+2,
                             slash_pos+4)
    df_tmp$pos[r] <- substr(df_tmp$player[r],
                            slash_pos-3,
                            slash_pos-2)
    df_tmp$player[r] <- substr(df_tmp$player[r],
                               1,
                               slash_pos-5)
    
  }
  df_tmp[df_tmp=="-"] <- 0
  tryCatch(if (exists("df_proj")==FALSE) {
    df_proj <- df_tmp
  } else {
    df_proj <- plyr::rbind.fill(df_proj, df_tmp)
  },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  pg <- pg + 1
}


#### FFToday ####

proj_base_url <- "http://www.fftoday.com/rankings/playerproj.php?Season="
proj_base_url1 <- "&PosID="
proj_base_url2 <- "&LeagueID=1&order_by=FFPts&sort_order=DESC&cur_page="
analyst <- 7
pg <- 1
positions <- c(10, 20, 30, 40)

for (p in 1:length(positions)) {
  for (i in seq(from=0,to = 3, by=1)) {
    
    print(paste0("Scraping ",analysts$analystName[analysts$analystId == analyst], 
                 ", Page ", pg, " ..."))
    proj_url <- paste0(proj_base_url,
                       season,
                       proj_base_url1,
                       positions[p],
                       proj_base_url2,
                       i)
    scrape_proj <- GET(proj_url)
    df_tmp <- data.frame(readHTMLTable(rawToChar(scrape_proj$content),
                                       as.data.frame = TRUE,
                                       stringsAsFactors=FALSE)
                         [11])  
    if (positions[p] == 10) {
      names(df_tmp) <- c("change","player","team","bye","pass_comp","pass_att",
                         "pass_yds","pass_tds","pass_int","rush_att","rush_yds",
                         "rush_tds","pts")
      df_tmp$pass_yds <- gsub(",","",df_tmp$pass_yds)
    }    else if (positions[p]==20) {
      names(df_tmp) <- c("change","player","team","bye","rush_att","rush_yds","rush_tds",
                         "rec","rec_yds","rec_tds","pts")
      df_tmp$rush_yds <- gsub(",","",df_tmp$rush_yds)
      df_tmp$rec_yds <- gsub(",","",df_tmp$rec_yds)
    }    else if (positions[p]==30) {
      names(df_tmp) <- c("change","player","team","bye","rec","rec_yds","rec_tds",
                         "rush_att","rush_yds","rush_tds","pts")
      df_tmp$rush_yds <- gsub(",","",df_tmp$rush_yds)
      df_tmp$rec_yds <- gsub(",","",df_tmp$rec_yds)
    }    else {
      names(df_tmp) <- c("change","player","team","bye","rec","rec_yds","rec_tds","pts")
      df_tmp$rec_yds <- gsub(",","",df_tmp$rec_yds)
    }
    
    df_tmp$player <- substr(df_tmp$player,3,nchar(df_tmp$player))
    df_tmp <- df_tmp[-1,-1]
    
    if (nrow(df_tmp) >0 ){
      df_tmp$season <- season
      df_tmp$analyst <- analyst
      df_tmp$pos <- ifelse(positions[p]==10, "QB",
                           ifelse(positions[p]==20,"RB",
                                  ifelse(positions[p]==30,"WR","TE")))
    }
    tryCatch(if (exists("df_proj")==FALSE) {
      df_proj <- df_tmp
    } else {
      df_proj <- plyr::rbind.fill(df_proj, df_tmp)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    pg <- pg + 1
  }
}



#### Fantasy Pros ####

proj_base_url <- "https://www.fantasypros.com/nfl/projections/"
proj_base_url1 <- ".php?week=draft"
analyst <- 9
pg <- 1
positions <- c("qb", "rb", "wr", "te")

for (p in 1:length(positions)) {
  
  print(paste0("Scraping ",analysts$analystName[analysts$analystId == analyst], 
               ", Page ", pg, " ..."))
  proj_url <- paste0(proj_base_url,
                     positions[p],
                     proj_base_url1)
  scrape_proj <- GET(proj_url)
  df_tmp <- data.frame(readHTMLTable(rawToChar(scrape_proj$content),
                                     as.data.frame = TRUE,
                                     stringsAsFactors=FALSE)
                       $data)  
  if (p == 1) {
    names(df_tmp) <- c("player","pass_att","pass_comp","pass_yds","pass_tds","pass_int",
                       "rush_att","rush_yds","rush_tds","fumbles","pts")
    df_tmp$pass_yds <- gsub(",","",df_tmp$pass_yds)
  }    else if (p==2) {
    names(df_tmp) <- c("player","rush_att","rush_yds","rush_tds",
                       "rec","rec_yds","rec_tds","fumbles","pts")  
    df_tmp$rush_yds <- gsub(",","",df_tmp$rush_yds)
    df_tmp$rec_yds <- gsub(",","",df_tmp$rec_yds)
  }    else if (p==3) {
    names(df_tmp) <- c("player","rec","rec_yds","rec_tds","rush_att",
                       "rush_yds","rush_tds","fumbles","pts")
    df_tmp$rush_yds <- gsub(",","",df_tmp$rush_yds)
    df_tmp$rec_yds <- gsub(",","",df_tmp$rec_yds)
  }    else {
    names(df_tmp) <- c("player","rec","rec_yds","rec_tds","fumbles","pts")
    df_tmp$rec_yds <- gsub(",","",df_tmp$rec_yds)
  }
  
  
  df_tmp$team <- substr(df_tmp$player,nchar(df_tmp$player)-2,nchar(df_tmp$player))
  df_tmp$player <- substr(df_tmp$player, 1, nchar(df_tmp$player)-3)
  df_tmp$pos <- toupper(positions[p])
  df_tmp$analyst <- analyst
  df_tmp$season <- season
  df_tmp[df_tmp$player=="Tavon Austin" & df_tmp$pos=="RB",] <- NULL
  df_tmp[df_tmp$player=="JD McKissic" & df_tmp$pos=="RB",] <- NULL

  tryCatch(if (exists("df_proj")==FALSE) {
    df_proj <- df_tmp
  } else {
    df_proj <- plyr::rbind.fill(df_proj, df_tmp)
  },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
  pg <- pg + 1
}


#### Yahoo! Sports ####

proj_base_url <- "http://football.fantasysports.yahoo.com/f1/1150706/players?"
proj_base_url1 <- "status=ALL&cut_type=9&myteam=0&sort=PTS&sdir=1"
analyst <- 3
pg <- 1
positions <- c("QB", "RB", "WR", "TE")


for (p in 1:length(positions)) {
  for (i in seq(0, 150, by=25)) {
    print(paste0("Scraping ",analysts$analystName[analysts$analystId == analyst], 
                 ", Page ", pg, " ..."))
    proj_url <- paste0(proj_base_url,
                       proj_base_url1,
                       positions[p],
                       "&count=",
                       i,
                       "&pos=",
                       positions[p],
                       "&stat1=S_PS_",
                       season)
    
    scrape_proj <- GET(proj_url)
    df_tmp <- data.frame(readHTMLTable(rawToChar(scrape_proj$content),
                                       as.data.frame = TRUE,
                                       stringsAsFactors=FALSE)
                         [2]$'NULL')  
    
    if (ncol(df_tmp) == 24) {
      names(df_tmp) <- c("star","player_all","add","owner","games","bye","pts","ownedPct","proj",
                         "actual","pass_yds","pass_tds","pass_int","rush_att","rush_yds",
                         "rush_tds","targets","rec","rec_yds","rec_tds","return_tds","two_pts",
                         "fumbles","missing1")
      df_tmp <- df_tmp[,c(2,11:23)]
    } else {
      names(df_tmp) <- c("star","player_all","add","forecast","owner","games","bye","pts",
                         "ownedPct","proj","actual","pass_yds","pass_tds","pass_int","rush_att",
                         "rush_yds","rush_tds","targets","rec","rec_yds","rec_tds","return_tds",
                         "two_pts","fumbles","missing1")
      df_tmp <- df_tmp[,c(2,12:24)]
    }
      

    for (r in 1:nrow(df_tmp)) {
      dash_pos <- instr(df_tmp$player[r],"-",1)
      df_tmp$player[r] <- ifelse(substr(df_tmp$player_all[r],1,2)=="Pl",
                                 substr(df_tmp$player_all[r],13,dash_pos-5),
                                 substr(df_tmp$player_all[r],22,dash_pos-5))
      df_tmp$team[r] <- substr(df_tmp$player_all[r],dash_pos-4,dash_pos-2)
    }

    df_tmp$pos <- toupper(positions[p])
    df_tmp$analyst <- analyst
    df_tmp$season <- season
    df_tmp[,c("player_all","star","missing_1")] <- NULL
    df_tmp[df_tmp$player=="Tavon Austin" & df_tmp$pos=="RB",] <- NULL
    df_tmp[df_tmp$player=="Jaylen Samuels" & df_tmp$pos=="TE",] <- NULL
    
    
    tryCatch(if (exists("df_proj")==FALSE) {
      df_proj <- df_tmp
    } else {
      df_proj <- plyr::rbind.fill(df_proj, df_tmp)
    },error=function(e){cat("ERROR:",conditionMessage(e),"\n")})
    pg <- pg + 1
  }
}

#### Clean Projections ####

## Clean Data Frame
df_proj <- df_proj[df_proj$pos %in% c("QB","RB","WR","TE","DST","K"),]
df_proj <- df_proj[!(is.na(df_proj$player)),]

for (i in 1:ncol(df_proj)) {
  if (!(names(df_proj)[i] %in% c("pos","team","player"))) {
    df_proj[,i] <- as.numeric(as.character(df_proj[,i]))
  }
}

df_proj$pos <- as.factor(df_proj$pos)
df_proj$team <- toupper(df_proj$team)
df_proj$team <- gsub(" ","",df_proj$team)
df_proj$team <- gsub("\\s+$","",df_proj$team)
df_proj$team[df_proj$team=="JAC"] <- "JAX"
df_proj$team[df_proj$team=="LA"] <- "LAR"
df_proj$team[df_proj$team=="WSH"] <- "WAS"

## Clean Names
df_proj$player <- gsub("'","",df_proj$player)
df_proj$player <- gsub("\\.","",df_proj$player)
df_proj$player <- gsub("-","",df_proj$player)
df_proj$player <- gsub(" IV","",df_proj$player)
df_proj$player <- gsub(" III","",df_proj$player)
df_proj$player <- gsub(" II","",df_proj$player)
df_proj$player <- gsub(" Jr","",df_proj$player)
df_proj$player <- gsub(" Sr","",df_proj$player)
df_proj$player <- gsub("\\*","",df_proj$player)

df_proj$trim_name <- df_proj$player
df_proj$trim_name <- gsub(" ", "",df_proj$trim_name)
df_proj$trim_name <- toupper(df_proj$trim_name)

## Manually Clean Names
df_proj$trim_name[df_proj$trim_name=="BENWATSON"] <- "BENJAMINWATSON"
df_proj$trim_name[df_proj$trim_name=="MITCHTRUBISKY"] <- "MITCHELLTRUBISKY"
df_proj$trim_name[df_proj$trim_name=="WILLFULLERV"] <- "WILLFULLER"
df_proj$trim_name[df_proj$trim_name=="RYANGRIFFIN" & df_proj$pos=="TE"] <- "RYANGRIFFINTE"
df_proj$trim_name[df_proj$trim_name=="CHRISTHOMPSON" & df_proj$pos=="RB"] <- "CHRISTHOMPSONRB"
df_proj <- df_proj[!(df_proj$trim_name=="DAVIDJOHNSON" & df_proj$pos=="TE"),]
df_proj <- df_proj[!(df_proj$trim_name=="TAVONAUSTIN" & df_proj$pos=="RB" & 
                       df_proj$analyst==9),]
df_proj <- df_proj[!(df_proj$trim_name=="TAVONAUSTIN" & df_proj$pos=="RB" & 
                       df_proj$analyst==3),]
df_proj <- df_proj[!(df_proj$trim_name=="JDMCKISSIC" & df_proj$pos=="RB" & 
                       df_proj$analyst==9),]
df_proj <- df_proj[!(df_proj$trim_name=="JAYLENSAMUELS" & df_proj$pos=="RB" & 
                       df_proj$analyst==3),]

player_dups <- df_proj %>% group_by(trim_name) %>% summarise(id_count = n())
df_proj <- unique(df_proj)

## Add IDs

df_proj <- merge(df_proj,
                 players[players$last_year>2010,
                         c("trim_name","player_id","draft_year","gsis_id","fantasy_pos")],
                 by.x = c("trim_name","pos"),
                 by.y = c("trim_name","fantasy_pos"),
                 all.x = TRUE)
df_proj$player_id[df_proj$player_id %in% c(2508048)] <- -1
df_proj <- df_proj[df_proj$player_id != -1,]
df_proj <- df_proj[!(is.na(df_proj$player)),]

# df_proj2 <- df_proj2[is.na(df_proj2$player_id),]

write.csv(df_proj, "2018_raw_projections_full.csv", row.names = FALSE)


#### Impute and summarize for modeling ####

## Impute missing Values
# QB
qb_stats <- df_proj %>% 
  filter(pass_att>0) %>% 
  group_by(player_id) %>% 
  summarise(ypa = sum(pass_yds)/sum(pass_att),
            ypc = sum(pass_yds)/sum(pass_comp))
  
df_proj <- merge(df_proj,
                  qb_stats,
                  by = "player_id",
                  all.x = TRUE)
df_proj$pass_comp[is.na(df_proj$pass_comp)] <- 0
df_proj$pass_att[is.na(df_proj$pass_att)] <- 0
df_proj$pass_yds[is.na(df_proj$pass_yds)] <- 0
df_proj$ypa[is.na(df_proj$ypa)] <- 0
df_proj$ypc[is.na(df_proj$ypc)] <- 0
df_proj$pass_comp <- ifelse(df_proj$ypc==0,0,
                              ifelse(df_proj$pass_comp>0,df_proj$pass_comp,
                                     df_proj$pass_yds/df_proj$ypc))
df_proj$pass_att <- ifelse(df_proj$ypa==0,0,
                             ifelse(df_proj$pass_att>0,df_proj$pass_att,
                                    df_proj$pass_yds/df_proj$ypa))

# RB/WR/TE
rb_stats <- df_proj %>% 
  filter(rush_att>0) %>% 
  group_by(player_id) %>% 
  summarise(yprush = sum(rush_yds)/sum(rush_att))

df_proj <- merge(df_proj,
                 rb_stats,
                 by = "player_id",
                 all.x = TRUE)
df_proj$rush_att[is.na(df_proj$rush_att)] <- 0
df_proj$rush_yds[is.na(df_proj$rush_yds)] <- 0
df_proj$yprush[is.na(df_proj$yprush)] <- 0
df_proj$rush_att <- ifelse(df_proj$yprush==0,0,
                            ifelse(df_proj$rush_att>0,df_proj$rush_att,
                                   df_proj$rush_yds/df_proj$yprush))

wr_stats <- df_proj %>% 
  filter(rec>0) %>% 
  group_by(player_id) %>% 
  summarise(yprec = sum(rec_yds)/sum(rec),
            catchpertarget = sum(rec)/sum(targets))

df_proj <- merge(df_proj,
                 wr_stats,
                 by = "player_id",
                 all.x = TRUE)
df_proj$rec[is.na(df_proj$rec)] <- 0
df_proj$rec_yds[is.na(df_proj$rec_yds)] <- 0
df_proj$yprec[is.na(df_proj$yprec)] <- 0
df_proj$catchpertarget[is.na(df_proj$catchpertarget)] <- 0

df_proj$rec <- ifelse(df_proj$yprec==0,0,
                           ifelse(df_proj$rec>0,df_proj$rec,
                                  df_proj$rec_yds/df_proj$yprec))
df_proj$targets <- ifelse(df_proj$catchpertarget==0,0,
                          ifelse(df_proj$targets>0,df_proj$targets,
                                 df_proj$rec/df_proj$catchpertarget))

# Misc - Return TDs and Fumbles
ret_stats <- df_proj %>% 
  filter(return_tds>0) %>% 
  group_by(player_id) %>% 
  summarise(return_tds_avg = mean(return_tds))

df_proj <- merge(df_proj,
                 ret_stats,
                 by = "player_id",
                 all.x = TRUE)
df_proj$return_tds[is.na(df_proj$return_tds)] <- 0
df_proj$return_tds <- ifelse(df_proj$return_tds_avg==0,0,
                          ifelse(df_proj$return_tds>0,df_proj$return_tds,
                                 df_proj$return_tds_avg))

fmbl_stats <- df_proj %>% 
  filter(fumbles>0) %>% 
  group_by(player_id) %>% 
  summarise(fmbl_avg = mean(fumbles))

df_proj <- merge(df_proj,
                 fmbl_stats,
                 by = "player_id",
                 all.x = TRUE)
df_proj$fumbles[is.na(df_proj$fumbles)] <- 0
df_proj$fumbles <- ifelse(df_proj$fmbl_avg==0,0,
                             ifelse(df_proj$fumbles>0,df_proj$fumbles,
                                    df_proj$fmbl_avg))


# Calculate Points based on current league
pts <- as.data.frame(mapply("*",df_proj[intersect(names(df_proj),
                                                         names(scoring))],
                            scoring[intersect(names(df_proj),
                                              names(scoring))]))
pts[is.na(pts)] <- 0
pts$pts <- rowSums(pts)
df_proj$pts <- pts$pts
df_proj[is.na(df_proj)] <- 0

write.csv(df_proj, "2018_imputed_projections_full.csv", row.names = FALSE)

# Summarize & Export

proj_2018 <- dcast(df_proj, player_id + pos + season + team ~ analyst, 
                           fun.aggregate = sum, value.var = "pts")
proj_2018[is.na(proj_2018)] <- 0
names(proj_2018) <- c("player_id","pos","season","team","analyst_.1","analyst_3",
                      "analyst_4","analyst_5","analyst_7","analyst_9")

proj_2018 <- proj_2018 %>% 
  group_by(player_id, pos, season, team) %>% 
  summarise(analyst_.1 = sum(analyst_.1),
            analyst_3 = sum(analyst_3),
            analyst_4 = sum(analyst_4),
            analyst_5 = sum(analyst_5),
            analyst_7 = sum(analyst_7),
            analyst_9 = sum(analyst_9))

proj_2018 <- merge(proj_2018,
                   players[,c("player_id","player_name")],
                   by = "player_id")
proj_2018 <- proj_2018[,c(11,1:10)]
proj_2018 <- unique(proj_2018)
proj_2018$proj_count <- ifelse(proj_2018$analyst_.1>0,1,0) + 
  ifelse(proj_2018$analyst_3>0,1,0)  + ifelse(proj_2018$analyst_4>0,1,0) + 
  ifelse(proj_2018$analyst_5>0,1,0)  + ifelse(proj_2018$analyst_7>0,1,0) + 
  ifelse(proj_2018$analyst_9>0,1,0)
proj_2018$avg_proj <- ifelse(proj_2018$proj_count == 0,0,
                             (proj_2018$analyst_.1 + proj_2018$analyst_3 +
                                proj_2018$analyst_4 + proj_2018$analyst_5 +
                                proj_2018$analyst_7 + proj_2018$analyst_9)/proj_2018$proj_count)



write.csv(proj_2018, "2018_aggregate_projections_full.csv", row.names = FALSE)
