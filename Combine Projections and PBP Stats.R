rm(list=ls())

library(dplyr)
library(ggplot2)
library(caret)
library(reshape2)

analysts  <- read.csv("analysts.csv")
madden    <- read.csv("madden_final.csv")
players   <- read.csv("master_nfl_player_list.csv")
play_data <- read.csv("play_data.csv")
scoring   <- read.csv("scoring.csv")
qb_proj   <- read.csv("qb_projections 2015-2017.csv")
rb_proj   <- read.csv("rb_projections 2015-2017.csv")
wr_proj   <- read.csv("wr_projections 2015-2017.csv")
te_proj   <- read.csv("te_projections 2015-2017.csv")
dst_proj  <- read.csv("dst_projections 2015-2017.csv")
k_proj    <- read.csv("k_projections 2015-2017.csv")
proj_combined <- read.csv("proj_combined.csv")


#### Calculate projections by analyst and year based on scoring table ####

# 2012 - 2014 Projections

proj <- as.data.frame(mapply("*",proj_combined[intersect(names(proj_combined),
                                                         names(scoring))],
                             scoring[intersect(names(proj_combined),
                                               names(scoring))]))

proj[is.na(proj)] <- 0
proj$pts <- rowSums(proj)
# proj$name <- proj_combined$name
# proj <- proj[,c("pts","name")]
# proj$pos <- proj_combined$pos
# proj$team <- proj_combined$team
# proj$season <- proj_combined$season
# proj$analyst_id <- proj_combined$analyst_id
# proj$name <- gsub(" Jr.","",proj$name)
# proj$name <- gsub('Chris "Beanie" Wells', "Beanie Wells", proj$name)
proj_combined$pts <- proj$pts

proj1 <- dcast(proj_combined, name + pos + season ~ analyst_id, 
                   fun.aggregate = sum, value.var = "pts")
proj1[is.na(proj1)] <- 0


# 2015 - 2017 Projections
proj_all <- plyr::rbind.fill(qb_proj, rb_proj, k_proj,
                    wr_proj, te_proj, dst_proj)

names(proj_all) <- c("player_id", "pass_att", "pass_comp", "pass_yds", 
                     "pass_tds", "pass_int", "pass_comp_pct", "rush_att", 
                     "rush_yds", "rush_tds", "fumbles", "pass_inc", 
                     "analyst_id", "name", "pos", "games", 
                     "sacks", "pass40","rush1st", "rush40", "rec1st", 
                     "return_tds", "two_pts", "pass300", "pass350", 
                     "pass400", "reg_tds", "season", "rec", "rec_yds", "rec_tds", 
                     "rec40", "return_yds", "rush100", "rush150", 
                     "rush200", "fg", "fg_att", "xp", "fg_miss", "fg0019", 
                     "fg2029", "fg3039", "fg4049","fg50", "fg0039", 
                     "rec100", "rec150", "rec200", "dst_int", "dst_fumblrec", 
                     "dst_sack", "dst_td", "dst_safety", "dst_pts_allow", 
                     "dst_blk", "dst_ret_td")

proj2 <- as.data.frame(mapply("*",proj_all[intersect(names(proj_all),
                                                     names(scoring))],
                              scoring[intersect(names(proj_all),
                                                names(scoring))]))
proj2[is.na(proj2)] <- 0
proj2$pts <- rowSums(proj2)
proj_all$pts <- proj2$pts

proj2 <- dcast(proj_all, name + pos + season  ~ analyst_id, 
              fun.aggregate = sum, value.var = "pts")
proj2[is.na(proj2)] <- 0

# Combine all years projections

final_proj <- plyr::rbind.fill(proj1, proj2)
final_proj <- final_proj[,c("name","pos","season","-1","3","4","5","7","9")]
names(final_proj) <- c("name","pos","season","analyst_.1", "analyst_3",
                       "analyst_4","analyst_5","analyst_7","analyst_9")

rm(dst_proj, k_proj, proj, proj_all, proj_combined,
   proj2, qb_proj, rb_proj, te_proj, wr_proj)

final_proj$name <- gsub("'","",final_proj$name)
final_proj$name <- gsub("\\.","",final_proj$name)
final_proj$name <- gsub("-","",final_proj$name)
final_proj$name <- gsub(" IV","",final_proj$name)
final_proj$name <- gsub(" III","",final_proj$name)
final_proj$name <- gsub(" II","",final_proj$name)
final_proj$name <- gsub(" Jr","",final_proj$name)
final_proj$name <- gsub(" Sr","",final_proj$name)
final_proj$name <- gsub("\\*","",final_proj$name)

final_proj$trim_name <- final_proj$name
final_proj$trim_name <- gsub(" ", "",final_proj$trim_name)
final_proj$trim_name <- toupper(final_proj$trim_name)

## Manually Clean Names
final_proj$trim_name[final_proj$trim_name=="BENWATSON"] <- "BENJAMINWATSON"
final_proj$trim_name[final_proj$trim_name=="MITCHTRUBISKY"] <- "MITCHELLTRUBISKY"
final_proj$trim_name[final_proj$trim_name=="WILLFULLERV"] <- "WILLFULLER"
final_proj$trim_name[final_proj$trim_name=="RYANGRIFFIN" & final_proj$pos=="TE"] <- "RYANGRIFFINTE"
final_proj$trim_name[final_proj$trim_name=="CHRISTHOMPSON" & final_proj$pos=="RB"] <- "CHRISTHOMPSONRB"
final_proj <- final_proj[!(final_proj$trim_name=="DAVIDJOHNSON" & final_proj$pos=="TE"),]


# Impute Missing Data with Average
final_proj$avg <- (final_proj$analyst_.1 + final_proj$analyst_4 + final_proj$analyst_5 + 
                        final_proj$analyst_7 + final_proj$analyst_9)/5
final_proj$analyst_3[is.na(final_proj$analyst_3)] <- final_proj$avg[is.na(final_proj$analyst_3)]
final_proj$avg <- NULL

write.csv(final_proj, "final_projections.csv", row.names = FALSE)




#### Play Data Metrics ####

# Calculate value of individual play for DVOA
downyards <- play_data %>% 
  group_by(playtype, down, ydstogo, season) %>%
  summarise(avgdownyds_voa = mean(yards_gained),
            sddownyds_voa = sd(yards_gained),
            avgpoints_voa = 6*mean(touchdown),
            prob1stdown_voa = sum(firstdown)/n(),
            probturn_voa = (sum(interceptionthrown)+sum(fumble))/n())
downyards$avgdownyds_voa <- ifelse(is.na(downyards$avgdownyds_voa),
                                   0,
                                   downyards$avgdownyds_voa)
downyards$sddownyds_voa <- ifelse(is.na(downyards$sddownyds_voa),
                                  0,
                                  downyards$sddownyds_voa)
downyards$avgpoints_voa <- ifelse(is.na(downyards$avgpoints_voa),
                                  0,
                                  downyards$avgpoints_voa)
downyards$prob1stdown_voa <- ifelse(is.na(downyards$prob1stdown_voa),
                                    0,
                                    downyards$prob1stdown_voa)
downyards$probturn_voa <- ifelse(is.na(downyards$probturn_voa),
                                 0,
                                 downyards$probturn_voa)

play_data <- merge(play_data,
                   downyards,
                   by = c("playtype","down","ydstogo","season"))

play_data$ydnorm_voa <- pnorm(play_data$yards_gained,
                          mean = play_data$avgdownyds_voa,
                          sd = play_data$sddownyds_voa)

play_data$yds_voa <- play_data$yards_gained - play_data$avgdownyds_voa

# Summarise by Def team and yards in buckets of 5
play_data$ydstogo5 <- ifelse(play_data$ydstogo < 5,
                             "0-5",
                             ifelse(play_data$ydstogo < 10,
                                    "5-10",
                                    ifelse(play_data$ydstogo < 15,
                                           "10-15",
                                           ifelse(play_data$ydstogo < 20,
                                                  "15-20","20+"))))

downteam <- play_data %>% 
  group_by(playtype, down, ydstogo5, season, defensiveteam) %>%
  summarise(avgdownyds_dvoa = mean(yards_gained),
            sddownyds_dvoa = sd(yards_gained),
            avgpoints_dvoa = 6*mean(touchdown),
            prob1stdown_dvoa = sum(firstdown)/n(),
            probturn_dvoa = (sum(interceptionthrown)+sum(fumble))/n())

downteam$avgdownyds_dvoa <- ifelse(is.na(downteam$avgdownyds_dvoa),
                                   0,
                                   downteam$avgdownyds_dvoa)
downteam$sddownyds_dvoa <- ifelse(is.na(downteam$sddownyds_dvoa),
                                  0,
                                  downteam$sddownyds_dvoa)
downteam$avgpoints_dvoa <- ifelse(is.na(downteam$avgpoints_dvoa),
                                  0,
                                  downteam$avgpoints_dvoa)
downteam$prob1stdown_dvoa <- ifelse(is.na(downteam$prob1stdown_dvoa),
                                    0,
                                    downteam$prob1stdown_dvoa)
downteam$probturn_dvoa <- ifelse(is.na(downteam$probturn_dvoa),
                                 0,
                                 downteam$probturn_dvoa)

play_data <- merge(play_data,
                   downteam,
                   by = c("playtype","down","ydstogo5","season","defensiveteam"))

play_data$ydnorm_dvoa <- pnorm(play_data$yards_gained,
                          mean = play_data$avgdownyds_dvoa,
                          sd = play_data$sddownyds_dvoa)

play_data$yds_dvoa <- play_data$yards_gained - play_data$avgdownyds_dvoa

### Pass Plays ###
qb_data <- play_data %>%
  group_by(season, passer_id) %>% 
  filter(spike == 0,
         playtype == "Pass") %>% 
  summarise(games = n_distinct(gameid),
            att = sum(passattempt),
            comp = sum(reception),
            yards = sum(yards_gained),
            avg_yards = mean(yards_gained),
            sd_yards = sd(yards_gained),
            yac = sum(yardsaftercatch),
            pass_length = sum(airyards),
            avg_pass_length = mean(airyards),
            sd_pass_length = sd(airyards),
            td = sum(touchdown),
            int = sum(interceptionthrown),
            fumbles = sum(fumble),
            avg_ydnorm_dvoa = mean(ydnorm_dvoa),
            avg_yds_dvoa = mean(yds_dvoa),
            sum_yds_dvoa = sum(yds_dvoa),
            avg_ydnorm_voa = mean(ydnorm_voa),
            avg_yds_voa = mean(yds_voa),
            sum_yds_voa = sum(yds_voa))

qb_data[is.na(qb_data)] <- 0
colnames(qb_data)[colnames(qb_data)=="passer_id"] <- "player_id"

# Make Data = Season + 1 to see if it predicts future years
qb_data$prev_season <- qb_data$season
qb_data$season <- qb_data$season + 1

# Merge to players to get IDs for Madden & projections
qb_data <- merge(qb_data,
                 players,
                 by = "player_id")

# Madden merge
names(madden) <- tolower(names(madden))
colnames(madden)[colnames(madden)=="year"] <- "season"
colnames(madden)[colnames(madden)=="player"] <- "name"
colnames(madden)[colnames(madden)=="playerid"] <- "proj_id"

qb_data <- merge(qb_data,
                 madden[madden$playerid != 0,],
                 by = c("proj_id","season"))
qb_data[,c("yahoo_id","cbs_id","mfl_Id","esbid","birthdate",
           "player_name","player_abbr","team","last","first")] <- NULL

# Projection merge


