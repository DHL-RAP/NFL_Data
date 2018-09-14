rm(list=ls())

library(dplyr)
library(caret)
library(ggplot2)
library(randomForest)
library(reshape2)
library(ggthemes)
library(sqldf)
library(mclust)

set.seed(1217)

madden <- read.csv("madden_final.csv", stringsAsFactors = FALSE)
players <- read.csv("master_nfl_player_list.csv", stringsAsFactors = FALSE)
projections <- read.csv("final_projections.csv", stringsAsFactors = FALSE)
pbp_stats <- read.csv("pbp_stats.csv", stringsAsFactors = FALSE)
proj_2018 <- read.csv("2018_aggregate_projections_full.csv", stringsAsFactors = FALSE)
adp <- read.csv("2018_adp.csv", stringsAsFactors = FALSE)
scoring <- read.csv("scoring.csv")

# Remove all historical players who didn't play after 2008
players$last_year <- players$draft_year + players$years_played - 1
players <- players[players$last_year>2008,]

# Projections + player merge
final_tbl <- merge(projections,
                   players[,c("player_id","trim_name","position","gsis_id","draft_year")],
                   by.x = c("trim_name","pos"),
                   by.y = c("trim_name","position"))
final_tbl$experience <- final_tbl$season - final_tbl$draft_year
final_tbl$experience[final_tbl$experience <0] <- 0
final_tbl$draft_year <- NULL

# Final tbl + madden merge
final_tbl <- merge(final_tbl,
                   madden[,c(3,10:67)],
                   by = c("player_id","season"))

# Final tbl + PBP merge
final_tbl <- merge(final_tbl,
                   pbp_stats,
                   by = c("gsis_id","season"),
                   all.x = TRUE)
final_tbl <- merge(final_tbl,
                   pbp_stats[,c("gsis_id","prev_season","prev_yr_pts")],
                   by.x = c("gsis_id","season"),
                   by.y = c("gsis_id","prev_season"),
                   all.x = TRUE)
colnames(final_tbl)[names(final_tbl)=="prev_yr_pts.x"] <- "prev_yr_pts"
colnames(final_tbl)[names(final_tbl)=="prev_yr_pts.y"] <- "actual_pts"
final_tbl <- unique(final_tbl)


# Add a few factors based on analyst projections
final_tbl$ppg <- ifelse(final_tbl$games == 0,0,final_tbl$prev_yr_pts/final_tbl$games)
final_tbl$proj_count <- ifelse(final_tbl$analyst_.1>0,1,0) + 
  ifelse(final_tbl$analyst_3>0,1,0)  +
  ifelse(final_tbl$analyst_4>0,1,0)  + 
  ifelse(final_tbl$analyst_5>0,1,0)  + 
  ifelse(final_tbl$analyst_7>0,1,0)  + 
  ifelse(final_tbl$analyst_9>0,1,0)
final_tbl$avg_proj <- ifelse(final_tbl$proj_count == 0,0,
                             (final_tbl$analyst_.1 + 
                                final_tbl$analyst_3 +
                                final_tbl$analyst_4 + 
                                final_tbl$analyst_5 +
                                final_tbl$analyst_7 + 
                                final_tbl$analyst_9)/final_tbl$proj_count)

# Set analyst projections to 0 to impute later
final_tbl$analyst_.1[final_tbl$analyst_.1==0 & 
                       final_tbl$avg_proj>20] <- final_tbl$avg_proj[final_tbl$analyst_.1==0 & 
                                                                      final_tbl$avg_proj>20]
final_tbl$analyst_3[final_tbl$analyst_3==0 & 
                      final_tbl$avg_proj>20] <- final_tbl$avg_proj[final_tbl$analyst_3==0 & 
                                                                     final_tbl$avg_proj>20]
final_tbl$analyst_4[final_tbl$analyst_4==0 & 
                      final_tbl$avg_proj>20] <- final_tbl$avg_proj[final_tbl$analyst_4==0 & 
                                                                     final_tbl$avg_proj>20]
final_tbl$analyst_5[final_tbl$analyst_5==0 & 
                      final_tbl$avg_proj>20] <- final_tbl$avg_proj[final_tbl$analyst_5==0 & 
                                                                     final_tbl$avg_proj>20]
final_tbl$analyst_7[final_tbl$analyst_7==0 & 
                      final_tbl$avg_proj>20] <- final_tbl$avg_proj[final_tbl$analyst_7==0 & 
                                                                     final_tbl$avg_proj>20]
final_tbl$analyst_9[final_tbl$analyst_9==0 & 
                      final_tbl$avg_proj>20] <- final_tbl$avg_proj[final_tbl$analyst_9==0 & 
                                                                     final_tbl$avg_proj>20]

## 2018 Table
# Projections + player merge
final_2018 <- merge(proj_2018,
                   players[,c("player_id","trim_name","position","gsis_id","draft_year")],
                   by = "player_id")
final_2018$experience <- final_2018$season - final_2018$draft_year
final_2018$experience[final_2018$experience <0] <- 0
final_2018$draft_year <- NULL

# Final tbl + madden merge
final_2018 <- merge(final_2018,
                   madden[,c(3,10:67)],
                   by = c("player_id","season"))

# Final tbl + PBP merge
final_2018 <- merge(final_2018,
                   pbp_stats,
                   by = c("gsis_id","season"),
                   all.x = TRUE)
final_2018 <- merge(final_2018,
                   pbp_stats[,c("gsis_id","prev_season","prev_yr_pts")],
                   by.x = c("gsis_id","season"),
                   by.y = c("gsis_id","prev_season"),
                   all.x = TRUE)
colnames(final_2018)[names(final_2018)=="prev_yr_pts.x"] <- "prev_yr_pts"
colnames(final_2018)[names(final_2018)=="prev_yr_pts.y"] <- "actual_pts"
final_2018 <- unique(final_2018)


# Add a few factors based on analyst projections
final_2018$ppg <- ifelse(final_2018$games == 0,0,final_2018$prev_yr_pts/final_2018$games)
final_2018$proj_count <- ifelse(final_2018$analyst_.1>0,1,0) + 
  ifelse(final_2018$analyst_3>0,1,0)  +
  ifelse(final_2018$analyst_4>0,1,0)  + 
  ifelse(final_2018$analyst_5>0,1,0)  + 
  ifelse(final_2018$analyst_7>0,1,0)  + 
  ifelse(final_2018$analyst_9>0,1,0)
final_2018$avg_proj <- ifelse(final_2018$proj_count == 0,0,
                             (final_2018$analyst_.1 + 
                                final_2018$analyst_3 +
                                final_2018$analyst_4 + 
                                final_2018$analyst_5 +
                                final_2018$analyst_7 + 
                                final_2018$analyst_9)/final_2018$proj_count)

# Set analyst projections to 0 to impute later
final_2018$analyst_.1[final_2018$analyst_.1==0 & 
                       final_2018$avg_proj>20] <- final_2018$avg_proj[final_2018$analyst_.1==0 & 
                                                                      final_2018$avg_proj>20]
final_2018$analyst_3[final_2018$analyst_3==0 & 
                      final_2018$avg_proj>20] <- final_2018$avg_proj[final_2018$analyst_3==0 & 
                                                                     final_2018$avg_proj>20]
final_2018$analyst_4[final_2018$analyst_4==0 & 
                      final_2018$avg_proj>20] <- final_2018$avg_proj[final_2018$analyst_4==0 & 
                                                                     final_2018$avg_proj>20]
final_2018$analyst_5[final_2018$analyst_5==0 & 
                      final_2018$avg_proj>20] <- final_2018$avg_proj[final_2018$analyst_5==0 & 
                                                                     final_2018$avg_proj>20]
final_2018$analyst_7[final_2018$analyst_7==0 & 
                      final_2018$avg_proj>20] <- final_2018$avg_proj[final_2018$analyst_7==0 & 
                                                                     final_2018$avg_proj>20]
final_2018$analyst_9[final_2018$analyst_9==0 & 
                      final_2018$avg_proj>20] <- final_2018$avg_proj[final_2018$analyst_9==0 & 
                                                                     final_2018$avg_proj>20]



#### CREATE MODEL ####

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     savePred = T)


#### QB Experienced ####
qb_tbl <- final_tbl[,!(names(final_tbl) %in% c("gsis_id","player_id","trim_name","name",
                                               "season","prev_season","catch","carrying",
                                               "trucking","stiff_arm","spin_move",
                                               "juke_move","route","catch_in_traffic",
                                               "spec._catch","release","jumping","tackle",
                                               "hit_power","power_moves","finesse_moves",
                                               "block_shedding","pursuit","play_recognition",
                                               "man_coverage","zone_coverage","press",
                                               "kick_power","kick_accuracy","impact_blocking",
                                               "run_block","pass_block","kick_return",
                                               "lead_block","run_block_power",
                                               "pass_block_power","run_block_finesse",
                                               "pass_block_finesse","short_route_runing",
                                               "medium_route_running","deep_route_running",
                                               "rec","rec_yds","avg_rec_yds","sd_rec_yds",
                                               "avg_rec_ydnorm_dvoa","avg_rec_yds_dvoa",
                                               "sum_rec_yds_dvoa","avg_rec_ydnorm_voa",
                                               "avg_rec_yds_voa","sum_rec_yds_voa","rec_tds"
))]
qb_tbl$res <- abs(qb_tbl$actual_pts - qb_tbl$avg_proj)
qb_tbl <- qb_tbl[qb_tbl$res < 100 & qb_tbl$experience > 0,]
qb_tbl$res <- NULL
qb_tbl[is.na(qb_tbl)] <- 0
qb_tbl <- qb_tbl[qb_tbl$pos=="QB",]
qb_tbl$pos <- NULL
train <- qb_tbl

qb_2018 <- final_2018[final_2018$pos=="QB" & final_2018$experience>0,]
qb_2018[is.na(qb_2018)] <- 0

# Random Forest
mod_rf_qbexp <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")

qb_2018$prediction <- predict(mod_rf_qbexp, qb_2018)


#### QB Rookies ####

# Remove pbp factors
qb_tbl_rook <- final_tbl[,!(names(final_tbl) %in% c("gsis_id","player_id","trim_name","name",
                                                    "season","prev_season","catch","carrying",
                                                    "trucking","stiff_arm","spin_move",
                                                    "juke_move","route","catch_in_traffic",
                                                    "spec._catch","release","jumping","tackle",
                                                    "hit_power","power_moves","finesse_moves",
                                                    "block_shedding","pursuit","play_recognition",
                                                    "man_coverage","zone_coverage","press",
                                                    "kick_power","kick_accuracy","impact_blocking",
                                                    "run_block","pass_block","kick_return",
                                                    "lead_block","run_block_power",
                                                    "pass_block_power","run_block_finesse",
                                                    "pass_block_finesse","short_route_runing",
                                                    "medium_route_running","deep_route_running",
                                                    "rec","rec_yds","avg_rec_yds","sd_rec_yds",
                                                    "avg_rec_ydnorm_dvoa","avg_rec_yds_dvoa",
                                                    "sum_rec_yds_dvoa","avg_rec_ydnorm_voa",
                                                    "avg_rec_yds_voa","sum_rec_yds_voa","rec_tds"
))]
qb_tbl_c <- qb_tbl[,grepl("voa", names(qb_tbl))]
qb_tbl_rook <- qb_tbl_rook[,!(names(qb_tbl_rook) %in% names(qb_tbl_c))]
qb_tbl_rook <- qb_tbl_rook[,!(names(qb_tbl_rook) 
                              %in% c("games","pass_att","pass_comp","pass_yds","avg_pass_yds",
                                     "sd_pass_yds","yac","pass_length","avg_pass_length",
                                     "sd_pass_length","pass_tds","pass_int","fumbles",
                                     "rush_att","rush_yds","avg_rush_yds","sd_rush_yds",
                                     "rush_tds","prev_yr_pts","ppg"))]
qb_tbl_c <- NULL

qb_tbl_rook$res <- abs(qb_tbl_rook$actual_pts - qb_tbl_rook$avg_proj)
qb_tbl_rook <- qb_tbl_rook[qb_tbl_rook$res < 100 & qb_tbl_rook$experience==0,]
qb_tbl_rook$res <- NULL
qb_tbl_rook[is.na(qb_tbl_rook)] <- 0
qb_tbl_rook <- qb_tbl_rook[qb_tbl_rook$pos=="QB",]
qb_tbl_rook$pos <- NULL
train <- qb_tbl_rook

qb_2018_rook <- final_2018[final_2018$pos=="QB" & final_2018$experience == 0,]
qb_2018_rook[is.na(qb_2018_rook)] <- 0

# Random Forest
mod_rf_qbrook <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")

qb_2018_rook$prediction <- predict(mod_rf_qbrook, qb_2018_rook)



#### RB All ####
rb_tbl <- final_tbl[final_tbl$pos == "RB",]
rb_tbl <- rb_tbl[,!(names(rb_tbl) %in% c("gsis_id","player_id","trim_name","name",
                                         "season","prev_season","power_moves","
                                         finesse_moves",
                                         "block_shedding","pursuit","play_recognition",
                                         "man_coverage","zone_coverage","press",
                                         "kick_power","kick_accuracy","impact_blocking",
                                         "run_block","pass_block","kick_return",
                                         "lead_block","run_block_power",
                                         "pass_block_power","run_block_finesse",
                                         "pass_block_finesse"))]

# Remove Anything related to Passing
rb_tbl_c <- rb_tbl[,grepl("pass", names(rb_tbl))]
rb_tbl <- rb_tbl[,!(names(rb_tbl) %in% names(rb_tbl_c))]
rb_tbl_c <- rb_tbl[,grepl("throw", names(rb_tbl))]
rb_tbl <- rb_tbl[,!(names(rb_tbl) %in% names(rb_tbl_c))]
rb_tbl_c <- NULL

# Clean and set training table
rb_tbl$res <- abs(rb_tbl$avg_proj - rb_tbl$actual_pts)
rb_tbl <- rb_tbl[rb_tbl$res<100,]
rb_tbl$res <- NULL
rb_tbl[is.na(rb_tbl)] <- 0
rb_tbl$pos <- NULL
train <- rb_tbl
rb_2018 <- final_2018[final_2018$pos == "RB",]
rb_2018[is.na(rb_2018)] <- 0

### Random Forest
mod_rf_rb <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")

rb_2018$prediction <- predict(mod_rf_rb, rb_2018)


#### WR All ####
wr_tbl <- final_tbl[final_tbl$pos == "WR",]
wr_tbl <- wr_tbl[,!(names(wr_tbl) %in% c("gsis_id","player_id","trim_name","name",
                                         "season","prev_season","power_moves","
                                         finesse_moves",
                                         "block_shedding","pursuit","play_recognition",
                                         "man_coverage","zone_coverage","press",
                                         "kick_power","kick_accuracy","impact_blocking",
                                         "run_block","pass_block","kick_return",
                                         "lead_block","run_block_power",
                                         "pass_block_power","run_block_finesse",
                                         "pass_block_finesse"))]

# Remove Anything related to Passing
wr_tbl_c <- wr_tbl[,grepl("pass", names(wr_tbl))]
wr_tbl <- wr_tbl[,!(names(wr_tbl) %in% names(wr_tbl_c))]
wr_tbl_c <- wr_tbl[,grepl("throw", names(wr_tbl))]
wr_tbl <- wr_tbl[,!(names(wr_tbl) %in% names(wr_tbl_c))]
wr_tbl_c <- NULL

# Clean and set training table
wr_tbl$res <- abs(wr_tbl$avg_proj - wr_tbl$actual_pts)
wr_tbl <- wr_tbl[wr_tbl$res<100,]
wr_tbl$res <- NULL
wr_tbl[is.na(wr_tbl)] <- 0
wr_tbl$pos <- NULL
train <- wr_tbl
wr_2018 <- final_2018[final_2018$pos == "WR",]
wr_2018[is.na(wr_2018)] <- 0

### Random Forest
mod_rf_wr <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")
mod_rf_wr
wr_2018$prediction <- predict(mod_rf_wr, wr_2018)


#### TE All ####
te_tbl <- final_tbl[final_tbl$pos == "TE",]
te_tbl <- te_tbl[,!(names(te_tbl) %in% c("gsis_id","player_id","trim_name","name",
                                         "season","prev_season","power_moves","
                                         finesse_moves",
                                         "block_shedding","pursuit","play_recognition",
                                         "man_coverage","zone_coverage","press",
                                         "kick_power","kick_accuracy","impact_blocking",
                                         "run_block","pass_block","kick_return",
                                         "lead_block","run_block_power",
                                         "pass_block_power","run_block_finesse",
                                         "pass_block_finesse"))]

# Remove Anything related to Passing
te_tbl_c <- te_tbl[,grepl("pass", names(te_tbl))]
te_tbl <- te_tbl[,!(names(te_tbl) %in% names(te_tbl_c))]
te_tbl_c <- te_tbl[,grepl("throw", names(te_tbl))]
te_tbl <- te_tbl[,!(names(te_tbl) %in% names(te_tbl_c))]
te_tbl_c <- NULL

# Clean and set training table
te_tbl$res <- abs(te_tbl$avg_proj - te_tbl$actual_pts)
te_tbl <- te_tbl[te_tbl$res<100,]
te_tbl$res <- NULL
te_tbl[is.na(te_tbl)] <- 0
te_tbl$pos <- NULL
train <- te_tbl
te_2018 <- final_2018[final_2018$pos == "TE",]
te_2018[is.na(te_2018)] <- 0

### Random Forest
mod_rf_te <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")
mod_rf_te
te_2018$prediction <- predict(mod_rf_te, te_2018)


#### COMBINE ####

projections_2018 <- plyr::rbind.fill(qb_2018, qb_2018_rook, rb_2018, wr_2018, te_2018)
write.csv(projections_2018, paste0("2018_model_projections_",format(Sys.Date(),"%m%d%Y"),
                                   ".csv"),row.names = FALSE)

trim_proj_2018 <- projections_2018[,c("player_id","player_name","trim_name","pos","team","analyst_.1",
                                      "analyst_3","analyst_4","analyst_5","analyst_7","analyst_9",
                                      "avg_proj","prediction")]

for (r in 1:nrow(trim_proj_2018)) {
  trim_proj_2018$min_proj[r] <- min(trim_proj_2018$analyst_.1[r], trim_proj_2018$analyst_3[r],
                                    trim_proj_2018$analyst_4[r], trim_proj_2018$analyst_5[r],
                                    trim_proj_2018$analyst_7[r], trim_proj_2018$analyst_9[r])
  trim_proj_2018$max_proj[r] <- max(trim_proj_2018$analyst_.1[r], trim_proj_2018$analyst_3[r],
                                    trim_proj_2018$analyst_4[r], trim_proj_2018$analyst_5[r],
                                    trim_proj_2018$analyst_7[r], trim_proj_2018$analyst_9[r])
}
trim_proj_2018[,c("analyst_.1","analyst_3","analyst_4","analyst_5",
                  "analyst_7","analyst_9")] <- NULL

## Add DST and K From FFAnalytics
scrape_2018 <- scrape_data(src = c("CBS","ESPN","FantasyData","FantasyPros","FantasySharks",
                                   "FFToday","FleaFlicker","NumberFire","Yahoo",
                                   "FantasyFootballNerd","NFL",
                                   "RTSports","Walterfootball"),
                           pos = c("QB", "RB", "WR", "TE", "K", "DST"),
                           season = 2018,
                           week = 0)
ffa_proj_2018 <- projections_table(scrape_2018)
ffa_dst_k <- ffa_proj_2018[ffa_proj_2018$pos %in% c("DST","K"),]
ffa_dst_k <- ffa_dst_k[ffa_dst_k$avg_type=="weighted",]
ffa_dst_k <- ffanalytics::add_player_info(ffa_dst_k)
ffa_dst_k$player_name <- paste0(ffa_dst_k$first_name," ",ffa_dst_k$last_name)
ffa_dst_k$prediction <- ffa_dst_k$points
ffa_dst_k$trim_name <- ffa_dst_k$player_name
ffa_dst_k$trim_name <- gsub(" ", "",ffa_dst_k$trim_name)
ffa_dst_k$trim_name <- toupper(ffa_dst_k$trim_name)

ffa_dst_k <- ffa_dst_k[,c("id","player_name","trim_name","position","team","points","prediction",
                          "floor","ceiling")]
names(ffa_dst_k) <- names(trim_proj_2018)
trim_proj_2018 <- rbind(trim_proj_2018, ffa_dst_k)

# Add ADP
trim_proj_2018 <- merge(trim_proj_2018,
                        adp[,c("trim_name","position","adp","stdev")],
                        by.x = c("trim_name","pos"),
                        by.y = c("trim_name","position"),
                        all.x = TRUE)
trim_proj_2018$overall_rank <- rank(-trim_proj_2018$prediction)
max_adp <- max(adp$adp)
std_adp <- mean(adp$stdev[adp$adp>(max_adp-10)])
trim_proj_2018$adp[is.na(trim_proj_2018$adp)] <- trim_proj_2018$overall_rank[
                                                          is.na(trim_proj_2018$adp)]
trim_proj_2018$stdev[is.na(trim_proj_2018$stdev)] <- std_adp

# Remove Duplicates
trim_proj_2018 <- sqldf("SELECT
                         player_id,
                        player_name,
                        pos,
                        min(team) as team,
                        avg(avg_proj) as avg_proj,
                        avg(prediction) as prediction,
                        avg(min_proj) as min_proj,
                        avg(max_proj) as max_proj,
                        avg(adp) as adp,
                        avg(stdev) as adp_stdev
                        
                        FROM trim_proj_2018
                        
                        GROUP BY
                        player_id, player_name, pos")

trim_proj_2018 <- trim_proj_2018 %>%
  arrange(pos, -prediction) %>% 
  group_by(pos) %>% 
  mutate(pos_rank = order(order(prediction, decreasing = TRUE)))

# Add Tiers
for (p in c("QB","RB","WR","TE","DST","K")) {
  cluster <- Mclust(trim_proj_2018$prediction[trim_proj_2018$pos==p], G=15)
  trim_proj_2018$tier[trim_proj_2018$pos==p] <- cluster$classification
}

trim_proj_2018$overall_rank <- rank(-trim_proj_2018$prediction)

write.csv(trim_proj_2018, paste0("2018_trimmed_model_projections_",format(Sys.Date(),"%m%d%Y"),
                                   ".csv"),row.names = FALSE)


# Gold Mining Graphs

for (p in c("QB","RB","WR","TE","DST","K")) {
  gg_data <- trim_proj_2018[trim_proj_2018$pos==p & trim_proj_2018$pos_rank<61,]
  
  xlimit <- 10*ceiling(max(gg_data$prediction)/10) + 75
  cluster <- Mclust(gg_data$prediction, G=7)
  gg_data$tier <- cluster$classification
  gg_data$name_lbl <- pmax(gg_data$max_proj, gg_data$prediction)
  vor <- ifelse(p=="TE",16,ifelse(p=="WR",34,ifelse(p=="RB",34,13)))
  
  ggplot(gg_data, 
         aes(x=prediction, y=pos_rank, color=factor(tier))) +
    geom_errorbarh(aes(xmin=min_proj,xmax=max_proj),height=.3)+
    geom_point(size=5,color="white")+
    geom_text(aes(x=prediction,label=round(prediction,0)),size=3,show.legend = FALSE)+
    geom_text(aes(x=name_lbl, label=player_name),
              hjust=-0.2, angle=(0), size=3,show.legend = FALSE)+
    geom_hline(yintercept = vor+0.5, linetype="dashed") +
    geom_text(aes(0,vor, label = "VOR Baseline", vjust = -0.5), size=3, color="black") +
    theme_minimal()+
    theme(legend.position = c(0.95, 0.1)) + 
    scale_y_reverse()+
    ylab("Rank") + 
    xlab(paste0(p," - Predicted Points Compared to Projections")) +
    coord_cartesian(xlim =c(0,xlimit))+
    scale_color_tableau()
  
  ggsave(paste0(p," Gold Mining ",Sys.Date(),".png"),
         width = 25, height = 20, units = "cm", dpi = 300)
  
}


