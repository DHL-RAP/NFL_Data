rm(list=ls())

library(dplyr)
library(caret)
library(ggplot2)
library(randomForest)
library(reshape2)

set.seed(1217)

madden <- read.csv("madden_final.csv", stringsAsFactors = FALSE)
players <- read.csv("master_nfl_player_list.csv", stringsAsFactors = FALSE)
projections <- read.csv("final_projections.csv", stringsAsFactors = FALSE)
pbp_stats <- read.csv("pbp_stats.csv", stringsAsFactors = FALSE)
proj_2018 <- read.csv("2018_aggregate_projections_full.csv", stringsAsFactors = FALSE)

# Remove all historical players who didn't play after 2008
players$last_year <- players$draft_year + players$years_played - 1
players <- players[players$last_year>2008,]


# Actual Results

# Projections + player merge
final_tbl <- merge(projections,
                   players[,c("player_id","trim_name","position","gsis_id","draft_year")],
                   by.x = c("trim_name","pos"),
                   by.y = c("trim_name","position"))
final_tbl$experience <- final_tbl$season - final_tbl$draft_year
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


#### CREATE MODEL ####

ctrl <- trainControl(method = "repeatedcv",
                     number = 10,
                     repeats = 5,
                     savePred = T)


#### QB ####
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
qb_tbl <- qb_tbl[qb_tbl$res < 100 & qb_tbl$experience>0,]
qb_tbl$res <- NULL
qb_tbl[is.na(qb_tbl)] <- 0
qb_tbl$pos <- as.factor(qb_tbl$pos)
qb_tbl <- qb_tbl[qb_tbl$pos=="QB",]
train_nbr <- createDataPartition(qb_tbl$actual_pts,p=0.7, 
                                 list=FALSE)
train <- qb_tbl[train_nbr,]
test <- qb_tbl[-train_nbr,]

# RandomForest
rf <- randomForest(actual_pts ~ .,
                   data = train,
                   ntree=100,
                   importance = TRUE)
rf
test$prediction <- predict(rf, test)

# Fill Out Residuals
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

model_performance <- data.frame(position = "QB",
                                time_stamp = as.POSIXlt(Sys.time()),
                                model_type = "Random Forest Package",
                                a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                                a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                                a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                                a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                                a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                                a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                                avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                                model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                                model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                                    sum((test$actual_pts - mean(test$actual_pts))^2))
names(model_performance) <- c("position","time_stamp","model_type","a1_rmse","a3_rmse",
                              "a4_rmse","a5_rmse","a7_rmse","a9_rmse","avg_rmse",
                              "model_rmse","model_r2")


### GLM
mod_glm <- train(actual_pts ~ .,
                 data = train,
                 method = "glm",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_glm, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                                        sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS
mod_bagmars <- train(actual_pts ~ .,
                 data = train,
                 method = "bagEarth",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_bagmars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### K-nearest neighbors
mod_knn <- train(actual_pts ~ .,
                  data = train,
                  method = "knn",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_knn, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "KNN",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS w/ gCV Pruning
mod_marsGCV <- train(actual_pts ~ .,
                     data = train,
                     method = "bagEarthGCV",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_marsGCV, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS w/ gCV Pruning",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Basic MARS
mod_mars <- train(actual_pts ~ .,
                     data = train,
                     method = "gcvEarth",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_mars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Ridge Regression w/ Variable Selection
mod_rrvs <- train(actual_pts ~ .,
                  data = train,
                  method = "foba",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_rrvs, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Ridge Regression w/ Variable Selection",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Support Vector Machine w/ Linear Basis
mod_svmlin <- train(actual_pts ~ .,
                  data = train,
                  method = "svmLinear",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_svmlin, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Support Vector Machine w/ Linear Basis",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Random Forest
mod_rf <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")

test$prediction <- predict(mod_rf, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Random Forest",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Boosted Generalized Linear Model
mod_glmboost <- train(actual_pts ~ .,
                data = train,
                method = "glmboost",
                trControl = ctrl,
                metric = "RMSE")

test$prediction <- predict(mod_glmboost, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Boosted GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Xtreme Gradient Boosting - Tree

xgb_grid <- expand.grid(nrounds = seq(from = 5, to = 10,by = 1),
                        max_depth = seq(from = 1, to = 15, by = 2),
                        eta = c(0.01, 0.001, .1),
                        gamma = c(1, 2),
                        colsample_bytree = c(0.4, .5, .7, .9),
                        min_child_weight = c(0.5, 1, 1.5),
                        subsample = 0)

mod_xgbtree <- train(actual_pts ~ .,
                      data = train,
                      method = "xgbTree",
                      trControl = ctrl,
                      metric = "RMSE")
                     # tuneGrid = xgb_grid)

test$prediction <- predict(mod_xgbtree, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "QB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Xtreme Gradient Boosting - Tree",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)

model_performance_qb <- model_performance

### Visualize Results ###

model_results <- resamples(list(bagmars = mod_bagmars,
                                glm = mod_glm,
                                glmboost = mod_glmboost,
                                knn = mod_knn,
                                mars = mod_mars,
                                marsgcv = mod_marsGCV,
                                rf = mod_rf,
                                rrvs = mod_rrvs,
                                svmlin = mod_svmlin,
                                xgbtree = mod_xgbtree))
model_comparison <- as.data.frame(model_results)
model_comparison$position <- "QB"

summary(model_results)
bwplot(model_results)
dotplot(model_results)

# Actual vs Predicted Graph
ggplot(test, aes(x=prediction,y=actual_pts)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Residual Comparison
test_gg <- melt(test[,c("actual_pts","analyst_.1","analyst_4","analyst_3","analyst_5",
                        "analyst_7","analyst_9","prediction", "avg_proj")], 
                id="actual_pts")
test_gg$res <- abs(test_gg$actual_pts-test_gg$value)
ggplot(test_gg,aes(x=res, fill = variable)) +
  geom_density(alpha=0.5)

# Feature Importance
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])
ggplot(featureImportance, 
       aes(x=reorder(Feature, Importance), 
           y=Importance)) + 
  geom_bar(stat="identity", fill="#d40511") +
  coord_flip() +
  theme_light(base_size=10) + 
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))

qb_imp <- featureImportance$Feature[featureImportance$Importance>0]


# Predict 2018


#### RB ####
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
rb_tbl[is.na(rb_tbl)] <- 0
rb_tbl$pos <- NULL
train_nbr <- createDataPartition(rb_tbl$actual_pts,p=0.7, list=FALSE)
train <- rb_tbl[train_nbr,]
test <- rb_tbl[-train_nbr,]

# RandomForest
rf <- randomForest(actual_pts ~ .,
                   data = train,
                   ntree=100,
                   importance = TRUE)
rf
test$prediction <- predict(rf, test)

# Fill Out Residuals
test$mod_res <- (test$prediction - test$actual_pts)^2
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2

model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Random Forest Package",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)

### GLM
mod_glm <- train(actual_pts ~ .,
                 data = train,
                 method = "glm",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_glm, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS
mod_bagmars <- train(actual_pts ~ .,
                     data = train,
                     method = "bagEarth",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_bagmars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### K-nearest neighbors
mod_knn <- train(actual_pts ~ .,
                 data = train,
                 method = "knn",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_knn, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "KNN",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS w/ gCV Pruning
mod_marsGCV <- train(actual_pts ~ .,
                     data = train,
                     method = "bagEarthGCV",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_marsGCV, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS w/ gCV Pruning",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Basic MARS
mod_mars <- train(actual_pts ~ .,
                  data = train,
                  method = "gcvEarth",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_mars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Ridge Regression w/ Variable Selection
mod_rrvs <- train(actual_pts ~ .,
                  data = train,
                  method = "foba",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_rrvs, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Ridge Regression w/ Variable Selection",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Support Vector Machine w/ Linear Basis
mod_svmlin <- train(actual_pts ~ .,
                    data = train,
                    method = "svmLinear",
                    trControl = ctrl,
                    metric = "RMSE")

test$prediction <- predict(mod_svmlin, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Support Vector Machine w/ Linear Basis",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Random Forest
mod_rf <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")

test$prediction <- predict(mod_rf, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Random Forest",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Boosted Generalized Linear Model
mod_glmboost <- train(actual_pts ~ .,
                      data = train,
                      method = "glmboost",
                      trControl = ctrl,
                      metric = "RMSE")

test$prediction <- predict(mod_glmboost, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Boosted GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Xtreme Gradient Boosting - Tree

xgb_grid <- expand.grid(nrounds = seq(from = 5, to = 10,by = 1),
                        max_depth = seq(from = 1, to = 15, by = 2),
                        eta = c(0.01, 0.001, .1),
                        gamma = c(1, 2),
                        colsample_bytree = c(0.4, .5, .7, .9),
                        min_child_weight = c(0.5, 1, 1.5))

mod_xgbtree <- train(actual_pts ~ .,
                     data = train,
                     method = "xgbTree",
                     trControl = ctrl,
                     metric = "RMSE")
                     #tuneGrid = xgb_grid)

test$prediction <- predict(mod_xgbtree, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Xtreme Gradient Boosting - Tree",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


#### WRTE ####
WRTE_tbl <- final_tbl[final_tbl$pos %in% c("WR","TE"),]
WRTE_tbl <- WRTE_tbl[,!(names(WRTE_tbl) %in% c("gsis_id","player_id","trim_name","name",
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
WRTE_tbl_c <- WRTE_tbl[,grepl("pass", names(WRTE_tbl))]
WRTE_tbl <- WRTE_tbl[,!(names(WRTE_tbl) %in% names(WRTE_tbl_c))]
WRTE_tbl_c <- WRTE_tbl[,grepl("throw", names(WRTE_tbl))]
WRTE_tbl <- WRTE_tbl[,!(names(WRTE_tbl) %in% names(WRTE_tbl_c))]
WRTE_tbl_c <- NULL

# Clean and set training table
WRTE_tbl$res <- abs(WRTE_tbl$avg_proj - WRTE_tbl$actual_pts)
WRTE_tbl <- WRTE_tbl[WRTE_tbl$res<100,]
WRTE_tbl[is.na(WRTE_tbl)] <- 0
WRTE_tbl$pos <- NULL
train_nbr <- createDataPartition(WRTE_tbl$actual_pts,p=0.7, list=FALSE)
train <- WRTE_tbl[train_nbr,]
test <- WRTE_tbl[-train_nbr,]

# RandomForest
rf <- randomForest(actual_pts ~ .,
                   data = train,
                   ntree=100,
                   importance = TRUE)
rf
test$prediction <- predict(rf, test)

# Fill Out Residuals
test$mod_res <- (test$prediction - test$actual_pts)^2
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2

model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Random Forest Package",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)

### GLM
mod_glm <- train(actual_pts ~ .,
                 data = train,
                 method = "glm",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_glm, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS
mod_bagmars <- train(actual_pts ~ .,
                     data = train,
                     method = "bagEarth",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_bagmars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### K-nearest neighbors
mod_knn <- train(actual_pts ~ .,
                 data = train,
                 method = "knn",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_knn, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "KNN",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS w/ gCV Pruning
mod_marsGCV <- train(actual_pts ~ .,
                     data = train,
                     method = "bagEarthGCV",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_marsGCV, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS w/ gCV Pruning",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Basic MARS
mod_mars <- train(actual_pts ~ .,
                  data = train,
                  method = "gcvEarth",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_mars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Ridge Regression w/ Variable Selection
mod_rrvs <- train(actual_pts ~ .,
                  data = train,
                  method = "foba",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_rrvs, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Ridge Regression w/ Variable Selection",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Support Vector Machine w/ Linear Basis
mod_svmlin <- train(actual_pts ~ .,
                    data = train,
                    method = "svmLinear",
                    trControl = ctrl,
                    metric = "RMSE")

test$prediction <- predict(mod_svmlin, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Support Vector Machine w/ Linear Basis",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Random Forest
mod_rf <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")

test$prediction <- predict(mod_rf, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Random Forest",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Boosted Generalized Linear Model
mod_glmboost <- train(actual_pts ~ .,
                      data = train,
                      method = "glmboost",
                      trControl = ctrl,
                      metric = "RMSE")

test$prediction <- predict(mod_glmboost, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Boosted GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Xtreme Gradient Boosting - Tree

xgb_grid <- expand.grid(nrounds = seq(from = 5, to = 10,by = 1),
                        max_depth = seq(from = 1, to = 15, by = 2),
                        eta = c(0.01, 0.001, .1),
                        gamma = c(1, 2),
                        colsample_bytree = c(0.4, .5, .7, .9),
                        min_child_weight = c(0.5, 1, 1.5))

mod_xgbtree <- train(actual_pts ~ .,
                     data = train,
                     method = "xgbTree",
                     trControl = ctrl,
                     metric = "RMSE")
#tuneGrid = xgb_grid)

test$prediction <- predict(mod_xgbtree, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "WRTE",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Xtreme Gradient Boosting - Tree",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)

#### ALL ####
final_trim_tbl <- final_tbl[,-c(1:4,6)]
train_nbr <- createDataPartition(final_trim_tbl$actual_pts,p=0.7, list=FALSE)
train <- final_trim_tbl[train_nbr,]
test <- final_trim_tbl[-train_nbr,]

# RandomForest
rf <- randomForest(actual_pts ~ .,
                   data = train,
                   ntree=100,
                   importance = TRUE)
rf
test$prediction <- predict(rf, test)

# Fill Out Residuals
test$mod_res <- (test$prediction - test$actual_pts)^2
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Random Forest Package",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)

### GLM
mod_glm <- train(actual_pts ~ .,
                 data = train,
                 method = "glm",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_glm, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$avg_res <- (test$avg_proj - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS
mod_bagmars <- train(actual_pts ~ .,
                     data = train,
                     method = "bagEarth",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_bagmars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### K-nearest neighbors
mod_knn <- train(actual_pts ~ .,
                 data = train,
                 method = "knn",
                 trControl = ctrl,
                 metric = "RMSE")

test$prediction <- predict(mod_knn, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "KNN",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Bagged MARS w/ gCV Pruning
mod_marsGCV <- train(actual_pts ~ .,
                     data = train,
                     method = "bagEarthGCV",
                     trControl = ctrl,
                     metric = "RMSE")

test$prediction <- predict(mod_marsGCV, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Bagged MARS w/ gCV Pruning",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Basic MARS
mod_mars <- train(actual_pts ~ .,
                  data = train,
                  method = "gcvEarth",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_mars, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "MARS",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Ridge Regression w/ Variable Selection
mod_rrvs <- train(actual_pts ~ .,
                  data = train,
                  method = "foba",
                  trControl = ctrl,
                  metric = "RMSE")

test$prediction <- predict(mod_rrvs, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Ridge Regression w/ Variable Selection",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Support Vector Machine w/ Linear Basis
mod_svmlin <- train(actual_pts ~ .,
                    data = train,
                    method = "svmLinear",
                    trControl = ctrl,
                    metric = "RMSE")

test$prediction <- predict(mod_svmlin, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Support Vector Machine w/ Linear Basis",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Random Forest
mod_rf <- train(actual_pts ~ .,
                data = train,
                method = "rf",
                trControl = ctrl,
                metric = "RMSE")

test$prediction <- predict(mod_rf, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Random Forest",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Boosted Generalized Linear Model
mod_glmboost <- train(actual_pts ~ .,
                      data = train,
                      method = "glmboost",
                      trControl = ctrl,
                      metric = "RMSE")

test$prediction <- predict(mod_glmboost, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Boosted GLM",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a3_rmse = sqrt(sum(test$a3_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)


### Xtreme Gradient Boosting - Tree

xgb_grid <- expand.grid(nrounds = seq(from = 5, to = 10,by = 1),
                        max_depth = seq(from = 1, to = 15, by = 2),
                        eta = c(0.01, 0.001, .1),
                        gamma = c(1, 2),
                        colsample_bytree = c(0.4, .5, .7, .9),
                        min_child_weight = c(0.5, 1, 1.5))

mod_xgbtree <- train(actual_pts ~ .,
                     data = train,
                     method = "xgbTree",
                     trControl = ctrl,
                     metric = "RMSE",
                     tuneGrid = xgb_grid)

test$prediction <- predict(mod_xgbtree, test)
test$a1_res <- (test$analyst_.1 - test$actual_pts)^2
test$a3_res <- (test$analyst_3 - test$actual_pts)^2
test$a4_res <- (test$analyst_4 - test$actual_pts)^2
test$a5_res <- (test$analyst_5 - test$actual_pts)^2
test$a7_res <- (test$analyst_7 - test$actual_pts)^2
test$a9_res <- (test$analyst_9 - test$actual_pts)^2
test$mod_res <- (test$prediction - test$actual_pts)^2

# Append results to table
model_perf_tmp <- data.frame(position = "RB",
                             time_stamp = as.POSIXlt(Sys.time()),
                             model_type = "Xtreme Gradient Boosting - Tree",
                             a1_rmse = sqrt(sum(test$a1_res)/nrow(test)),
                             a4_rmse = sqrt(sum(test$a4_res)/nrow(test)),
                             a5_rmse = sqrt(sum(test$a5_res)/nrow(test)),
                             a7_rmse = sqrt(sum(test$a7_res)/nrow(test)),
                             a9_rmse = sqrt(sum(test$a9_res)/nrow(test)),
                             avg_rmse = sqrt(sum(test$avg_res)/nrow(test)),
                             model_rmse = sqrt(sum(test$mod_res)/nrow(test)),
                             model_r2 = 1-sum((test$actual_pts-test$prediction)^2)/
                               sum((test$actual_pts - mean(test$actual_pts))^2))
model_performance <- rbind(model_performance, model_perf_tmp)

model_performance_RB <- model_performance[model_performance$position=="RB",]

#### Visualize Results ####

model_results <- resamples(list(bagmars = mod_bagmars,
                                glm = mod_glm,
                                glmboost = mod_glmboost,
                                knn = mod_knn,
                                mars = mod_mars,
                                marsgcv = mod_marsGCV,
                                rf = mod_rf,
                                rrvs = mod_rrvs,
                                svmlin = mod_svmlin,
                                xgbtree = mod_xgbtree))
model_comparison_tmp <- as.data.frame(model_results)
model_comparison_tmp$position <- "ALL"
model_comparison <- rbind(model_comparison, model_comparison_tmp)

summary(model_results)
bwplot(model_results)
dotplot(model_results)

# Actual vs Predicted Graph
ggplot(test, aes(x=prediction,y=actual_pts)) +
  geom_point() +
  geom_smooth(method="auto", se=TRUE, fullrange=FALSE, level=0.95)

# Residual Comparison
test_gg <- melt(test[,c("actual_pts","analyst_.1","analyst_4","analyst_5",
                        "analyst_7","analyst_9","prediction", "avg_proj")], 
                id="actual_pts")
test_gg$res <- abs(test_gg$actual_pts-test_gg$value)
ggplot(test_gg,aes(x=res, fill = variable)) +
  geom_density(alpha=0.5)

# Feature Importance
imp <- importance(rf, type=1)
featureImportance <- data.frame(Feature=row.names(imp), 
                                Importance=imp[,1])
ggplot(featureImportance, 
       aes(x=reorder(Feature, Importance), 
           y=Importance)) + 
  geom_bar(stat="identity", fill="#d40511") +
  coord_flip() +
  theme_light(base_size=10) + 
  xlab("") +
  ylab("Importance") + 
  ggtitle("Random Forest Feature Importance\n") +
  theme(plot.title=element_text(size=18))


