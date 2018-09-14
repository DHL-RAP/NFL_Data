rm(list=ls())

library(dplyr)
library(caret)
library(randomForest)

madden_data <- read.csv("madden_pre_clean.csv")
columns_to_clean <- c("tup","brs","brt","lbl","rbp","pbp","rpf","pbf")

## Kick Return
madden_trim <- madden_data[!is.na(madden_data$ret),c(8,11:55)] 
train_nbr <- createDataPartition(madden_trim$ret,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(ret ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

# test$pred_tup <- predict(rf, test)
madden_new <- madden_data[is.na(madden_data$ret),]
madden_new$ret <- predict(rf, madden_new)
madden_old <- madden_data[!is.na(madden_data$ret),]
madden_data <- rbind(madden_new, madden_old)
rm(madden_old, madden_new)

madden_18 <- madden_data[madden_data$season==2018,]
madden_non18 <- madden_data[madden_data$season != 2018,]

## Throw under pressure
madden_trim <- madden_18[,c(8,11:56)]
train_nbr <- createDataPartition(madden_trim$tup,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(tup ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

# test$pred_tup <- predict(rf, test)
madden_non18$tup <- predict(rf, madden_non18)


## Break Sack
madden_trim <- madden_18[,c(8,11:55,57)]
train_nbr <- createDataPartition(madden_trim$brs,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(brs ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_brs <- predict(rf, test)
madden_non18$brs <- predict(rf, madden_non18)


## Break Tackle
madden_trim <- madden_18[,c(8,11:55,58)]
train_nbr <- createDataPartition(madden_trim$brt,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(brt ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_brt <- predict(rf, test)
madden_non18$brt <- predict(rf, madden_non18)


## Lead Block
madden_trim <- madden_18[,c(8,11:55,59)]
train_nbr <- createDataPartition(madden_trim$lbl,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(lbl ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_lbl <- predict(rf, test)
madden_non18$lbl <- predict(rf, madden_non18)


## Run Block Power
madden_trim <- madden_18[,c(8,11:55,60)]
train_nbr <- createDataPartition(madden_trim$rbp,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(rbp ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_rbp <- predict(rf, test)
madden_non18$rbp <- predict(rf, madden_non18)


## Pass Block Power
madden_trim <- madden_18[,c(8,11:55,61)]
train_nbr <- createDataPartition(madden_trim$pbp,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(pbp ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_pbp <- predict(rf, test)
madden_non18$pbp <- predict(rf, madden_non18)


## Run Block Finesse
madden_trim <- madden_18[,c(8,11:55,62)]
train_nbr <- createDataPartition(madden_trim$rpf,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(rpf ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_rpf <- predict(rf, test)
madden_non18$rpf <- predict(rf, madden_non18)


## Pass Block Finesse
madden_trim <- madden_18[,c(8,11:55,63)]
train_nbr <- createDataPartition(madden_trim$pbf,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(pbf ~ .,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_pbf <- predict(rf, test)
madden_non18$pbf <- predict(rf, madden_non18)


## Short Route Running
madden_trim <- madden_18[,c(8,11:55,72)]
train_nbr <- createDataPartition(madden_trim$Short_Route_Runing,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(Short_Route_Runing ~ . -rte,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_Short_Route_Runing <- predict(rf, test)
madden_non18$Short_Route_Runing <- predict(rf, madden_non18)


## Medium Route Running
madden_trim <- madden_18[,c(8,11:55,73)]
train_nbr <- createDataPartition(madden_trim$Medium_Route_Running,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(Medium_Route_Running ~ . -rte,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_Medium_Route_Running <- predict(rf, test)
madden_non18$Medium_Route_Running <- predict(rf, madden_non18)


## Deep Route Running
madden_trim <- madden_18[,c(8,11:55,74)]
train_nbr <- createDataPartition(madden_trim$Deep_Route_Running,p=0.7, list=FALSE)
train <- madden_trim[train_nbr,]
test <- madden_trim[-train_nbr,]

rf <- randomForest(Deep_Route_Running ~ . -rte,
                   data = train,
                   ntree = 100,
                   importance = TRUE)

test$pred_Deep_Route_Running <- predict(rf, test)
madden_non18$Deep_Route_Running <- predict(rf, madden_non18)


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


madden_final <- rbind(madden_18,madden_non18)
write.csv(madden_final, "madden_final.csv",row.names = FALSE)
