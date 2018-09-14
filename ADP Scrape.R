
library(jsonlite)
library(dplyr)
library(XML)
library(httr)
library(stringr)

# Get ADP & St.Dev
adp_url <- "https://fantasyfootballcalculator.com/api/v1/adp/standard?teams=12&year=2018"
adp_json <- fromJSON(adp_url)
adp <- adp_json$players
adp$position[adp$position=="DEF"] <- "DST" 

# Get Backup #s from FFPros for missing players
adp_url <- "https://www.fantasypros.com/nfl/adp/overall.php"
adp_html <- GET(adp_url)
adp_backup <- data.frame(readHTMLTable(rawToChar(adp_html$content),
                                   as.data.frame = TRUE,
                                   stringsAsFactors=FALSE)
                     $data)
adp_backup <- adp_backup[,c(2,3,11)]
names(adp_backup) <- c("name","pos","adp")
adp_backup$pos_par <- str_locate(adp_backup$name, "\\(")[, 1]
adp_backup$name <- substr(adp_backup$name,1,adp_backup$pos_par-5)
adp_backup$pos <- ifelse(substr(adp_backup$pos,1,1)=="D","DST",
                         ifelse(substr(adp_backup$pos,1,1)=="K","K",
                                substr(adp_backup$pos,1,2)))

adp$name <- gsub("'","",adp$name)
adp$name <- gsub("\\.","",adp$name)
adp$name <- gsub("-","",adp$name)
adp$name <- gsub(" IV","",adp$name)
adp$name <- gsub(" III","",adp$name)
adp$name <- gsub(" II","",adp$name)
adp$name <- gsub(" Jr","",adp$name)
adp$name <- gsub(" Sr","",adp$name)
adp$name <- gsub("\\*","",adp$name)
adp$trim_name <- adp$name
adp$trim_name <- gsub(" ", "",adp$trim_name)
adp$trim_name <- toupper(adp$trim_name)

adp_backup$name <- gsub("'","",adp_backup$name)
adp_backup$name <- gsub("\\.","",adp_backup$name)
adp_backup$name <- gsub("-","",adp_backup$name)
adp_backup$name <- gsub(" IV","",adp_backup$name)
adp_backup$name <- gsub(" III","",adp_backup$name)
adp_backup$name <- gsub(" II","",adp_backup$name)
adp_backup$name <- gsub(" Jr","",adp_backup$name)
adp_backup$name <- gsub(" Sr","",adp_backup$name)
adp_backup$name <- gsub("\\*","",adp_backup$name)
adp_backup$trim_name <- adp_backup$name
adp_backup$trim_name <- gsub(" ", "",adp_backup$trim_name)
adp_backup$trim_name <- toupper(adp_backup$trim_name)

# Add missing players from backup to main

adp_final <- merge(adp[,c("trim_name","position","adp","stdev")],
                   adp_backup[,c("pos","trim_name","adp")],
                   by.x = c("trim_name","position"),
                   by.y = c("trim_name","pos"),
                   all.y = TRUE)
adp_final$adp <- ifelse(is.na(adp_final$adp.x),
                        adp_final$adp.y,
                        adp_final$adp.x)
adp_final$stdev[is.na(adp_final$stdev)] <- 20
adp_final$adp.x <- NULL
adp_final$adp.y <- NULL

write.csv(adp_final, "2018_adp.csv", row.names = FALSE)
