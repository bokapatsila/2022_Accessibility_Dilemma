### This code was developed to calculate Job accessibility by a car for the paper 
### "Resolving the accessibility dilemma: Comparing cumulative and gravity-based measures 
###  of accessibility in eight Canadian cities"
### The paper is available at https://doi.org/10.1016/j.jtrangeo.2023.103530

### The code was developed by Bogdan Kapatsila and  Manuel Santana Palacios in Spring-Summer 2022


rm(list=ls())
setwd("D:/Research/2022_Cum_Grav_Accessibility/Montreal")

### Packages used throughout the analysis

library(tidyverse)
library(data.table)
library(RColorBrewer)
library(ggplot2)

### Read CMA CT centroids (preprocessed in GIS) ================================

#Setup the environment for r5r, osm.pbf id from https://extract.bbbike.org/
r5r_core <- setup_r5('D:/Research/2022_Cum_Grav_Accessibility/Montreal', verbose=FALSE)

ct_centroids <- read.csv('Inputs/MontrealCMA_Centroids.csv', header = TRUE, na.strings = c("","NA"))
ct_centroids$id <- as.character(ct_centroids$id)
ct_centroids$id <- ifelse(nchar(ct_centroids$id)==7,paste0(ct_centroids$id,'.00'),ct_centroids$id)
#ct_centroids <- ct_centroids[, c(1, 3, 2)] # Should be id, lat, lon

# Check where origin or destination points are snapped
snap_df <- find_snap(r5r_core, points = ct_centroids, mode = 'CAR')

### Total jobs file (all modes), add coordinates for cumulative access =========

# Read jobs
jobs.can <- read.csv('Inputs/Jobs_at_CTs_reduced.csv', colClasses = c(ID = "character"))


jobs.can <- rename(jobs.can, 
                   id = ID,
                   total_jobs=
                     Total...Industry...North.American.Industry.Classification.System..NAICS..2012,
                   'under.5k' = 'X......Under..5.000..including.loss.',
                   '5k.and.9.99k' = 'X.......5.000.to..9.999',
                   '10k.and.14.99k' = 'X.......10.000.to..14.999',
                   '15k.and.19.99k' = 'X.......15.000.to..19.999',
                   '20k.and.29.99k' = 'X.....20.000.to..29.999',
                   '30k.and.39.99k' = 'X.....30.000.to..39.999',
                   '40k.and.49.99k' = 'X.....40.000.to..49.999',
                   '50k.and.59.99k' = 'X.....50.000.to..59.999',
                   '60k.and.69.99k' = 'X.....60.000.to..69.999',
                   '70K.and.79.99k' = 'X.....70.000.to..79.999',
                   '80K.and.89.99k' = 'X.....80.000.to..89.999',
                   '90K.and.99.99k' = 'X.....90.000.to..99.999',
                   '100k.and.plus' = 'X.....100.000.and.over')

colnames(jobs.can)

jobs.can1 <- jobs.can %>% filter(`under.5k` != 'x',
                                 `5k.and.9.99k` != 'x',
                                 `10k.and.14.99k` != 'x',
                                 `15k.and.19.99k` != 'x',
                                 `20k.and.29.99k` != 'x',
                                 `30k.and.39.99k` != 'x',
                                 `40k.and.49.99k` != 'x',
                                 `50k.and.59.99k` != 'x',
                                 `60k.and.69.99k` != 'x',
                                 `70K.and.79.99k` != 'x',
                                 `80K.and.89.99k` != 'x',
                                 `90K.and.99.99k` != 'x',
                                 `100k.and.plus` != 'x')

jobs.can2 <- jobs.can1 %>% mutate_at(c('total_jobs',
                                       'under.5k',
                                       '5k.and.9.99k',
                                       '10k.and.14.99k',
                                       '15k.and.19.99k',
                                       '20k.and.29.99k',
                                       '30k.and.39.99k',
                                       '40k.and.49.99k',
                                       '50k.and.59.99k',
                                       '60k.and.69.99k',
                                       '70K.and.79.99k',
                                       '80K.and.89.99k',
                                       '90K.and.99.99k',
                                       '100k.and.plus'), as.numeric)

jobs.can2$jobs.lower.inc <- (jobs.can2$`under.5k` + 
                               jobs.can2$`5k.and.9.99k` + 
                               jobs.can2$`10k.and.14.99k` + 
                               jobs.can2$`15k.and.19.99k` + 
                               jobs.can2$`20k.and.29.99k`)

jobs.can2$jobs.higher.inc <-(jobs.can2$`30k.and.39.99k` + 
                               jobs.can2$`40k.and.49.99k` +
                               jobs.can2$`50k.and.59.99k` + 
                               jobs.can2$`60k.and.69.99k` + 
                               jobs.can2$`70K.and.79.99k` + 
                               jobs.can2$`80K.and.89.99k` + 
                               jobs.can2$`90K.and.99.99k` + 
                               jobs.can2$`100k.and.plus`)

jobs.slim <- select(jobs.can2,
                    c("id",
                      "total_jobs",
                      'jobs.lower.inc',
                      'jobs.higher.inc')) %>% filter(substr(id, 1,3)==462)
jobs.slim$id <- ifelse(nchar(jobs.slim$id)==7,paste0(jobs.slim$id,'.00'),jobs.slim$id)

write.csv(jobs.slim,"Outputs/all_jobs_montreal.csv")

# Join jobs with centroids and save

ct_centroids_jobs <- left_join(ct_centroids, jobs.slim, by = 'id')
write.csv(ct_centroids_jobs,"Outputs/ct_centroids_jobs_montreal.csv")


rm(list=ls())

### CUMULATIVE VEHICLE ACCESS AND TRAVEL TIME FOR FITTING ======================
# Travel time data was a custom purchase from Google
# Travel time data: format = (FromCTUID, toCTUID, TravelTime in minutes)
TT <- read.csv("Inputs/montreal.results.csv", colClasses = "character")

TT <- subset(TT, select=c(OriginCTUID, DestinationCTUID, Duration))
colnames(TT) = c ("From", "To","Time") # Rename columns

TT$Time <- as.numeric(as.character(TT$Time)) # convert travel time from factors to numbers
TT[is.na(TT)] <- 999999999 # Converting NA to inf
TT$Time = TT$Time/60 #converts the travel times in "Time" column of table "TT" into minutes from seconds (remove for halifax - file already in minutes)

TT_0 <- filter(TT, Time==0) # Leave 0 travel time as 0
TT_Positive <- filter(TT, Time>0) # Get all travel times more than 0
TT_Positive <- mutate(TT_Positive, Time=Time+5.63) #Adjust for parking search time
TT <- rbind(TT_0, TT_Positive) # Combine back

# Save travel time for fitting
ttm_8_9am_car <- subset(TT, select=c(From, To, Time))
ttm_8_9am_car$od <- paste0(ttm_8_9am_car$From, '-', ttm_8_9am_car$To)
ttm_8_9am_car <- subset(ttm_8_9am_car, select=-c(From, To))
names(ttm_8_9am_car)[names(ttm_8_9am_car)=='Time'] <- 'travel_time'
write.csv(ttm_8_9am_car, "Outputs/ttm_8_9am_car.csv")

rm(TT_0, TT_Positive)

# All jobs, this dataset was a custom a request from Statistics Canada producing counts of 
# commuters from and to every Census Tract by income group
Locations <- read.csv("Outputs/all_jobs_montreal.csv", colClasses = "character")
names(Locations)[names(Locations)=='total_jobs'] <- 'Jobs'

# Merge jobs with travel time 
TT_Locations <- merge(TT, Locations, by.x = "To",by.y="id")
TT_Locations$Jobs <- as.numeric(TT_Locations$Jobs)

# Counting jobs only if travel time is lower than XX minutes
TT_Locations$Jobs_5 <- ifelse (TT_Locations$Time<5, TT_Locations$Jobs,0)
TT_Locations$Jobs_10 <- ifelse (TT_Locations$Time<10, TT_Locations$Jobs,0)
TT_Locations$Jobs_15 <- ifelse (TT_Locations$Time<15, TT_Locations$Jobs,0)
TT_Locations$Jobs_20 <- ifelse (TT_Locations$Time<20, TT_Locations$Jobs,0)
TT_Locations$Jobs_25 <- ifelse (TT_Locations$Time<25, TT_Locations$Jobs,0)
TT_Locations$Jobs_30 <- ifelse (TT_Locations$Time<30, TT_Locations$Jobs,0)
TT_Locations$Jobs_35 <- ifelse (TT_Locations$Time<35, TT_Locations$Jobs,0)
TT_Locations$Jobs_40 <- ifelse (TT_Locations$Time<40, TT_Locations$Jobs,0)
TT_Locations$Jobs_45 <- ifelse (TT_Locations$Time<45, TT_Locations$Jobs,0)
TT_Locations$Jobs_50 <- ifelse (TT_Locations$Time<50, TT_Locations$Jobs,0)
TT_Locations$Jobs_55 <- ifelse (TT_Locations$Time<55, TT_Locations$Jobs,0)
TT_Locations$Jobs_60 <- ifelse (TT_Locations$Time<60, TT_Locations$Jobs,0)
TT_Locations$Jobs_65 <- ifelse (TT_Locations$Time<65, TT_Locations$Jobs,0)
TT_Locations$Jobs_70 <- ifelse (TT_Locations$Time<70, TT_Locations$Jobs,0)
TT_Locations$Jobs_75 <- ifelse (TT_Locations$Time<75, TT_Locations$Jobs,0)
TT_Locations$Jobs_80 <- ifelse (TT_Locations$Time<80, TT_Locations$Jobs,0)
TT_Locations$Jobs_85 <- ifelse (TT_Locations$Time<85, TT_Locations$Jobs,0)
TT_Locations$Jobs_90 <- ifelse (TT_Locations$Time<90, TT_Locations$Jobs,0)
TT_Locations$Jobs_95 <- ifelse (TT_Locations$Time<95, TT_Locations$Jobs,0)
TT_Locations$Jobs_100 <- ifelse (TT_Locations$Time<100, TT_Locations$Jobs,0)
TT_Locations$Jobs_105 <- ifelse (TT_Locations$Time<105, TT_Locations$Jobs,0)
TT_Locations$Jobs_110 <- ifelse (TT_Locations$Time<110, TT_Locations$Jobs,0)
TT_Locations$Jobs_115 <- ifelse (TT_Locations$Time<115, TT_Locations$Jobs,0)
TT_Locations$Jobs_120 <- ifelse (TT_Locations$Time<120, TT_Locations$Jobs,0)

#Adding number of jobs accessible based on origin 
Jobs_5 <- aggregate( Jobs_5 ~ From, data = TT_Locations, sum)
Jobs_10 <- aggregate( Jobs_10 ~ From, data = TT_Locations, sum)
Jobs_15 <- aggregate( Jobs_15 ~ From, data = TT_Locations, sum)
Jobs_20 <- aggregate( Jobs_20 ~ From, data = TT_Locations, sum)
Jobs_25 <- aggregate( Jobs_25 ~ From, data = TT_Locations, sum)
Jobs_30 <- aggregate( Jobs_30 ~ From, data = TT_Locations, sum)
Jobs_35 <- aggregate( Jobs_35 ~ From, data = TT_Locations, sum)
Jobs_40 <- aggregate( Jobs_40 ~ From, data = TT_Locations, sum)
Jobs_45 <- aggregate( Jobs_45 ~ From, data = TT_Locations, sum)
Jobs_50 <- aggregate( Jobs_50 ~ From, data = TT_Locations, sum)
Jobs_55 <- aggregate( Jobs_55 ~ From, data = TT_Locations, sum)
Jobs_60 <- aggregate( Jobs_60 ~ From, data = TT_Locations, sum)
Jobs_65 <- aggregate( Jobs_65 ~ From, data = TT_Locations, sum)
Jobs_70 <- aggregate( Jobs_70 ~ From, data = TT_Locations, sum)
Jobs_75 <- aggregate( Jobs_75 ~ From, data = TT_Locations, sum)
Jobs_80 <- aggregate( Jobs_80 ~ From, data = TT_Locations, sum)
Jobs_85 <- aggregate( Jobs_85 ~ From, data = TT_Locations, sum)
Jobs_90 <- aggregate( Jobs_90 ~ From, data = TT_Locations, sum)
Jobs_95 <- aggregate( Jobs_95 ~ From, data = TT_Locations, sum)
Jobs_100 <- aggregate( Jobs_100 ~ From, data = TT_Locations, sum)
Jobs_105 <- aggregate( Jobs_105 ~ From, data = TT_Locations, sum)
Jobs_110 <- aggregate( Jobs_110 ~ From, data = TT_Locations, sum)
Jobs_115 <- aggregate( Jobs_115 ~ From, data = TT_Locations, sum)
Jobs_120 <- aggregate( Jobs_120 ~ From, data = TT_Locations, sum)

Accessibility_Jobs <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "From"), 
                             list(Jobs_5, Jobs_10, Jobs_15, Jobs_20, Jobs_25, 
                                  Jobs_30, Jobs_35, Jobs_40, Jobs_45, Jobs_50, 
                                  Jobs_55, Jobs_60, Jobs_65, Jobs_70, Jobs_75,
                                  Jobs_80, Jobs_85, Jobs_90, Jobs_95, Jobs_100,
                                  Jobs_105, Jobs_110, Jobs_115, Jobs_120))

Accessibility_Jobs_time <- rename(Accessibility_Jobs,  '5' = 'Jobs_5', '10' = 'Jobs_10',
                   '15' = 'Jobs_15', '20' = 'Jobs_20', '25' = 'Jobs_25', '30' = 'Jobs_30',
                   '35' = 'Jobs_35', '40' = 'Jobs_40', '45' = 'Jobs_45', '50' = 'Jobs_50',
                   '55' = 'Jobs_55', '60' = 'Jobs_60', '65' = 'Jobs_65', '70' = 'Jobs_70',
                   '75' = 'Jobs_75', '80' = 'Jobs_80', '85' = 'Jobs_85', '90' = 'Jobs_90',
                   '95' = 'Jobs_95', '100' = 'Jobs_100', '105' = 'Jobs_105', '110' = 'Jobs_110',
                   '115' = 'Jobs_115', '120' = 'Jobs_120')

write.csv(Accessibility_Jobs_time,"Outputs/access_unweighted.csv")

rm(Locations, Accessibility_Jobs, Accessibility_Jobs_time, 
   Jobs_5, Jobs_10, Jobs_15, Jobs_20, Jobs_25, 
   Jobs_30, Jobs_35, Jobs_40, Jobs_45, Jobs_50, 
   Jobs_55, Jobs_60, Jobs_65, Jobs_70, Jobs_75,
   Jobs_80, Jobs_85, Jobs_90, Jobs_95, Jobs_100,
   Jobs_105, Jobs_110, Jobs_115, Jobs_120)

# Low-income jobs
Locations <- read.csv("Outputs/all_jobs_montreal.csv", colClasses = "character")
names(Locations)[names(Locations)=='jobs.lower.inc'] <- 'Jobs'

# Merge jobs with travel time 
TT_Locations <- merge(TT, Locations, by.x = "To",by.y="id")
TT_Locations$Jobs <- as.numeric(TT_Locations$Jobs)

# Counting jobs only if travel time is lower than XX minutes
TT_Locations$Jobs_5 <- ifelse (TT_Locations$Time<5, TT_Locations$Jobs,0)
TT_Locations$Jobs_10 <- ifelse (TT_Locations$Time<10, TT_Locations$Jobs,0)
TT_Locations$Jobs_15 <- ifelse (TT_Locations$Time<15, TT_Locations$Jobs,0)
TT_Locations$Jobs_20 <- ifelse (TT_Locations$Time<20, TT_Locations$Jobs,0)
TT_Locations$Jobs_25 <- ifelse (TT_Locations$Time<25, TT_Locations$Jobs,0)
TT_Locations$Jobs_30 <- ifelse (TT_Locations$Time<30, TT_Locations$Jobs,0)
TT_Locations$Jobs_35 <- ifelse (TT_Locations$Time<35, TT_Locations$Jobs,0)
TT_Locations$Jobs_40 <- ifelse (TT_Locations$Time<40, TT_Locations$Jobs,0)
TT_Locations$Jobs_45 <- ifelse (TT_Locations$Time<45, TT_Locations$Jobs,0)
TT_Locations$Jobs_50 <- ifelse (TT_Locations$Time<50, TT_Locations$Jobs,0)
TT_Locations$Jobs_55 <- ifelse (TT_Locations$Time<55, TT_Locations$Jobs,0)
TT_Locations$Jobs_60 <- ifelse (TT_Locations$Time<60, TT_Locations$Jobs,0)
TT_Locations$Jobs_65 <- ifelse (TT_Locations$Time<65, TT_Locations$Jobs,0)
TT_Locations$Jobs_70 <- ifelse (TT_Locations$Time<70, TT_Locations$Jobs,0)
TT_Locations$Jobs_75 <- ifelse (TT_Locations$Time<75, TT_Locations$Jobs,0)
TT_Locations$Jobs_80 <- ifelse (TT_Locations$Time<80, TT_Locations$Jobs,0)
TT_Locations$Jobs_85 <- ifelse (TT_Locations$Time<85, TT_Locations$Jobs,0)
TT_Locations$Jobs_90 <- ifelse (TT_Locations$Time<90, TT_Locations$Jobs,0)
TT_Locations$Jobs_95 <- ifelse (TT_Locations$Time<95, TT_Locations$Jobs,0)
TT_Locations$Jobs_100 <- ifelse (TT_Locations$Time<100, TT_Locations$Jobs,0)
TT_Locations$Jobs_105 <- ifelse (TT_Locations$Time<105, TT_Locations$Jobs,0)
TT_Locations$Jobs_110 <- ifelse (TT_Locations$Time<110, TT_Locations$Jobs,0)
TT_Locations$Jobs_115 <- ifelse (TT_Locations$Time<115, TT_Locations$Jobs,0)
TT_Locations$Jobs_120 <- ifelse (TT_Locations$Time<120, TT_Locations$Jobs,0)

#Adding number of jobs accessible based on origin 
Jobs_5 <- aggregate( Jobs_5 ~ From, data = TT_Locations, sum)
Jobs_10 <- aggregate( Jobs_10 ~ From, data = TT_Locations, sum)
Jobs_15 <- aggregate( Jobs_15 ~ From, data = TT_Locations, sum)
Jobs_20 <- aggregate( Jobs_20 ~ From, data = TT_Locations, sum)
Jobs_25 <- aggregate( Jobs_25 ~ From, data = TT_Locations, sum)
Jobs_30 <- aggregate( Jobs_30 ~ From, data = TT_Locations, sum)
Jobs_35 <- aggregate( Jobs_35 ~ From, data = TT_Locations, sum)
Jobs_40 <- aggregate( Jobs_40 ~ From, data = TT_Locations, sum)
Jobs_45 <- aggregate( Jobs_45 ~ From, data = TT_Locations, sum)
Jobs_50 <- aggregate( Jobs_50 ~ From, data = TT_Locations, sum)
Jobs_55 <- aggregate( Jobs_55 ~ From, data = TT_Locations, sum)
Jobs_60 <- aggregate( Jobs_60 ~ From, data = TT_Locations, sum)
Jobs_65 <- aggregate( Jobs_65 ~ From, data = TT_Locations, sum)
Jobs_70 <- aggregate( Jobs_70 ~ From, data = TT_Locations, sum)
Jobs_75 <- aggregate( Jobs_75 ~ From, data = TT_Locations, sum)
Jobs_80 <- aggregate( Jobs_80 ~ From, data = TT_Locations, sum)
Jobs_85 <- aggregate( Jobs_85 ~ From, data = TT_Locations, sum)
Jobs_90 <- aggregate( Jobs_90 ~ From, data = TT_Locations, sum)
Jobs_95 <- aggregate( Jobs_95 ~ From, data = TT_Locations, sum)
Jobs_100 <- aggregate( Jobs_100 ~ From, data = TT_Locations, sum)
Jobs_105 <- aggregate( Jobs_105 ~ From, data = TT_Locations, sum)
Jobs_110 <- aggregate( Jobs_110 ~ From, data = TT_Locations, sum)
Jobs_115 <- aggregate( Jobs_115 ~ From, data = TT_Locations, sum)
Jobs_120 <- aggregate( Jobs_120 ~ From, data = TT_Locations, sum)

Accessibility_Jobs <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "From"), 
                             list(Jobs_5, Jobs_10, Jobs_15, Jobs_20, Jobs_25, 
                                  Jobs_30, Jobs_35, Jobs_40, Jobs_45, Jobs_50, 
                                  Jobs_55, Jobs_60, Jobs_65, Jobs_70, Jobs_75,
                                  Jobs_80, Jobs_85, Jobs_90, Jobs_95, Jobs_100,
                                  Jobs_105, Jobs_110, Jobs_115, Jobs_120))

Accessibility_Jobs_time <- rename(Accessibility_Jobs,  '5' = 'Jobs_5', '10' = 'Jobs_10',
                                  '15' = 'Jobs_15', '20' = 'Jobs_20', '25' = 'Jobs_25', '30' = 'Jobs_30',
                                  '35' = 'Jobs_35', '40' = 'Jobs_40', '45' = 'Jobs_45', '50' = 'Jobs_50',
                                  '55' = 'Jobs_55', '60' = 'Jobs_60', '65' = 'Jobs_65', '70' = 'Jobs_70',
                                  '75' = 'Jobs_75', '80' = 'Jobs_80', '85' = 'Jobs_85', '90' = 'Jobs_90',
                                  '95' = 'Jobs_95', '100' = 'Jobs_100', '105' = 'Jobs_105', '110' = 'Jobs_110',
                                  '115' = 'Jobs_115', '120' = 'Jobs_120')

write.csv(Accessibility_Jobs_time,"Outputs/access_unweighted_lower.csv")

rm(Accessibility_Jobs, Accessibility_Jobs_time, 
   Jobs_5, Jobs_10, Jobs_15, Jobs_20, Jobs_25, 
   Jobs_30, Jobs_35, Jobs_40, Jobs_45, Jobs_50, 
   Jobs_55, Jobs_60, Jobs_65, Jobs_70, Jobs_75,
   Jobs_80, Jobs_85, Jobs_90, Jobs_95, Jobs_100,
   Jobs_105, Jobs_110, Jobs_115, Jobs_120)

# High-income jobs
Locations <- read.csv("Outputs/all_jobs_montreal.csv", colClasses = "character")
names(Locations)[names(Locations)=='jobs.higher.inc'] <- 'Jobs'

# Merge jobs with travel time 
TT_Locations <- merge(TT, Locations, by.x = "To",by.y="id")
TT_Locations$Jobs <- as.numeric(TT_Locations$Jobs)

# Counting jobs only if travel time is lower than XX minutes
TT_Locations$Jobs_5 <- ifelse (TT_Locations$Time<5, TT_Locations$Jobs,0)
TT_Locations$Jobs_10 <- ifelse (TT_Locations$Time<10, TT_Locations$Jobs,0)
TT_Locations$Jobs_15 <- ifelse (TT_Locations$Time<15, TT_Locations$Jobs,0)
TT_Locations$Jobs_20 <- ifelse (TT_Locations$Time<20, TT_Locations$Jobs,0)
TT_Locations$Jobs_25 <- ifelse (TT_Locations$Time<25, TT_Locations$Jobs,0)
TT_Locations$Jobs_30 <- ifelse (TT_Locations$Time<30, TT_Locations$Jobs,0)
TT_Locations$Jobs_35 <- ifelse (TT_Locations$Time<35, TT_Locations$Jobs,0)
TT_Locations$Jobs_40 <- ifelse (TT_Locations$Time<40, TT_Locations$Jobs,0)
TT_Locations$Jobs_45 <- ifelse (TT_Locations$Time<45, TT_Locations$Jobs,0)
TT_Locations$Jobs_50 <- ifelse (TT_Locations$Time<50, TT_Locations$Jobs,0)
TT_Locations$Jobs_55 <- ifelse (TT_Locations$Time<55, TT_Locations$Jobs,0)
TT_Locations$Jobs_60 <- ifelse (TT_Locations$Time<60, TT_Locations$Jobs,0)
TT_Locations$Jobs_65 <- ifelse (TT_Locations$Time<65, TT_Locations$Jobs,0)
TT_Locations$Jobs_70 <- ifelse (TT_Locations$Time<70, TT_Locations$Jobs,0)
TT_Locations$Jobs_75 <- ifelse (TT_Locations$Time<75, TT_Locations$Jobs,0)
TT_Locations$Jobs_80 <- ifelse (TT_Locations$Time<80, TT_Locations$Jobs,0)
TT_Locations$Jobs_85 <- ifelse (TT_Locations$Time<85, TT_Locations$Jobs,0)
TT_Locations$Jobs_90 <- ifelse (TT_Locations$Time<90, TT_Locations$Jobs,0)
TT_Locations$Jobs_95 <- ifelse (TT_Locations$Time<95, TT_Locations$Jobs,0)
TT_Locations$Jobs_100 <- ifelse (TT_Locations$Time<100, TT_Locations$Jobs,0)
TT_Locations$Jobs_105 <- ifelse (TT_Locations$Time<105, TT_Locations$Jobs,0)
TT_Locations$Jobs_110 <- ifelse (TT_Locations$Time<110, TT_Locations$Jobs,0)
TT_Locations$Jobs_115 <- ifelse (TT_Locations$Time<115, TT_Locations$Jobs,0)
TT_Locations$Jobs_120 <- ifelse (TT_Locations$Time<120, TT_Locations$Jobs,0)

#Adding number of jobs accessible based on origin 
Jobs_5 <- aggregate( Jobs_5 ~ From, data = TT_Locations, sum)
Jobs_10 <- aggregate( Jobs_10 ~ From, data = TT_Locations, sum)
Jobs_15 <- aggregate( Jobs_15 ~ From, data = TT_Locations, sum)
Jobs_20 <- aggregate( Jobs_20 ~ From, data = TT_Locations, sum)
Jobs_25 <- aggregate( Jobs_25 ~ From, data = TT_Locations, sum)
Jobs_30 <- aggregate( Jobs_30 ~ From, data = TT_Locations, sum)
Jobs_35 <- aggregate( Jobs_35 ~ From, data = TT_Locations, sum)
Jobs_40 <- aggregate( Jobs_40 ~ From, data = TT_Locations, sum)
Jobs_45 <- aggregate( Jobs_45 ~ From, data = TT_Locations, sum)
Jobs_50 <- aggregate( Jobs_50 ~ From, data = TT_Locations, sum)
Jobs_55 <- aggregate( Jobs_55 ~ From, data = TT_Locations, sum)
Jobs_60 <- aggregate( Jobs_60 ~ From, data = TT_Locations, sum)
Jobs_65 <- aggregate( Jobs_65 ~ From, data = TT_Locations, sum)
Jobs_70 <- aggregate( Jobs_70 ~ From, data = TT_Locations, sum)
Jobs_75 <- aggregate( Jobs_75 ~ From, data = TT_Locations, sum)
Jobs_80 <- aggregate( Jobs_80 ~ From, data = TT_Locations, sum)
Jobs_85 <- aggregate( Jobs_85 ~ From, data = TT_Locations, sum)
Jobs_90 <- aggregate( Jobs_90 ~ From, data = TT_Locations, sum)
Jobs_95 <- aggregate( Jobs_95 ~ From, data = TT_Locations, sum)
Jobs_100 <- aggregate( Jobs_100 ~ From, data = TT_Locations, sum)
Jobs_105 <- aggregate( Jobs_105 ~ From, data = TT_Locations, sum)
Jobs_110 <- aggregate( Jobs_110 ~ From, data = TT_Locations, sum)
Jobs_115 <- aggregate( Jobs_115 ~ From, data = TT_Locations, sum)
Jobs_120 <- aggregate( Jobs_120 ~ From, data = TT_Locations, sum)

Accessibility_Jobs <- Reduce(function(x, y) merge(x, y, all=TRUE, by = "From"), 
                             list(Jobs_5, Jobs_10, Jobs_15, Jobs_20, Jobs_25, 
                                  Jobs_30, Jobs_35, Jobs_40, Jobs_45, Jobs_50, 
                                  Jobs_55, Jobs_60, Jobs_65, Jobs_70, Jobs_75,
                                  Jobs_80, Jobs_85, Jobs_90, Jobs_95, Jobs_100,
                                  Jobs_105, Jobs_110, Jobs_115, Jobs_120))

Accessibility_Jobs_time <- rename(Accessibility_Jobs,  '5' = 'Jobs_5', '10' = 'Jobs_10',
                                  '15' = 'Jobs_15', '20' = 'Jobs_20', '25' = 'Jobs_25', '30' = 'Jobs_30',
                                  '35' = 'Jobs_35', '40' = 'Jobs_40', '45' = 'Jobs_45', '50' = 'Jobs_50',
                                  '55' = 'Jobs_55', '60' = 'Jobs_60', '65' = 'Jobs_65', '70' = 'Jobs_70',
                                  '75' = 'Jobs_75', '80' = 'Jobs_80', '85' = 'Jobs_85', '90' = 'Jobs_90',
                                  '95' = 'Jobs_95', '100' = 'Jobs_100', '105' = 'Jobs_105', '110' = 'Jobs_110',
                                  '115' = 'Jobs_115', '120' = 'Jobs_120')

write.csv(Accessibility_Jobs_time,"Outputs/access_unweighted_higher.csv")

rm(Accessibility_Jobs, Accessibility_Jobs_time, 
   Jobs_5, Jobs_10, Jobs_15, Jobs_20, Jobs_25, 
   Jobs_30, Jobs_35, Jobs_40, Jobs_45, Jobs_50, 
   Jobs_55, Jobs_60, Jobs_65, Jobs_70, Jobs_75,
   Jobs_80, Jobs_85, Jobs_90, Jobs_95, Jobs_100,
   Jobs_105, Jobs_110, Jobs_115, Jobs_120)

### Jobs accessible by a car prep ==============================================


fl <- read.csv('Inputs/car_van_truck_inc.csv')
colnames(fl)
fl_slim0 <- subset(fl, select = -c(?..GeoFile, 
                                   GeoText, 
                                   GeoGNR., 
                                   PGeoFile, 
                                   PGeoText, 
                                   PGeoGNR.,
                                   Median.total.income....)) %>% filter(CMA_CA_Code == 462, 
                                                                        GeoLevel == 'CT', 
                                                                        PCMA_CA_Code == 462, 
                                                                        PGeoLevel == 'CT')
head(fl_slim0)
rm(fl)

fl_slim1 <- rename(fl_slim0,  'total' = 'Total...Total.income.in.2015',
                   'under.5k' = 'Under..5.000',
                   '5k.and.9.99k' = 'X.5.000.to..9.999',
                   '10k.and.14.99k' = 'X.10.000.to..14.999',
                   '15k.and.19.99k' = 'X.15.000.to..19.999',
                   '20k.and.29.99k' = 'X.20.000.to..29.999',
                   '30k.and.39.99k' = 'X.30.000.to..39.999',
                   '40k.and.49.99k' = 'X.40.000.to..49.999',
                   '50k.and.59.99k' = 'X.50.000.to..59.999',
                   '60k.and.79.99k' = 'X.60.000.to..79.999',
                   '80K.and.99.99k' = 'X.80.000.to..99.999',
                   '100k.and.plus' = 'X.100.000.and.over')

rm(fl_slim0)

fl_slim2 <- subset(fl_slim1, select = -c(CMA_CA_Code, 
                                         GeoLevel, 
                                         PCMA_CA_Code, 
                                         PGeoLevel)) %>% filter(total != 'X')

head(fl_slim2)
rm(fl_slim1)

fl_slim2$origin <- ifelse(nchar(fl_slim2$GeoCode)==7,paste0(fl_slim2$GeoCode,'.00'),fl_slim2$GeoCode)
fl_slim2$destination <- ifelse(nchar(fl_slim2$PGeoCode)==7,paste0(fl_slim2$PGeoCode,'.00'),fl_slim2$PGeoCode)


fl_slim2$od <- paste0(fl_slim2$origin, '-', fl_slim2$destination)
str(fl_slim2)

fl_slim3 <- fl_slim2 %>% mutate_at(c('total',
                                     'under.5k',
                                     '5k.and.9.99k',
                                     '10k.and.14.99k',
                                     '15k.and.19.99k',
                                     '20k.and.29.99k',
                                     '30k.and.39.99k',
                                     '40k.and.49.99k',
                                     '50k.and.59.99k',
                                     '60k.and.79.99k',
                                     '80K.and.99.99k',
                                     '100k.and.plus'), as.numeric)

str(fl_slim3)

rm(fl_slim2)

fl_slim3$lower.inc <- (fl_slim3$`under.5k` + 
                         fl_slim3$`5k.and.9.99k` + 
                         fl_slim3$`10k.and.14.99k` + 
                         fl_slim3$`15k.and.19.99k` + 
                         fl_slim3$`20k.and.29.99k`)

fl_slim3$higher.inc <-(fl_slim3$`40k.and.49.99k` + 
                         fl_slim3$`50k.and.59.99k` + 
                         fl_slim3$`60k.and.79.99k` + 
                         fl_slim3$`80K.and.99.99` + 
                         fl_slim3$`100k.and.plus`)

head(fl_slim3)
fl_slim4 <- subset(fl_slim3, select = c(od, total, lower.inc, higher.inc))
head(fl_slim4)
rm(fl_slim3)

fl_slim4 <- subset(fl_slim4, select=-c(X))

write.csv(fl_slim4,"Outputs/flows_mtl_clean_car_van_truck_montreal.csv")

### Join travel time with flows ================================================

ttm_8_9am_car <- read.csv('Outputs/ttm_8_9am_car.csv', header = TRUE, na.strings = c("","NA"))
ttm_8_9am_car <- subset(ttm_8_9am_car, select=-c(X))

flows_mtl_clean_car_van_truck <- read.csv('Outputs/flows_mtl_clean_car_van_truck.csv', header = TRUE, na.strings = c("","NA"))
flows_mtl_clean_car_van_truck <- subset(flows_mtl_clean_car_van_truck, select=-c(X))
flows_mtl_clean_car_van_truck <- na.omit(flows_mtl_clean_car_van_truck)
sum(is.na(flows_mtl_clean_car_van_truck$total))


# Join
ttm.fl <- left_join(flows_mtl_clean_car_van_truck, ttm_8_9am_car, by = 'od')
ttm.fl.clean <- ttm.fl %>% filter(travel_time >0)
head(ttm.fl)
rm(flows_mtl_clean_car_van_truck, ttm_8_9am_car, ttm.fl)


# All flows
mean.time <- sum(ttm.fl.clean$travel_time*ttm.fl.clean$total)/sum(ttm.fl.clean$total)
ttm.fl.long <- uncount(ttm.fl.clean, weights = total, .remove = TRUE)
mean.time.long <- mean(ttm.fl.long$travel_time)
head(ttm.fl.clean,15)
ttm.fl.long <- subset(ttm.fl.long, select = -c(lower.inc, higher.inc))
ttm.fl.long <- subset(ttm.fl.long, travel_time >=5)

print(mean(ttm.fl.long$travel_time))
print(min(ttm.fl.long$travel_time))
print(max(ttm.fl.long$travel_time))

head(ttm.fl.long,25)

# Lower-income flows
l.ttm.fl.clean <- subset(ttm.fl.clean, lower.inc > 0)
str(l.ttm.fl.clean)
l.mean.time <- sum(l.ttm.fl.clean$travel_time*l.ttm.fl.clean$lower.inc)/sum(l.ttm.fl.clean$lower.inc)

l.ttm.fl.long <- uncount(l.ttm.fl.clean, weights = lower.inc, .remove = TRUE)
l.mean.time.long <- mean(l.ttm.fl.long$travel_time)
head(l.ttm.fl.clean,15)
l.ttm.fl.long <- subset(l.ttm.fl.long, select = -c(total, higher.inc))
head(l.ttm.fl.long,25)

# Higher-income flows
h.ttm.fl.clean <- subset(ttm.fl.clean, higher.inc > 0)
str(h.ttm.fl.clean)
h.mean.time <- sum(h.ttm.fl.clean$travel_time*h.ttm.fl.clean$higher.inc)/sum(h.ttm.fl.clean$higher.inc)

h.ttm.fl.long <- uncount(h.ttm.fl.clean, weights = higher.inc, .remove = TRUE)
h.mean.time.long <- mean(h.ttm.fl.long$travel_time)
head(h.ttm.fl.clean)
h.ttm.fl.long <- subset(h.ttm.fl.long, select = -c(total, lower.inc))
head(h.ttm.fl.long)

write.csv(ttm.fl.long, "Outputs/all_flows_time_car.csv")
write.csv(l.ttm.fl.long, "Outputs/l_inc_flows_time_car.csv")
write.csv(h.ttm.fl.long, "Outputs/h_inc_flows_time_car.csv")

### Parameters for gravity models ==============================================

ttm.fl.long <- read.csv('Outputs/all_flows_time_car.csv', header = TRUE, na.strings = c("","NA"))

# All flows

## check distribution
ggplot(data = ttm.fl.long, 
       aes(travel_time)) + 
  geom_histogram(binwidth=1)

bins <- seq(0, 101, by=1)

ttm.fl.long$bin <- cut(ttm.fl.long$travel_time, 
                       bins, 
                       right = TRUE)

flows.by.time <- data.table(table(ttm.fl.long$bin))
flows.by.time$mins <- bins[-length(bins)]
flows.by.time <- subset(flows.by.time, mins >=5)

flows.by.time <- rename(flows.by.time, trips = N,
                        bin = V1)
head(flows.by.time)

flows.by.time$perc_flows <- flows.by.time$trips/sum(flows.by.time$trips)*100
flows.by.time$std_flows <- flows.by.time$perc_flows/max(flows.by.time$perc_flows)
head(flows.by.time)

f1 <- ggplot(flows.by.time, 
             aes(x = mins, y = std_flows)) + 
  geom_point(color='#0D5C91') +
  labs(y="Normalized Work Trips Weight Factor",
       x="Trip duration (mins)")

f1 

flows.by.time$cumflows <- 1- cumsum(flows.by.time$trips)/sum(flows.by.time$trips)
f2 <- ggplot(flows.by.time, aes(x = mins, y = cumflows)) + geom_point(color='#E27F08') +
  labs(y="Normalized Work Trips Weight Factor",
       x="Trip duration (mins)")
f2

# fixed negative exponential decay function estimation - PDF
model1 <- nls(std_flows ~ exp(S * mins), 
              data = flows.by.time,
              start = list(S = -0.01),
              #            algorithm = default is Gaus-Newton; same results using Port
              nls.control(maxiter = 100))
summary(model1)

fit.1 <- lm(std_flows ~ predict(model1), 
            data = flows.by.time) %>% broom::glance()

coef.1 <- coef(model1)
modelr::rsquare(model1, flows.by.time)

flows.by.time$predic_m1 <- predict(model1, flows.by.time)
head(flows.by.time)


head(flows.by.time)
f3 <- (f1+ geom_line(data = flows.by.time, 
                     aes(x = mins, y = predic_m1), 
                     size=1,
                     color = "black") + 
         labs(y="Normalized Work Trips Weight Factor",
              x="Trip duration (mins)"))

fit.1

f3 + 
  annotate('text', x=75, 
           y=.90,
           size = 10,
           fontface = 'bold',
           label= "Y == e ^ {-0.0294 * X}", 
           parse = TRUE) + theme(text = element_text(size = 20))


ggsave("Outputs/fig1a.jpg")

model1 ##

# fixed negative exponential decay function estimation - CDF
model2 <- nls(cumflows ~ exp(S * mins), 
              data = flows.by.time,
              start = list(S = -0.01),
              #            algorithm = default is Gaus-Newton; same results using Port
              nls.control(maxiter = 100))
summary(model2)
coef.2 <- coef(model2)
modelr::rsquare(model2, flows.by.time)

flows.by.time$predic_m2 <- predict(model2, flows.by.time)
head(flows.by.time)
f4 <- (f2+ geom_line(data = flows.by.time, 
                     aes(x = mins, y = predic_m2), 
                     color = "black"))

f4 + annotate('text', x=75, 
              y=.90,
              size = 10,
              fontface = 'bold',
              label= "Y == e ^ {-0.0372 * X}",
              parse = TRUE) + theme(text = element_text(size = 20)) 


ggsave("Outputs/fig1b.jpg")

model2
model1

# Gaussian function estimation - CDF
model3 <- nls(cumflows ~ exp(S * mins ^ 2),
              data = flows.by.time,
              start = list(S = -0.01),
              nls.control(maxiter = 100))
summary(model3)
coef.3 <- coef(model3)
modelr::rsquare(model3, flows.by.time)

flows.by.time$predic_m3 <- predict(model3, flows.by.time)
head(flows.by.time)
f4 <- (f2+ geom_line(data = flows.by.time, 
                     aes(x = mins, y = predic_m3), 
                     color = "black"))

f4 + annotate('text', x=75, 
              y=.90,
              size = 10,
              fontface = 'bold',
              label= "Y == e ^ {-0.00121*X^2}",
              parse = TRUE) + theme(text = element_text(size = 20)) 

ggsave("Outputs/fig1c.jpg")


# Log-Logistic (Fisk) function estimation - CDF
median <- median(ttm.fl.long$travel_time) #18 minutes

model4 <- nls(cumflows ~ 1/(1 + (mins/median) ^ S),
              data = flows.by.time,
              start = list(S = -0.01),
              nls.control(maxiter = 100))
summary(model4)
coef.4 <- coef(model4)
modelr::rsquare(model4, flows.by.time)

flows.by.time$predic_m4 <- predict(model4, flows.by.time)
head(flows.by.time)
f5 <- (f2+ geom_line(data = flows.by.time, 
                     aes(x = mins, y = predic_m4), 
                     color = "black"))

f5 + annotate('text', x=73, 
              y=.90,
              size = 9,
              label= "Y == 1 / 1 + {(X / p50)} ^ 4.56",
              parse = TRUE) + theme(text = element_text(size = 20)) 

ggsave("Outputs/fig1d.jpg")


# Logistic function estimation - CDF
mean(ttm.fl.long$travel_time) # 30.29
median(ttm.fl.long$travel_time) # 29.38
sd(ttm.fl.long$travel_time) # 9.61

# Low-income flows

l.ttm.fl.long <- read.csv('Outputs/l_inc_flows_time_car.csv', header = TRUE, na.strings = c("","NA"))

## check distribution

ggplot(data = l.ttm.fl.long, 
       aes(travel_time)) + 
  geom_histogram(binwidth=1)

bins <- seq(0, 101, by=1)

l.ttm.fl.long$bin <- cut(l.ttm.fl.long$travel_time, 
                         bins, 
                         right = TRUE)

l.flows.by.time <- data.table(table(l.ttm.fl.long$bin))
l.flows.by.time$mins <- bins[-length(bins)]
l.flows.by.time <- rename(l.flows.by.time, trips = N,
                          bin = V1)

l.flows.by.time <- subset(l.flows.by.time, mins >=5)

head(l.flows.by.time)

l.flows.by.time$perc_flows <- l.flows.by.time$trips/sum(l.flows.by.time$trips)*100
l.flows.by.time$std_flows <- l.flows.by.time$perc_flows/max(l.flows.by.time$perc_flows)
head(l.flows.by.time)

f1.1 <- ggplot(l.flows.by.time, aes(x = mins, y = std_flows)) + geom_point()
f1.1

l.flows.by.time$cumflows <- 1- cumsum(l.flows.by.time$trips)/sum(l.flows.by.time$trips)
f2.1 <- ggplot(l.flows.by.time, aes(x = mins, y = cumflows)) + geom_point()
f2

## fixed negative exponential decay function estimation - PDF
model1.1 <- nls(std_flows ~ exp(S * mins), 
                data = l.flows.by.time,
                start = list(S = -0.01),
                #            algorithm = default is Gaus-Newton; same results using Port
                nls.control(maxiter = 100))
summary(model1.1)
coef.1.1 <- coef(model1.1)

l.flows.by.time$predic_m1.1 <- predict(model1.1, l.flows.by.time)
head(l.flows.by.time)
f3 <- (f1+ geom_line(data = l.flows.by.time, 
                     aes(x = mins, y = predic_m1.1), 
                     color = "red"))
f3
model1.1 ##

## fixed negative exponential decay function estimation - CDF
model2.1 <- nls(cumflows ~ exp(S * mins), 
                data = l.flows.by.time,
                start = list(S = -0.01),
                nls.control(maxiter = 100))
summary(model2.1)
coef.2.1 <- coef(model2.1)

l.flows.by.time$predic_m2.1 <- predict(model2.1, l.flows.by.time)
head(l.flows.by.time)
f4 <- (f2+ geom_line(data = l.flows.by.time, 
                     aes(x = mins, y = predic_m2.1), 
                     color = "blue"))
f4
model2.1
model1.1

## Gaussian function estimation - CDF
model3.1 <- nls(cumflows ~ exp(S * mins ^ 2),
                data = l.flows.by.time,
                start = list(S = -0.01),
                nls.control(maxiter = 100))
summary(model3.1)
coef.3.1 <- coef(model3.1)

l.flows.by.time$predic_m3.1 <- predict(model3.1, l.flows.by.time)
head(l.flows.by.time)
f4 <- (f2+ geom_line(data = l.flows.by.time, 
                     aes(x = mins, y = predic_m3.1), 
                     color = "green"))
f4

## Log-Logistic (Fisk) function estimation - CDF
median <- median(l.ttm.fl.long$travel_time) #44 minutes

model4.1 <- nls(cumflows ~ 1/(1 + (mins/median) ^ S),
                data = l.flows.by.time,
                start = list(S = -0.01),
                nls.control(maxiter = 100))
summary(model4.1)
coef.4.1 <- coef(model4.1)

l.flows.by.time$predic_m4.1 <- predict(model4.1, l.flows.by.time)
head(l.flows.by.time)
f5 <- (f2+ geom_line(data = l.flows.by.time, 
                     aes(x = mins, y = predic_m4.1), 
                     color = "purple"))
f5

## Logistic function estimation - CDF
mean(l.ttm.fl.long$travel_time) # 19.35
sd(l.ttm.fl.long$travel_time) # 7.9

# High-income flows

h.ttm.fl.long <- read.csv('Outputs/h_inc_flows_time_car.csv', header = TRUE, na.strings = c("","NA"))

## check distribution

ggplot(data = h.ttm.fl.long, 
       aes(travel_time)) + 
  geom_histogram(binwidth=1)

bins <- seq(0, 101, by=1)

h.ttm.fl.long$bin <- cut(h.ttm.fl.long$travel_time, 
                         bins, 
                         right = TRUE)

h.flows.by.time <- data.table(table(h.ttm.fl.long$bin))
h.flows.by.time$mins <- bins[-length(bins)]
h.flows.by.time <- rename(h.flows.by.time, trips = N,
                          bin = V1)

h.flows.by.time <- subset(h.flows.by.time, mins >=5)
head(h.flows.by.time)

h.flows.by.time$perc_flows <- h.flows.by.time$trips/sum(h.flows.by.time$trips)*100
h.flows.by.time$std_flows <- h.flows.by.time$perc_flows/max(h.flows.by.time$perc_flows)
head(h.flows.by.time)

f1 <- ggplot(h.flows.by.time, aes(x = mins, y = std_flows)) + geom_point()
f1

h.flows.by.time$cumflows <- 1- cumsum(h.flows.by.time$trips)/sum(h.flows.by.time$trips)
f2 <- ggplot(h.flows.by.time, aes(x = mins, y = cumflows)) + geom_point()
f2

## fixed negative exponential decay function estimation - PDF
model1.2 <- nls(std_flows ~ exp(S * mins), 
                data = h.flows.by.time,
                start = list(S = -0.01),
                #            algorithm = default is Gaus-Newton; same results using Port
                nls.control(maxiter = 100))
summary(model1.2)
coef.1.2 <- coef(model1.2)

h.flows.by.time$predic_m1.2 <- predict(model1.2, h.flows.by.time)
head(h.flows.by.time)
f3 <- (f1+ geom_line(data = h.flows.by.time, 
                     aes(x = mins, y = predic_m1.2), 
                     color = "red"))
f3
model1.2 ##

## fixed negative exponential decay function estimation - CDF
model2.2 <- nls(cumflows ~ exp(S * mins), 
                data = h.flows.by.time,
                start = list(S = -0.01),
                #            algorithm = default is Gaus-Newton; same results using Port
                nls.control(maxiter = 100))
summary(model2.2)
coef.2.2 <- coef(model2.2)

h.flows.by.time$predic_m2.2 <- predict(model2.2, h.flows.by.time)
head(h.flows.by.time)
f4 <- (f2+ geom_line(data = h.flows.by.time, 
                     aes(x = mins, y = predic_m2.2), 
                     color = "blue"))
f4
model2.2
model1.2

## Gaussian function estimation - CDF
model3.2 <- nls(cumflows ~ exp(S * mins ^ 2),
                data = h.flows.by.time,
                start = list(S = -0.01),
                nls.control(maxiter = 100))
summary(model3.2)
coef.3.2 <- coef(model3.2)

h.flows.by.time$predic_m3.2 <- predict(model3.2, h.flows.by.time)
head(h.flows.by.time)
f4 <- (f2+ geom_line(data = h.flows.by.time, 
                     aes(x = mins, y = predic_m3.2), 
                     color = "green"))
f4

## Log-Logistic (Fisk) function estimation - CDF
median <- median(h.ttm.fl.long$travel_time) #48 minutes

model4.2 <- nls(cumflows ~ 1/(1 + (mins/median) ^ S),
                data = h.flows.by.time,
                start = list(S = -0.01),
                nls.control(maxiter = 100))
summary(model4.2)
coef.4.2 <- coef(model4.2)

h.flows.by.time$predic_m4.2 <- predict(model4.2, h.flows.by.time)
head(h.flows.by.time)
f5 <- (f2+ geom_line(data = h.flows.by.time, 
                     aes(x = mins, y = predic_m4.2), 
                     color = "purple"))
f5

## Logistic function estimation - CDF
mean(h.ttm.fl.long$travel_time) # 21.53
sd(h.ttm.fl.long$travel_time) # 8.48

# append all info from models
Coef<- rbind(coef.1, coef.2, coef.3, coef.4,
             coef.1.1, coef.2.1, coef.3.1, coef.4.1,
             coef.1.2, coef.2.2, coef.3.2, coef.4.2)

coeffs <- data.table(Coef, keep.rownames=TRUE)
coeffs <- rename(coeffs, fit = rn,
                 coef = S)
models <- coeffs

models$fit[models$fit == 'coef.1'] <-   'all_nepdf'
models$fit[models$fit == 'coef.1.1'] <- 'low_nepdf'
models$fit[models$fit == 'coef.1.2'] <- 'high_nepdf'

models$fit[models$fit == 'coef.2'] <-   'all_necdf'
models$fit[models$fit == 'coef.2.1'] <- 'low_necdf'
models$fit[models$fit == 'coef.2.2'] <- 'high_necdf'

models$fit[models$fit == 'coef.3'] <-   'all_gauss'
models$fit[models$fit == 'coef.3.1'] <- 'low_gauss'
models$fit[models$fit == 'coef.3.2'] <- 'high_gauss'

models$fit[models$fit == 'coef.4'] <-   'all_fisk'
models$fit[models$fit == 'coef.4.1'] <- 'low_fisk'
models$fit[models$fit == 'coef.4.2'] <- 'high_fisk'

median.all <- median(ttm.fl.long$travel_time)
median.low <- median(l.ttm.fl.long$travel_time)
median.high <- median(h.ttm.fl.long$travel_time)

mean.all <- mean(ttm.fl.long$travel_time)
mean.low <- mean(l.ttm.fl.long$travel_time)
mean.high <- mean(h.ttm.fl.long$travel_time)

stddev.all <- sd(ttm.fl.long$travel_time)
stddev.low <- sd(l.ttm.fl.long$travel_time)
stddev.high <- sd(h.ttm.fl.long$travel_time)

stats<- rbind(mean.all, median.all, stddev.all,
              mean.low, median.low, stddev.low,
              mean.high, median.high, stddev.high)

descstat <- data.table(stats, keep.rownames=TRUE)

descstat <- rename(descstat, stat = rn, value = V1)
head(descstat,9)

# save
write.csv(models,"Outputs/models_car.csv")
write.csv(descstat,"Outputs/descstat_car.csv")

### GRAVITY-BASED ACCESS =======================================================

ttm <- read.csv('Outputs/ttm_8_9am_car.csv', header = TRUE, na.strings = c("","NA"))
head(ttm)
ttm <- subset(ttm, select = c(od, travel_time))
ttm$fromId <- substring(ttm$od,1,10)
ttm$toId <- substring(ttm$od,12,21)
head(ttm)

jobs <- read.csv('Outputs/ct_centroids_jobs_montreal.csv', header = TRUE, 
                 na.strings = c("","NA"), colClasses = c(id = "character"))

str(jobs)
jobs <- subset(jobs, select=-c(X, lat, lon))
head(jobs)
jobs$toId <- ifelse(nchar(jobs$id)==7,
                    paste0(jobs$id,'.00'),jobs$id)
head(jobs)
jobs <- subset(jobs, select=-c(id))
str(ttm)
str(jobs)

ct <- read.csv('Outputs/ct_centroids_jobs_montreal.csv', header = TRUE, 
               na.strings = c("","NA"), colClasses = c(id = "character"))
ct <- subset(ct, select= -c(X, total_jobs))
ct$id <- ifelse(nchar(ct$id)==7,
                    paste0(ct$id,'.00'), ct$id)
#names(ct)[names(ct) == 'id'] <- 'fromId'
head(ct)
#mapview(ct)

ttm.jobs <- left_join(ttm, jobs, by = 'toId')
head(ttm.jobs)

colnames(ttm.jobs)
ttm.jobs.0 <- drop_na(ttm.jobs, total_jobs)
ttm.jobs.0 <- drop_na(ttm.jobs, lat)
ttm.jobs.0 <- subset(ttm.jobs, fromId != toId)
ttm.jobs.0$total_jobs[is.na(ttm.jobs.0$total_jobs)] <- 0
ttm.jobs.0$jobs.lower.inc[is.na(ttm.jobs.0$jobs.lower.inc)] <- 0
ttm.jobs.0$jobs.higher.inc[is.na(ttm.jobs.0$jobs.higher.inc)] <- 0
ttm.jobs.0$jobs.lower.inc <- as.numeric(ttm.jobs.0$jobs.lower.inc)
ttm.jobs.0$jobs.higher.inc <- as.numeric(ttm.jobs.0$jobs.higher.inc)
head(ttm.jobs.0)

# Estimate jobs weighted by travel times in minutes
## read decay estimates and desc stats 
coef <- read_csv("Outputs/models_car.csv")
coef
desc <- read_csv("Outputs/descstat_car.csv")
desc


# ALL FLOWS
## slice parameters
cf.nepdf.all <- as.numeric(subset(coef,fit=='all_nepdf', select = c(coef)))
cf.necdf.all <- as.numeric(subset(coef,fit=='all_necdf', select = c(coef)))
cf.gauss.all <- as.numeric(subset(coef,fit=='all_gauss', select = c(coef)))
cf.fisk.all <- as.numeric(subset(coef,fit=='all_fisk', select = c(coef)))
p50.all <- as.numeric(subset(desc,stat=='median.all', select = c(value))) # stat, value

## estimate weights
ttm.jobs.0$w.negexp.pdf.all <- exp(cf.nepdf.all * ttm.jobs.0$travel_time)
ttm.jobs.0$w.negexp.cdf.all <- exp(cf.necdf.all * ttm.jobs.0$travel_time)
ttm.jobs.0$w.gauss.cdf.all <- exp(cf.gauss.all * (ttm.jobs.0$travel_time)^2)
ttm.jobs.0$w.fisk.cdf.all <- 1/(1 + ((ttm.jobs.0$travel_time/p50.all)^cf.fisk.all))
ttm.jobs.0$w.negexp.01.all <- exp(-0.010 * ttm.jobs.0$travel_time)
str(ttm.jobs.0)

## get jobs weighted
ttm.jobs.0$wjobs.ne.pdf.all <- ttm.jobs.0$w.negexp.pdf.all * ttm.jobs.0$total_jobs
ttm.jobs.0$wjobs.ne.cdf.all <- ttm.jobs.0$w.negexp.cdf.all * ttm.jobs.0$total_jobs
ttm.jobs.0$wjobs.gauss.cdf.all <- ttm.jobs.0$w.gauss.cdf.all * ttm.jobs.0$total_jobs
ttm.jobs.0$wjobs.fisk.cdf.all <- ttm.jobs.0$w.fisk.cdf.all * ttm.jobs.0$total_jobs
ttm.jobs.0$wjobs.ne.01.all <- ttm.jobs.0$w.negexp.01.all * ttm.jobs.0$total_jobs
str(ttm.jobs.0)

# LOWER INCOME FLOWFS
## slice parameters
cf.nepdf.low <- as.numeric(subset(coef,fit=='low_nepdf', select = c(coef)))
cf.necdf.low <- as.numeric(subset(coef,fit=='low_necdf', select = c(coef)))
cf.gauss.low <- as.numeric(subset(coef,fit=='low_gauss', select = c(coef)))
cf.fisk.low <- as.numeric(subset(coef,fit=='low_fisk', select = c(coef)))
p50.low <- as.numeric(subset(desc,stat=='median.low', select = c(value)))

## estimate weights
ttm.jobs.0$w.negexp.pdf.low <- exp(cf.nepdf.low * ttm.jobs.0$travel_time)
ttm.jobs.0$w.negexp.cdf.low <- exp(cf.necdf.low * ttm.jobs.0$travel_time)
ttm.jobs.0$w.gauss.cdf.low <- exp(cf.gauss.low * (ttm.jobs.0$travel_time)^2)
ttm.jobs.0$w.fisk.cdf.low <- 1/(1 + ((ttm.jobs.0$travel_time/p50.low)^cf.fisk.low))
ttm.jobs.0$w.negexp.01.low <- exp(-0.010 * ttm.jobs.0$travel_time)

## get jobs weighted
ttm.jobs.0$wjobs.ne.pdf.low <- ttm.jobs.0$w.negexp.pdf.low * ttm.jobs.0$jobs.lower.inc
ttm.jobs.0$wjobs.ne.cdf.low <- ttm.jobs.0$w.negexp.cdf.low * ttm.jobs.0$jobs.lower.inc
ttm.jobs.0$wjobs.gauss.cdf.low <- ttm.jobs.0$w.gauss.cdf.low * ttm.jobs.0$jobs.lower.inc
ttm.jobs.0$wjobs.fisk.cdf.low <- ttm.jobs.0$w.fisk.cdf.low * ttm.jobs.0$jobs.lower.inc
ttm.jobs.0$wjobs.ne.01.low <- ttm.jobs.0$w.negexp.01.low * ttm.jobs.0$jobs.lower.inc

# HIGHER INCOME FLOWS
## slice parameters
cf.nepdf.high <- as.numeric(subset(coef,fit=='high_nepdf', select = c(coef)))
cf.necdf.high <- as.numeric(subset(coef,fit=='high_necdf', select = c(coef)))
cf.gauss.high <- as.numeric(subset(coef,fit=='high_gauss', select = c(coef)))
cf.fisk.high <- as.numeric(subset(coef,fit=='high_fisk', select = c(coef)))
p50.high <- as.numeric(subset(desc,stat=='median.high', select = c(value)))

## estimate weights
ttm.jobs.0$w.negexp.pdf.high <- exp(cf.nepdf.high * ttm.jobs.0$travel_time)
ttm.jobs.0$w.negexp.cdf.high <- exp(cf.necdf.high * ttm.jobs.0$travel_time)
ttm.jobs.0$w.gauss.cdf.high <- exp(cf.gauss.high * (ttm.jobs.0$travel_time)^2)
ttm.jobs.0$w.fisk.cdf.high <- 1/(1 + ((ttm.jobs.0$travel_time/p50.high)^cf.fisk.high))
ttm.jobs.0$w.negexp.01.high <- exp(-0.010 * ttm.jobs.0$travel_time)

## get jobs weighted
ttm.jobs.0$wjobs.ne.pdf.high <- ttm.jobs.0$w.negexp.pdf.high * ttm.jobs.0$jobs.higher.inc
ttm.jobs.0$wjobs.ne.cdf.high <- ttm.jobs.0$w.negexp.cdf.high * ttm.jobs.0$jobs.higher.inc
ttm.jobs.0$wjobs.gauss.cdf.high <- ttm.jobs.0$w.gauss.cdf.high * ttm.jobs.0$jobs.higher.inc
ttm.jobs.0$wjobs.fisk.cdf.high <- ttm.jobs.0$w.fisk.cdf.high * ttm.jobs.0$jobs.higher.inc
ttm.jobs.0$wjobs.ne.01.high <- ttm.jobs.0$w.negexp.01.high * ttm.jobs.0$jobs.higher.inc

str(ttm.jobs.0)

# clean a bit
ttm.jobs.1 <- subset(ttm.jobs.0, select = c(od, fromId, toId,
                                            wjobs.ne.pdf.all, 
                                            wjobs.ne.cdf.all, 
                                            wjobs.gauss.cdf.all, 
                                            wjobs.fisk.cdf.all,
                                            wjobs.ne.01.all, 
                                            
                                            wjobs.ne.pdf.low, 
                                            wjobs.ne.cdf.low, 
                                            wjobs.gauss.cdf.low, 
                                            wjobs.fisk.cdf.low,
                                            wjobs.ne.01.low,
                                            
                                            wjobs.ne.pdf.high, 
                                            wjobs.ne.cdf.high, 
                                            wjobs.gauss.cdf.high, 
                                            wjobs.fisk.cdf.high,
                                            wjobs.ne.01.high,
                                            
                                            total_jobs,
                                            jobs.lower.inc,
                                            jobs.higher.inc,
                                            travel_time))
str(ttm.jobs.1)

# pivot by origin
acc_gr0 <- ttm.jobs.1 %>% group_by(fromId) %>% 
  summarise(wjobs.ne.pdf.all = sum(wjobs.ne.pdf.all),
            wjobs.ne.cdf.all = sum(wjobs.ne.cdf.all),
            wjobs.gauss.cdf.all = sum(wjobs.gauss.cdf.all),
            wjobs.fisk.cdf.all = sum(wjobs.fisk.cdf.all),
            wjobs.ne.01.all = sum(wjobs.ne.01.all),
            
            wjobs.ne.pdf.low = sum(wjobs.ne.pdf.low),
            wjobs.ne.cdf.low = sum(wjobs.ne.cdf.low),
            wjobs.gauss.cdf.low = sum(wjobs.gauss.cdf.low),
            wjobs.fisk.cdf.low = sum(wjobs.fisk.cdf.low),
            wjobs.ne.01.low = sum(wjobs.ne.01.low),
            
            
            wjobs.ne.pdf.high = sum(wjobs.ne.pdf.high),
            wjobs.ne.cdf.high = sum(wjobs.ne.cdf.high),
            wjobs.gauss.cdf.high = sum(wjobs.gauss.cdf.high),
            wjobs.fisk.cdf.high = sum(wjobs.fisk.cdf.high),
            wjobs.ne.01.high = sum(wjobs.ne.01.high),
  )

str(acc_gr0) #109 Ids with access estimates
str(jobs) # 109 obs
str(ct) # 109 census tracts

acc_gr.geo <- left_join(ct, rename(acc_gr0, id = fromId), by = 'id')
str(acc_gr.geo) # 181 census tracts
acc_gr.geo[is.na(acc_gr.geo)] <- 0
head(acc_gr.geo)
head(jobs)
acc_gr1 <- left_join(acc_gr.geo, rename(jobs, id = toId, 
                                        alljobs_atId = total_jobs,
                                        lowjobs_atId = jobs.lower.inc,
                                        highjobs_atId = jobs.higher.inc), 
                     by = 'id')

acc_gr1$alljobs_atId <- as.numeric(acc_gr1$alljobs_atId)
acc_gr1$lowjobs_atId <- as.numeric(acc_gr1$lowjobs_atId)
acc_gr1$highjobs_atId <- as.numeric(acc_gr1$highjobs_atId)

acc_gr1$alljobs_atId[is.na(acc_gr1$alljobs_atId)] <- 0
acc_gr1$lowjobs_atId[is.na(acc_gr1$lowjobs_atId)] <- 0
acc_gr1$highjobs_atId[is.na(acc_gr1$highjobs_atId)] <- 0

acc_gr2 <- subset(acc_gr1, select = -c(lat, lon))

str(acc_gr2)

# Add jobs in each origin
acc_gr2$w.alljobs.ne.pdf <- acc_gr2$wjobs.ne.pdf.all + acc_gr2$alljobs_atId
acc_gr2$w.alljobs.ne.cdf <- acc_gr2$wjobs.ne.cdf.all + acc_gr2$alljobs_atId
acc_gr2$w.alljobs.gauss.cdf <- acc_gr2$wjobs.gauss.cdf.all + acc_gr2$alljobs_atId
acc_gr2$w.alljobs.fisk.cdf <- acc_gr2$wjobs.fisk.cdf.all + acc_gr2$alljobs_atId
acc_gr2$w.alljobs.ne.01 <- acc_gr2$wjobs.ne.01.all + acc_gr2$alljobs_atId

acc_gr2$w.lowjobs.ne.pdf <- acc_gr2$wjobs.ne.pdf.low + acc_gr2$lowjobs_atId
acc_gr2$w.lowjobs.ne.cdf <- acc_gr2$wjobs.ne.cdf.low + acc_gr2$lowjobs_atId
acc_gr2$w.lowjobs.gauss.cdf <- acc_gr2$wjobs.gauss.cdf.low + acc_gr2$lowjobs_atId
acc_gr2$w.lowjobs.fisk.cdf <- acc_gr2$wjobs.fisk.cdf.low + acc_gr2$lowjobs_atId
acc_gr2$w.lowjobs.ne.01 <- acc_gr2$wjobs.ne.01.low + acc_gr2$lowjobs_atId

acc_gr2$w.highjobs.ne.pdf <- acc_gr2$wjobs.ne.pdf.high + acc_gr2$highjobs_atId
acc_gr2$w.highjobs.ne.cdf <- acc_gr2$wjobs.ne.cdf.high + acc_gr2$highjobs_atId
acc_gr2$w.highjobs.gauss.cdf <- acc_gr2$wjobs.gauss.cdf.high + acc_gr2$highjobs_atId
acc_gr2$w.highjobs.fisk.cdf <- acc_gr2$wjobs.fisk.cdf.high + acc_gr2$highjobs_atId
acc_gr2$w.highjobs.ne.01 <- acc_gr2$wjobs.ne.01.high + acc_gr2$highjobs_atId


str(acc_gr2)

## clean a bit more
access_weighted <- subset(acc_gr2, select = -c(wjobs.ne.pdf.all, 
                                               wjobs.ne.cdf.all,
                                               wjobs.gauss.cdf.all,
                                               wjobs.fisk.cdf.all,
                                               wjobs.ne.01.all,
                                               
                                               wjobs.ne.pdf.low, 
                                               wjobs.ne.cdf.low,
                                               wjobs.gauss.cdf.low,
                                               wjobs.fisk.cdf.low,
                                               wjobs.ne.01.low,
                                               
                                               wjobs.ne.pdf.high, 
                                               wjobs.ne.cdf.high,
                                               wjobs.gauss.cdf.high,
                                               wjobs.fisk.cdf.high,
                                               wjobs.ne.01.high,
                                               
                                               alljobs_atId,
                                               lowjobs_atId,
                                               highjobs_atId
                                               ))

str(access_weighted)
head(access_weighted)

access_weighted$geometry <- NULL
colnames(access_weighted)

access_weighted <- rename(access_weighted, 
                          allwjobs_nepdf=w.alljobs.ne.pdf,
                          allwjobs_necdf=w.alljobs.ne.cdf,
                          allwjobs_gauss=w.alljobs.gauss.cdf,
                          allwjobs_fisk =w.alljobs.fisk.cdf,
                          allwjobs_ne01 =w.alljobs.ne.01,
                          
                          lowwjobs_nepdf=w.lowjobs.ne.pdf,
                          lowwjobs_necdf=w.lowjobs.ne.cdf,
                          lowwjobs_gauss=w.lowjobs.gauss.cdf,
                          lowwjobs_fisk =w.lowjobs.fisk.cdf,
                          lowwjobs_ne01 =w.lowjobs.ne.01,
                          
                          highwjobs_nepdf=w.highjobs.ne.pdf,
                          highwjobs_necdf=w.highjobs.ne.cdf,
                          highwjobs_gauss=w.highjobs.gauss.cdf,
                          highwjobs_fisk =w.highjobs.fisk.cdf,
                          highwjobs_ne01 =w.highjobs.ne.01
                          )

write.csv(access_weighted,"Outputs/access_weighted.csv")

### CORRELATION ================================================================

# All flows
acc_unweighted <- read_csv("Outputs/access_unweighted.csv")
head(acc_unweighted)
acc_unweighted <- subset(acc_unweighted, select=-c(X1))
names(acc_unweighted)[names(acc_unweighted)=='From'] <- "id"
acc_unweighted$id <- as.character(acc_unweighted$id)
acc_unweighted$id <- ifelse(nchar(acc_unweighted$id)==7,paste0(acc_unweighted$id,'.00'),acc_unweighted$id)


acc_weighted <- read_csv("Outputs/access_weighted.csv")
acc_weighted$id <- as.character(acc_weighted$id)
colnames(acc_weighted)
acc_weighted$id <- ifelse(nchar(acc_weighted$id)==7,paste0(acc_weighted$id,'.00'),acc_weighted$id)

acc_weighted.all <- subset(acc_weighted, select = c(id,
                                                    allwjobs_nepdf,
                                                    allwjobs_necdf,
                                                    allwjobs_gauss,
                                                    allwjobs_fisk))
                                                   #allwjobs_ne01))
colnames(acc_weighted.all)

access_master_all <-left_join(acc_unweighted, acc_weighted.all, by = 'id')
#access_master_all <- drop_na(access_master_all)
colnames(access_master_all,2)
write.csv(access_master_all,"Outputs/access_master_alljobs.csv")

access.all_slim <- subset(access_master_all, select = -c(id))
colnames(access.all_slim)

corr.mtx.all <- cor(access.all_slim, method = 'pearson')
write.csv(corr.mtx.all,"Outputs/acc_corr_mtx.all_car.csv")

df1 <- as.data.frame(corr.mtx.all)
df1[ "rowname" ] <- rownames(df1)
df2 <- df1[1:15,]
df2$rowname <- as.numeric(df2$rowname)
df3 <- subset(df2, select=c(rowname,
                            allwjobs_nepdf,
                            allwjobs_necdf,
                            allwjobs_gauss,
                            allwjobs_fisk))
#allwjobs_ne01))
data_long <- gather(df3, Type, Correlation, allwjobs_nepdf:allwjobs_fisk, factor_key=TRUE)

write.csv(data_long,"Outputs/acc_corr_mtx.all_vis_car.csv")

# Lower-income flows
acc_unweighted_long.low <- read_csv("Outputs/access_unweighted_lower.csv")
head(acc_unweighted_long.low)
acc_unweighted_long.low <- rename(acc_unweighted_long.low, id = From)
acc_unweighted_long.low$id <- as.character(acc_unweighted_long.low$id)
acc_unweighted_long.low$id <- ifelse(nchar(acc_unweighted_long.low$id)==7,paste0(acc_unweighted_long.low$id,'.00'),acc_unweighted_long.low$id)
acc_unweighted.low <- subset(acc_unweighted_long.low, select = -c(...1))

acc_weighted <- read_csv("outputs/access_weighted.csv")
acc_weighted$id <- as.character(acc_weighted$id)
colnames(acc_weighted)
acc_weighted$id <- ifelse(nchar(acc_weighted$id)==7,paste0(acc_weighted$id,'.00'),acc_weighted$id)

acc_weighted.low <- subset(acc_weighted, select = c(id,
                                                    lowwjobs_nepdf,
                                                    lowwjobs_necdf,
                                                    lowwjobs_gauss,
                                                    lowwjobs_fisk
                                                    #lowwjobs_ne01
))
colnames(acc_weighted.low)

access_master_low <-left_join(acc_unweighted.low, acc_weighted.low, by = 'id')
#access_master_low <- drop_na(access_master_low)
colnames(access_master_low,2)
write.csv(access_master_low,"outputs/access_master_lowjobs_car.csv")

access.low_slim <- subset(access_master_low, select = -c(id))
colnames(access.low_slim)

corr.mtx.low <- cor(access.low_slim, method = 'pearson')
write.csv(corr.mtx.low,"outputs/acc_corr_mtx.low_car.csv")

df1.low <- as.data.frame(corr.mtx.low)
df1.low[ "rowname" ] <- rownames(df1.low)
df2.low <- df1.low[1:15,]
df2.low$rowname <- as.numeric(df2.low$rowname)
df3.low <- subset(df2.low, select=c(rowname,
                                    lowwjobs_nepdf,
                                    lowwjobs_necdf,
                                    lowwjobs_gauss,
                                    lowwjobs_fisk))
#lowwjobs_ne01))
data_long.low <- gather(df3.low, Type, Correlation, lowwjobs_nepdf:lowwjobs_fisk, factor_key=TRUE)

write.csv(data_long.low,"Outputs/acc_corr_mtx.low_vis_car.csv")

# Higher-income flows
acc_unweighted_long.high <- read_csv("outputs/access_unweighted_higher.csv")
head(acc_unweighted_long.high)
acc_unweighted_long.high <- rename(acc_unweighted_long.high, id = From)
acc_unweighted_long.high$id <- as.character(acc_unweighted_long.high$id)
acc_unweighted_long.high$id <- ifelse(nchar(acc_unweighted_long.high$id)==7,paste0(acc_unweighted_long.high$id,'.00'),acc_unweighted_long.high$id)
acc_unweighted.high <- subset(acc_unweighted_long.high, select = -c(...1))

acc_weighted <- read_csv("outputs/access_weighted.csv")
#acc_weighted$id <- as.character(acc_weighted$id)
colnames(acc_weighted)
acc_weighted$id <- ifelse(nchar(acc_weighted$id)==7,paste0(acc_weighted$id,'.00'),acc_weighted$id)

acc_weighted.high <- subset(acc_weighted, select = c(id,
                                                     highwjobs_nepdf,
                                                     highwjobs_necdf,
                                                     highwjobs_gauss,
                                                     highwjobs_fisk))
#highwjobs_ne01))
colnames(acc_weighted.high)

access_master_high <-left_join(acc_unweighted.high, acc_weighted.high, by = 'id')
access_master_high <- drop_na(access_master_high)
colnames(access_master_high,2)
write.csv(access_master_high,"outputs/access_master_highjobs_car.csv")

access.high_slim <- subset(access_master_high, select = -c(id))
colnames(access.high_slim)

corr.mtx.high <- cor(access.high_slim, method = 'pearson')
write.csv(corr.mtx.high,"outputs/acc_corr_mtx.high_car.csv")

df1.high <- as.data.frame(corr.mtx.high)
df1.high[ "rowname" ] <- rownames(df1.high)
df2.high <- df1.high[1:15,]
df2.high$rowname <- as.numeric(df2.high$rowname)
df3.high <- subset(df2.high, select=c(rowname,
                                      highwjobs_nepdf,
                                      highwjobs_necdf,
                                      highwjobs_gauss,
                                      highwjobs_fisk))
#highwjobs_ne01))
data_long.high <- gather(df3.high, Type, Correlation, highwjobs_nepdf:highwjobs_fisk, factor_key=TRUE)

write.csv(data_long.high,"Outputs/acc_corr_mtx.high_vis_car.csv")


### VISUALIZE ==================================================================

# All jobs

data_long <- read_csv("Outputs/acc_corr_mtx.all_vis_car.csv")
data_long$Type <- as.factor(data_long$Type)

# Rename factor levels
levels(data_long$Type) <- list("Negative Exponential PDF" = "allwjobs_nepdf",        
                               "Negative Exponential CDF" = "allwjobs_necdf",
                               "Gaussain CDF" = "allwjobs_gauss",
                               "Log-Logistic CDF" = "allwjobs_fisk")
# "Negative Exponential (0.01)" = "allwjobs_ne01")
# Plot

ggplot(data_long, 
       aes(x = rowname, y = Correlation, group=Type)) + 
  geom_line(aes(color=Type), size=1) + 
  scale_color_manual(values=c("#0D5C91", "#9FC8E3", "#F9BF78", "#E27F08")) +
  geom_vline(aes(xintercept=25.68), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=26.8), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=36.8), colour="Black", linetype="dashed") +
  geom_text(aes(x=23.68, label="Median = 25.68", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=28.8, label="Mean = 26.8", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=34.8, label="Mean+SD = 36.8", y=0.45), colour="Black", angle=90, size=8) +
  #labs(y="Correlation Coefficient",
  #     x="Cummulative Opportunities Threshold (min)") +
  theme(legend.position = c(0.80, 0.2), text = element_text(size = 23), legend.title = element_blank()) + 
  scale_x_continuous(name="Cumulative Opportunities Threshold (min)", limits = c(0, 75), breaks = seq(0,80,10)) +
  scale_y_continuous(name="Correlation Coefficient", limits = c(0, 1), breaks = c(0, 0.3, 0.6, 0.9)) +
  ggtitle("Montreal")

ggsave("Outputs/fig2_car_all_incomes.jpg", width = 8, height = 5.5)


# Low-income jobs

data_long.low <- read_csv("Outputs/acc_corr_mtx.low_vis_car.csv")
data_long.low$Type <- as.factor(data_long.low$Type)

# Rename factor levels
levels(data_long.low$Type) <- list("Negative Exponential PDF" = "lowwjobs_nepdf",        
                                   "Negative Exponential CDF" = "lowwjobs_necdf",
                                   "Gaussain CDF" = "lowwjobs_gauss",
                                   "Log-Logistic CDF" = "lowwjobs_fisk")
# "Negative Exponential (0.01)" = "lowwjobs_ne01")
# Plot

ggplot(data_long.low, 
       aes(x = rowname, y = Correlation, group=Type)) + 
  geom_line(aes(color=Type), size=1) + 
  scale_color_manual(values=c("#0D5C91", "#9FC8E3", "#F9BF78", "#E27F08")) +
  geom_vline(aes(xintercept=22.55), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=24.1), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=33.66), colour="Black", linetype="dashed") +
  geom_text(aes(x=20.55, label="Median = 22.55", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=26.1, label="Mean = 24.1", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=31.6, label="Mean+SD = 33.6", y=0.45), colour="Black", angle=90, size=8) +
  #labs(y="Correlation Coefficient",
  #     x="Cummulative Opportunities Threshold (min)") +
  theme(legend.position = c(0.80, 0.2), text = element_text(size = 23), legend.title = element_blank()) + 
  scale_x_continuous(name="Cumulative Opportunities Threshold (min)", limits = c(0, 75), breaks = seq(0,80,10)) +
  scale_y_continuous(name="Correlation Coefficient", limits = c(0, 1), breaks = c(0, 0.3, 0.6, 0.9)) +
  ggtitle("Montreal")

ggsave("Outputs/fig2_car_low_incomes.jpg", width = 8, height = 5.5)


# High-income jobs

data_long.high <- read_csv("Outputs/acc_corr_mtx.high_vis_car.csv")
data_long.high$Type <- as.factor(data_long.high$Type)

# Rename factor levels
levels(data_long.high$Type) <- list("Negative Exponential PDF" = "highwjobs_nepdf",        
                                    "Negative Exponential CDF" = "highwjobs_necdf",
                                    "Gaussain CDF" = "highwjobs_gauss",
                                    "Log-Logistic CDF" = "highwjobs_fisk")
# "Negative Exponential (0.01)" = "lowwjobs_ne01")

# Plot

ggplot(data_long.high, 
       aes(x = rowname, y = Correlation, group=Type)) + 
  geom_line(aes(color=Type), size=1) + 
  scale_color_manual(values=c("#0D5C91", "#9FC8E3", "#F9BF78", "#E27F08")) +
  geom_vline(aes(xintercept=27.15), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=28.11), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=38.13), colour="Black", linetype="dashed") +
  geom_text(aes(x=25.15, label="Median = 27.15", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=30.11, label="Mean = 28.11", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=36.13, label="Mean+SD = 38.13", y=0.45), colour="Black", angle=90, size=8) +
  #labs(y="Correlation Coefficient",
  #     x="Cummulative Opportunities Threshold (min)") +
  theme(legend.position = c(0.80, 0.2), text = element_text(size = 23), legend.title = element_blank()) + 
  scale_x_continuous(name="Cumulative Opportunities Threshold (min)", limits = c(0, 75), breaks = seq(0,80,10)) +
  scale_y_continuous(name="Correlation Coefficient", limits = c(0, 1), breaks = c(0, 0.3, 0.6, 0.9)) +
  ggtitle("Montreal")

ggsave("Outputs/fig2_car_high_incomes.jpg", width = 8, height = 5.5)
