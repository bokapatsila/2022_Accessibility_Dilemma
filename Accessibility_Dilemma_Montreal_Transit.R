### This code was developed to calculate Job accessibility by transit for the paper 
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
library(r5r)
options(java.parameters = "-Xmx5G")

### Modify GTFS Feed for STM (Frequency to timetable) ==========================
# Needed only for Montreal STM
# https://github.com/ipeaGIT/r5r/issues/181
library(gtfstools)
library(gtfs2gps)

frequencies_gtfs <- read_gtfs("Inputs/Frequencies/societe-de-transport-de-montreal_39_20160816.zip")
test_gtfs_freq(frequencies_gtfs)
stop_times_gtfs <- frequencies_to_stop_times(frequencies_gtfs)
write_gtfs(stop_times_gtfs, "Inputs/societe-de-transport-de-montreal_39_20160816.zip")

### Read CMA CT centroids (preprocessed in GIS) ================================

ct_centroids <- read.csv('Inputs/MontrealCMA_Centroids.csv', header = TRUE, na.strings = c("","NA"))
ct_centroids$id <- as.character(ct_centroids$id)
ct_centroids$id <- ifelse(nchar(ct_centroids$id)==7,paste0(ct_centroids$id,'.00'),ct_centroids$id)
#ct_centroids <- ct_centroids[, c(1, 3, 2)] # Should be id, lat, lon

#Setup the environment for r5r, osm.pbf id from https://extract.bbbike.org/
r5r_core <- setup_r5('D:/Research/2022_Cum_Grav_Accessibility/Montreal/Inputs', verbose=FALSE)

# Check where origin or destination points are snapped
snap_df <- find_snap(r5r_core, points = ct_centroids, mode = 'WALK')

### Calculate transit travel time ==============================================
# GTFS Feed Source: https://transitfeeds.com/l/32-canada

mode <- c("WALK", "TRANSIT")
max_walk_dist <- 750
max_trip_duration <- 100
time_window <- 60
percentiles <- c(50)

departure_datetime <- as.POSIXct("19-10-2016 08:00:00",
                                 format = "%d -%m-%Y %H:%M:%S", 
                                 tz = "America/Montreal")

ttm <- travel_time_matrix(r5r_core = r5r_core,
                          departure_datetime = departure_datetime, 
                          origins = ct_centroids,
                          destinations = ct_centroids,
                          mode = mode,
                          max_walk_dist = max_walk_dist,
                          max_trip_duration = max_trip_duration,
                          time_window = time_window,
                          percentiles = percentiles,
                          max_rides = 3,
                          verbose = FALSE)

head(ttm)

ttm$od <- paste(ttm$fromId, ttm$toId, sep = '-')
head(ttm)

setwd("D:/Research/2022_Cum_Grav_Accessibility/Montreal")

#write.csv(ttm, "ttm_750m_20211210_0250.csv")
write.csv(ttm, "Outputs/ttm_Montreal.csv")

### Total jobs file (all modes), add coordinates for cumulative access =========

# Read jobs, this dataset was a custom a request from Statistics Canada producing counts of 
# commuters from and to every Census Tract by income group
jobs.can <- read.csv('Inputs/Jobs_at_CTs_reduced.csv', colClasses = c(ID = "character"))

### Jobs cleanup
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

### CUMULATIVE TRANSIT ACCESS ==================================================

pts <- read.csv('Outputs/ct_centroids_jobs_montreal.csv', header = TRUE, na.strings = c("","NA"))
pts <- subset(pts, select=-c(X))

# set r5r parameters
mode <- c("WALK", "TRANSIT")
max_walk_dist <- 1200
max_trip_duration <- 60*2
time_window <- 60
percentiles <- c(50)
cutoffs0 <- seq(5, 60, by=5)
cutoffs1 <- seq(60, 90, by=5)
cutoffs2 <- seq(90, 120, by=5)
dep_time <- as.POSIXct("19-10-2016 08:00:00",
                                 format = "%d -%m-%Y %H:%M:%S", 
                                 tz = "America/Montreal")

# Get cumulative opportunities for all jobs
access_unweighted.0 <- accessibility(r5r_core = r5r_core,
                                     departure_datetime = dep_time,
                                     origins = pts,
                                     destinations = pts,
                                     mode = mode,
                                     opportunities_colname = "total_jobs",
                                     decay_function = "step",
                                     max_walk_dist = max_walk_dist,
                                     cutoffs = cutoffs0,
                                     time_window = time_window,
                                     percentiles = percentiles,
                                     verbose = FALSE)

head(access_unweighted.0)
unique(access_unweighted.0$cutoff)

access_unweighted.1 <- accessibility(r5r_core = r5r_core,
                                     departure_datetime = dep_time,
                                     origins = pts,
                                     destinations = pts,
                                     mode = mode,
                                     opportunities_colname = "total_jobs",
                                     decay_function = "step",
                                     max_walk_dist = max_walk_dist,
                                     cutoffs = cutoffs1,
                                     time_window = time_window,
                                     percentiles = percentiles,
                                     verbose = FALSE)
access_unweighted.1
unique(access_unweighted.1$cutoff)

access_unweighted.2 <- accessibility(r5r_core = r5r_core,
                                     departure_datetime = dep_time,
                                     origins = pts,
                                     destinations = pts,
                                     mode = mode,
                                     opportunities_colname = "total_jobs",
                                     decay_function = "step",
                                     max_walk_dist = max_walk_dist,
                                     cutoffs = cutoffs2,
                                     time_window = time_window,
                                     percentiles = percentiles,
                                     verbose = FALSE)
access_unweighted.2
unique(access_unweighted.2$cutoff)

# Append results
access_unweighted.0 %>% filter(cutoff == 60)
access_unweighted <- rbind(access_unweighted.0,
                           access_unweighted.1 %>% 
                             filter(cutoff!=60),
                           access_unweighted.2 %>% 
                             filter(cutoff!=90))

unique(access_unweighted$cutoff)
head(access_unweighted)

write.csv(access_unweighted,"Outputs/transit_access_unweighted.csv")

# Get cumulative opportunities for lower-income jobs
rJava::.jgc(R.gc = TRUE)
access_unweighted.l.0 <- accessibility(r5r_core = r5r_core,
                                       departure_datetime = dep_time,
                                       origins = pts,
                                       destinations = pts,
                                       mode = mode,
                                       opportunities_colname = "jobs.lower.inc",
                                       decay_function = "step",
                                       max_walk_dist = max_walk_dist,
                                       cutoffs = cutoffs0,
                                       time_window = time_window,
                                       percentiles = percentiles,
                                       verbose = FALSE)

head(access_unweighted.l.0)
unique(access_unweighted.l.0$cutoff)

rJava::.jgc(R.gc = TRUE)
access_unweighted.l.1 <- accessibility(r5r_core = r5r_core,
                                       departure_datetime = dep_time,
                                       origins = pts,
                                       destinations = pts,
                                       mode = mode,
                                       opportunities_colname = "jobs.lower.inc",
                                       decay_function = "step",
                                       max_walk_dist = max_walk_dist,
                                       cutoffs = cutoffs1,
                                       time_window = time_window,
                                       percentiles = percentiles,
                                       verbose = FALSE)
access_unweighted.l.1
unique(access_unweighted.l.1$cutoff)

rJava::.jgc(R.gc = TRUE)
access_unweighted.l.2 <- accessibility(r5r_core = r5r_core,
                                       departure_datetime = dep_time,
                                       origins = pts,
                                       destinations = pts,
                                       mode = mode,
                                       opportunities_colname = "jobs.lower.inc",
                                       decay_function = "step",
                                       max_walk_dist = max_walk_dist,
                                       cutoffs = cutoffs2,
                                       time_window = time_window,
                                       percentiles = percentiles,
                                       verbose = FALSE)
access_unweighted.l.2
unique(access_unweighted.l.2$cutoff)

# Append results

access_unweighted.l.0 %>% filter(cutoff == 60)

access_unweighted.l <- rbind(access_unweighted.l.0,
                             access_unweighted.l.1 %>% 
                               filter(cutoff!=60),
                             access_unweighted.l.2 %>% 
                               filter(cutoff!=90))

unique(access_unweighted.l$cutoff)

head(access_unweighted.l)

write.csv(access_unweighted.l,"Outputs/transit_access_unweighted_lower.csv")

# Get cumulative opportunities for higher-income jobs
rJava::.jgc(R.gc = TRUE)

access_unweighted.h.0 <- accessibility(r5r_core = r5r_core,
                                       departure_datetime = dep_time,
                                       origins = pts,
                                       destinations = pts,
                                       mode = mode,
                                       opportunities_colname = "jobs.higher.inc",
                                       decay_function = "step",
                                       max_walk_dist = max_walk_dist,
                                       cutoffs = cutoffs0,
                                       time_window = time_window,
                                       percentiles = percentiles,
                                       verbose = FALSE)

head(access_unweighted.h.0)
unique(access_unweighted.h.0$cutoff)

rJava::.jgc(R.gc = TRUE)
access_unweighted.h.1 <- accessibility(r5r_core = r5r_core,
                                       departure_datetime = dep_time,
                                       origins = pts,
                                       destinations = pts,
                                       mode = mode,
                                       opportunities_colname = "jobs.higher.inc",
                                       decay_function = "step",
                                       max_walk_dist = max_walk_dist,
                                       cutoffs = cutoffs1,
                                       time_window = time_window,
                                       percentiles = percentiles,
                                       verbose = FALSE)
access_unweighted.h.1
unique(access_unweighted.h.1$cutoff)

rJava::.jgc(R.gc = TRUE)
access_unweighted.h.2 <- accessibility(r5r_core = r5r_core,
                                       departure_datetime = dep_time,
                                       origins = pts,
                                       destinations = pts,
                                       mode = mode,
                                       opportunities_colname = "jobs.higher.inc",
                                       decay_function = "step",
                                       max_walk_dist = max_walk_dist,
                                       cutoffs = cutoffs2,
                                       time_window = time_window,
                                       percentiles = percentiles,
                                       verbose = FALSE)
access_unweighted.h.2
unique(access_unweighted.h.2$cutoff)

# Append results

access_unweighted.h.0 %>% filter(cutoff == 60)

access_unweighted.h <- rbind(access_unweighted.h.0,
                             access_unweighted.h.1 %>% 
                               filter(cutoff!=60),
                             access_unweighted.h.2 %>% 
                               filter(cutoff!=90))

unique(access_unweighted.h$cutoff)

head(access_unweighted.h)

write.csv(access_unweighted.h,"Outputs/transit_access_unweighted_higher.csv")

stop_r5(r5r_core)

### Jobs accessible by transit prep ============================================

fl <- read.csv('Inputs/transitflows_inc.csv')
colnames(fl)
fl_slim0 <- subset(fl, select = -c(GeoFile, 
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

write.csv(fl_slim4,"Outputs/flows_transit_montreal.csv")

### Join travel time with flows ================================================

ttm_8_9am_transit <- read.csv('Outputs/ttm_Montreal.csv', header = TRUE, na.strings = c("","NA"))
ttm_8_9am_transit <- subset(ttm_8_9am_transit, select=-c(X))

flows_transit_quebec <- read.csv('Outputs/flows_transit_montreal.csv', header = TRUE, na.strings = c("","NA"))
flows_transit_quebec <- subset(flows_transit_quebec, select=-c(X))
sum(is.na(flows_transit_quebec$total))


# Join
ttm.fl <- left_join(flows_transit_quebec, ttm_8_9am_transit, by = 'od')
ttm.fl.clean <- ttm.fl %>% filter(travel_time >0)
head(ttm.fl)
rm(flows_transit_quebec, ttm_8_9am_transit, ttm.fl)


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

write.csv(ttm.fl.long, "Outputs/all_flows_time_transit.csv")
write.csv(l.ttm.fl.long, "Outputs/l_inc_flows_time_transit.csv")
write.csv(h.ttm.fl.long, "Outputs/h_inc_flows_time_transit.csv")

### Parameters for gravity models ==============================================

ttm.fl.long <- read.csv('Outputs/all_flows_time_transit.csv', header = TRUE, na.strings = c("","NA"))

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
           label= "Y == e ^ {-0.0238 * X}", 
           parse = TRUE) + theme(text = element_text(size = 20))


ggsave("Outputs/fig1a_transit.jpg")

model1 ##
modelr::rsquare(model1, flows.by.time)

# fixed negative exponential decay function estimation - CDF
model2 <- nls(cumflows ~ exp(S * mins), 
              data = flows.by.time,
              start = list(S = -0.01),
              #            algorithm = default is Gaus-Newton; same results using Port
              nls.control(maxiter = 100))
summary(model2)
coef.2 <- coef(model2)

flows.by.time$predic_m2 <- predict(model2, flows.by.time)
head(flows.by.time)
f4 <- (f2+ geom_line(data = flows.by.time, 
                     aes(x = mins, y = predic_m2), 
                     color = "black"))

f4 + annotate('text', x=75, 
              y=.90,
              size = 10,
              fontface = 'bold',
              label= "Y == e ^ {-0.0194 * X}",
              parse = TRUE) + theme(text = element_text(size = 20)) 


ggsave("Outputs/fig1b_transit.jpg")

model2
modelr::rsquare(model2, flows.by.time)

# Gaussian function estimation - CDF
model3 <- nls(cumflows ~ exp(S * mins ^ 2),
              data = flows.by.time,
              start = list(S = -0.01),
              nls.control(maxiter = 100))
summary(model3)
coef.3 <- coef(model3)

flows.by.time$predic_m3 <- predict(model3, flows.by.time)
head(flows.by.time)
f4 <- (f2+ geom_line(data = flows.by.time, 
                     aes(x = mins, y = predic_m3), 
                     color = "black"))

f4 + annotate('text', x=75, 
              y=.90,
              size = 10,
              fontface = 'bold',
              label= "Y == e ^ {-0.000383*X^2}",
              parse = TRUE) + theme(text = element_text(size = 20)) 

ggsave("Outputs/fig1c_transit.jpg")

modelr::rsquare(model3, flows.by.time)


# Log-Logistic (Fisk) function estimation - CDF
median <- median(ttm.fl.long$travel_time) #49 minutes

model4 <- nls(cumflows ~ 1/(1 + (mins/median) ^ S),
              data = flows.by.time,
              start = list(S = -0.01),
              nls.control(maxiter = 100))
summary(model4)
coef.4 <- coef(model4)

flows.by.time$predic_m4 <- predict(model4, flows.by.time)
head(flows.by.time)
f5 <- (f2+ geom_line(data = flows.by.time, 
                     aes(x = mins, y = predic_m4), 
                     color = "black"))

f5 + annotate('text', x=73, 
              y=.90,
              size = 9,
              label= "Y == 1 / 1 + {(X / p50)} ^ 4.69",
              parse = TRUE) + theme(text = element_text(size = 20)) 

ggsave("Outputs/fig1d_transit.jpg")
modelr::rsquare(model4, flows.by.time)


# Logistic function estimation - CDF
mean(ttm.fl.long$travel_time) # 50.75402
median(ttm.fl.long$travel_time) # 49
sd(ttm.fl.long$travel_time) # 19.39245

# Low-income flows

l.ttm.fl.long <- read.csv('Outputs/l_inc_flows_time_transit.csv', header = TRUE, na.strings = c("","NA"))

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
mean(l.ttm.fl.long$travel_time) # 46.56788
sd(l.ttm.fl.long$travel_time) # 18.36827

# High-income flows

h.ttm.fl.long <- read.csv('Outputs/h_inc_flows_time_transit.csv', header = TRUE, na.strings = c("","NA"))

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
mean(h.ttm.fl.long$travel_time) # 50.54965
sd(h.ttm.fl.long$travel_time) # 19.30134


# Append all info from models
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
write.csv(models,"Outputs/models_transit.csv")
write.csv(descstat,"Outputs/descstat_transit.csv")

### GRAVITY-BASED ACCESS =======================================================

ttm <- read.csv('Outputs/ttm_Montreal.csv', header = TRUE, na.strings = c("","NA"))
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
# ttm.jobs.0 <- drop_na(ttm.jobs, lat)
ttm.jobs.0 <- subset(ttm.jobs, fromId != toId)
ttm.jobs.0$total_jobs[is.na(ttm.jobs.0$total_jobs)] <- 0
ttm.jobs.0$jobs.lower.inc[is.na(ttm.jobs.0$jobs.lower.inc)] <- 0
ttm.jobs.0$jobs.higher.inc[is.na(ttm.jobs.0$jobs.higher.inc)] <- 0
ttm.jobs.0$jobs.lower.inc <- as.numeric(ttm.jobs.0$jobs.lower.inc)
ttm.jobs.0$jobs.higher.inc <- as.numeric(ttm.jobs.0$jobs.higher.inc)
head(ttm.jobs.0)

# Estimate jobs weighted by travel times in minutes
## read decay estimates and desc stats 
coef <- read_csv("Outputs/models_transit.csv")
coef
desc <- read_csv("Outputs/descstat_transit.csv")
desc


# ALL FLOWS
## slice parameters
cf.nepdf.all <- as.numeric(subset(coef,fit=='all_nepdf', select = c(coef)))
cf.necdf.all <- as.numeric(subset(coef,fit=='all_necdf', select = c(coef)))
cf.gauss.all <- as.numeric(subset(coef,fit=='all_gauss', select = c(coef)))
cf.fisk.all <- as.numeric(subset(coef,fit=='all_fisk', select = c(coef)))
p50.all <- as.numeric(subset(desc,stat=='median.all', select = c(value)))

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
str(jobs) # 181 obs
str(ct) # 181 census tracts

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
#acc_gr2$w.alljobs.ne.01 <- acc_gr2$wjobs.ne.01.all + acc_gr2$alljobs_atId

acc_gr2$w.lowjobs.ne.pdf <- acc_gr2$wjobs.ne.pdf.low + acc_gr2$lowjobs_atId
acc_gr2$w.lowjobs.ne.cdf <- acc_gr2$wjobs.ne.cdf.low + acc_gr2$lowjobs_atId
acc_gr2$w.lowjobs.gauss.cdf <- acc_gr2$wjobs.gauss.cdf.low + acc_gr2$lowjobs_atId
acc_gr2$w.lowjobs.fisk.cdf <- acc_gr2$wjobs.fisk.cdf.low + acc_gr2$lowjobs_atId
#acc_gr2$w.lowjobs.ne.01 <- acc_gr2$wjobs.ne.01.low + acc_gr2$lowjobs_atId

acc_gr2$w.highjobs.ne.pdf <- acc_gr2$wjobs.ne.pdf.high + acc_gr2$highjobs_atId
acc_gr2$w.highjobs.ne.cdf <- acc_gr2$wjobs.ne.cdf.high + acc_gr2$highjobs_atId
acc_gr2$w.highjobs.gauss.cdf <- acc_gr2$wjobs.gauss.cdf.high + acc_gr2$highjobs_atId
acc_gr2$w.highjobs.fisk.cdf <- acc_gr2$wjobs.fisk.cdf.high + acc_gr2$highjobs_atId
#acc_gr2$w.highjobs.ne.01 <- acc_gr2$wjobs.ne.01.high + acc_gr2$highjobs_atId

str(acc_gr2)

## clean a bit more
access_weighted <- subset(acc_gr2, select = -c(wjobs.ne.pdf.all, 
                                               wjobs.ne.cdf.all,
                                               wjobs.gauss.cdf.all,
                                               wjobs.fisk.cdf.all,
                                               #wjobs.ne.01.all,
                                               
                                               wjobs.ne.pdf.low, 
                                               wjobs.ne.cdf.low,
                                               wjobs.gauss.cdf.low,
                                               wjobs.fisk.cdf.low,
                                               #wjobs.ne.01.low,
                                               
                                               wjobs.ne.pdf.high, 
                                               wjobs.ne.cdf.high,
                                               wjobs.gauss.cdf.high,
                                               wjobs.fisk.cdf.high,
                                               #wjobs.ne.01.high,
                                               
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
                          #allwjobs_ne01 =w.alljobs.ne.01
                          
                          lowwjobs_nepdf=w.lowjobs.ne.pdf,
                          lowwjobs_necdf=w.lowjobs.ne.cdf,
                          lowwjobs_gauss=w.lowjobs.gauss.cdf,
                          lowwjobs_fisk =w.lowjobs.fisk.cdf,
                          #lowwjobs_ne01 =w.lowjobs.ne.01,
                          
                          highwjobs_nepdf=w.highjobs.ne.pdf,
                          highwjobs_necdf=w.highjobs.ne.cdf,
                          highwjobs_gauss=w.highjobs.gauss.cdf,
                          highwjobs_fisk =w.highjobs.fisk.cdf
                          #highwjobs_ne01 =w.highjobs.ne.01
                          )

write.csv(access_weighted,"Outputs/access_weighted_transit.csv")

### CORRELATION ================================================================
library(reshape2)

# All flows
acc_unweighted <- read_csv("Outputs/transit_access_unweighted.csv")
head(acc_unweighted)
acc_unweighted <- subset(acc_unweighted, select=-c(...1))
names(acc_unweighted)[names(acc_unweighted)=='from_id'] <- "id"

acc_unweighted <- reshape2::dcast(acc_unweighted, id + percentile ~ cutoff, 
                        value.var = 'accessibility')
acc_unweighted$id <- as.character(acc_unweighted$id)
head(acc_unweighted)

acc_unweighted$id <- as.character(acc_unweighted$id)
acc_unweighted$id <- ifelse(nchar(acc_unweighted$id)==7,paste0(acc_unweighted$id,'.00'),acc_unweighted$id)


acc_weighted <- read_csv("Outputs/access_weighted_transit.csv")
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
colnames(access_master_all,2)
write.csv(access_master_all,"Outputs/access_master_alljobs_transit.csv")

access.all_slim <- subset(access_master_all, select = -c(id, percentile))
colnames(access.all_slim)

corr.mtx.all <- cor(access.all_slim, method = 'pearson')
write.csv(corr.mtx.all,"Outputs/acc_corr_mtx.all_transit.csv")

corr.mtx.all <- read.csv("Outputs/acc_corr_mtx.all_transit.csv")

df1 <- as.data.frame(corr.mtx.all)
#df1[ "rowname" ] <- rownames(df1)
names(df1)[names(df1)=='X'] <- 'rowname'
df2 <- df1[1:24,]
df2$rowname <- as.numeric(df2$rowname)
df3 <- subset(df2, select=c(rowname,
                            allwjobs_nepdf,
                            allwjobs_necdf,
                            allwjobs_gauss,
                            allwjobs_fisk))
                            #allwjobs_ne01))
data_long <- gather(df3, Type, Correlation, allwjobs_nepdf:allwjobs_fisk, factor_key=TRUE)

write.csv(data_long,"Outputs/acc_corr_mtx.all_vis_transit.csv")

# Lower-income flows
acc_unweighted_long.low <- read_csv("Outputs/transit_access_unweighted_lower.csv")
head(acc_unweighted_long.low)
acc_unweighted_long.low <- rename(acc_unweighted_long.low, id = from_id)
head(acc_unweighted_long.low)

acc_unweighted.low <- dcast(acc_unweighted_long.low, id + percentile ~ cutoff, 
                            value.var = 'accessibility')
acc_unweighted.low$id <- as.character(acc_unweighted.low$id)
acc_unweighted.low$id <- ifelse(nchar(acc_unweighted.low$id)==7,paste0(acc_unweighted.low$id,'.00'),acc_unweighted.low$id)

head(acc_unweighted.low)

acc_weighted <- read_csv("outputs/access_weighted_transit.csv")
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

access_master_low <-left_join(acc_unweighted, acc_weighted.low, by = 'id')
colnames(access_master_low,2)
write.csv(access_master_low,"outputs/access_master_lowjobs_transit.csv")

access.low_slim <- subset(access_master_low, select = -c(id, percentile))
colnames(access.low_slim)

corr.mtx.low <- cor(access.low_slim, method = 'pearson')
write.csv(corr.mtx.low,"outputs/acc_corr_mtx.low_transit.csv")

corr.mtx.low <- read.csv("Outputs/acc_corr_mtx.low_transit.csv")

df1.low <- as.data.frame(corr.mtx.low)
#df1.low[ "rowname" ] <- rownames(df1.low)
names(df1.low)[names(df1.low)=='X'] <- 'rowname'
df2.low <- df1.low[1:24,]
df2.low$rowname <- as.numeric(df2.low$rowname)
df3.low <- subset(df2.low, select=c(rowname,
                            lowwjobs_nepdf,
                            lowwjobs_necdf,
                            lowwjobs_gauss,
                            lowwjobs_fisk))
                           #lowwjobs_ne01))
data_long.low <- gather(df3.low, Type, Correlation, lowwjobs_nepdf:lowwjobs_fisk, factor_key=TRUE)

write.csv(data_long.low,"Outputs/acc_corr_mtx.low_vis_transit.csv")

# Higher-income flows
acc_unweighted_long.high <- read_csv("outputs/transit_access_unweighted_higher.csv")
head(acc_unweighted_long.high)
acc_unweighted_long.high <- rename(acc_unweighted_long.high, id = from_id)
head(acc_unweighted_long.high)

acc_unweighted.high <- dcast(acc_unweighted_long.high, id + percentile ~ cutoff, 
                             value.var = 'accessibility')
acc_unweighted.high$id <- as.character(acc_unweighted.high$id)
acc_unweighted.high$id <- ifelse(nchar(acc_unweighted.high$id)==7,paste0(acc_unweighted.high$id,'.00'),acc_unweighted.high$id)

head(acc_unweighted.high)

acc_weighted <- read_csv("outputs/access_weighted_transit.csv")
acc_weighted$id <- as.character(acc_weighted$id)
colnames(acc_weighted)
acc_weighted$id <- ifelse(nchar(acc_weighted$id)==7,paste0(acc_weighted$id,'.00'),acc_weighted$id)

acc_weighted.high <- subset(acc_weighted, select = c(id,
                                                     highwjobs_nepdf,
                                                     highwjobs_necdf,
                                                     highwjobs_gauss,
                                                     highwjobs_fisk))
                                                    #highwjobs_ne01))
colnames(acc_weighted.high)

access_master_high <-left_join(acc_unweighted, acc_weighted.high, by = 'id')
colnames(access_master_high,2)
write.csv(access_master_high,"outputs/access_master_highjobs_transit.csv")

access.high_slim <- subset(access_master_high, select = -c(id, percentile))
colnames(access.high_slim)

corr.mtx.high <- cor(access.high_slim, method = 'pearson')
write.csv(corr.mtx.high,"outputs/acc_corr_mtx.high_transit.csv")

corr.mtx.high <- read.csv("Outputs/acc_corr_mtx.high_transit.csv")

df1.high <- as.data.frame(corr.mtx.high)
#df1.high[ "rowname" ] <- rownames(df1.high)
names(df1.high)[names(df1.high)=='X'] <- 'rowname'
df2.high <- df1.high[1:24,]
df2.high$rowname <- as.numeric(df2.high$rowname)
df3.high <- subset(df2.high, select=c(rowname,
                                      highwjobs_nepdf,
                                      highwjobs_necdf,
                                      highwjobs_gauss,
                                      highwjobs_fisk))
                                     #highwjobs_ne01))
data_long.high <- gather(df3.high, Type, Correlation, highwjobs_nepdf:highwjobs_fisk, factor_key=TRUE)

write.csv(data_long.high,"Outputs/acc_corr_mtx.high_vis_transit.csv")


### VISUALIZE ==================================================================

# All jobs

data_long <- read_csv("Outputs/acc_corr_mtx.all_vis_transit.csv")

data_long$Type <- as.factor(data_long$Type)


# Rename factor levels
levels(data_long$Type) <- list("Negative Exponential PDF" = "allwjobs_nepdf",        
                               "Negative Exponential CDF" = "allwjobs_necdf",
                               "Gaussain CDF" = "allwjobs_gauss",
                               "Log-Logistic CDF" = "allwjobs_fisk")
                              # "Negative Exponential (0.01)" = "allwjobs_ne01")

data_long <- drop_na(data_long)

# Plot

ggplot(data_long, 
       aes(x = rowname, y = Correlation, group=Type)) + 
  geom_line(aes(color=Type), size=1) + 
  scale_color_manual(values=c("#0D5C91", "#9FC8E3", "#F9BF78", "#E27F08")) +
  geom_vline(aes(xintercept=46), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=48.8), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=67.75), colour="Black", linetype="dashed") +
  geom_text(aes(x=43, label="Median = 46", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=51.8, label="Mean = 48.8", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=64.75, label="Mean+SD = 67.75", y=0.45), colour="Black", angle=90, size=8) +
  #labs(y="Correlation Coefficient",
  #     x="Cummulative Opportunities Threshold (min)") +
  theme(legend.position = c(0.80, 0.3), text = element_text(size = 23), legend.title = element_blank()) + 
  scale_x_continuous(name="Cumulative Opportunities Threshold (min)", breaks = seq(0,120,10)) +
  scale_y_continuous(name="Correlation Coefficient", limits = c(0, 1), breaks = c(0, 0.3, 0.6, 0.9)) +
  ggtitle("Montreal")

ggsave("Outputs/fig2_transit_all_incomes.jpg", width = 10, height = 5.5)


# Low-income jobs

data_long.low <- read_csv("Outputs/acc_corr_mtx.low_vis_transit.csv")
data_long.low$Type <- as.factor(data_long.low$Type)

# Rename factor levels
levels(data_long.low$Type) <- list("Negative Exponential PDF" = "lowwjobs_nepdf",        
                               "Negative Exponential CDF" = "lowwjobs_necdf",
                               "Gaussain CDF" = "lowwjobs_gauss",
                               "Log-Logistic CDF" = "lowwjobs_fisk")
                             # "Negative Exponential (0.01)" = "lowwjobs_ne01")

#data_long.low <- drop_na(data_long.low)
# Plot

ggplot(data_long.low, 
       aes(x = rowname, y = Correlation, group=Type)) + 
  geom_line(aes(color=Type), size=1) + 
  scale_color_manual(values=c("#0D5C91", "#9FC8E3", "#F9BF78", "#E27F08")) +
  geom_vline(aes(xintercept=44), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=46.7), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=64.93), colour="Black", linetype="dashed") +
  geom_text(aes(x=41, label="Median = 44", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=49.7, label="Mean = 46.7", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=61.93, label="Mean+SD = 64.93", y=0.45), colour="Black", angle=90, size=8) +
  #labs(y="Correlation Coefficient",
  #     x="Cummulative Opportunities Threshold (min)") +
  theme(legend.position = c(0.80, 0.3), text = element_text(size = 23), legend.title = element_blank()) +  
  scale_x_continuous(name="Cumulative Opportunities Threshold (min)", breaks = seq(0,120,10)) +
  scale_y_continuous(name="Correlation Coefficient", limits = c(0, 1), breaks = c(0, 0.3, 0.6, 0.9)) +
  ggtitle("Montreal")

ggsave("Outputs/fig2_transit_low_incomes.jpg", width = 10, height = 5.5)


# High-income jobs

data_long.high <- read_csv("Outputs/acc_corr_mtx.high_vis_transit.csv")
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
  geom_vline(aes(xintercept=48), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=50.6), colour="Black", linetype="dashed") +
  geom_vline(aes(xintercept=69.85), colour="Black", linetype="dashed") +
  geom_text(aes(x=45, label="Median = 48", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=53.6, label="Mean = 50.6", y=0.45), colour="Black", angle=90, size=8) +
  geom_text(aes(x=66.85, label="Mean+SD = 69.85", y=0.45), colour="Black", angle=90, size=8) +
  #labs(y="Correlation Coefficient",
  #     x="Cummulative Opportunities Threshold (min)") +
  theme(legend.position = c(0.80, 0.3), text = element_text(size = 23), legend.title = element_blank()) + 
  scale_x_continuous(name="Cumulative Opportunities Threshold (min)", breaks = seq(0,120,10)) +
  scale_y_continuous(name="Correlation Coefficient", limits = c(0, 1), breaks = c(0, 0.3, 0.6, 0.9)) +
  ggtitle("Montreal")

ggsave("Outputs/fig2_transit_high_incomes.jpg", width = 10, height = 5.5)

  