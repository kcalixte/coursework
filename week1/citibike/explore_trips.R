library(dplyr)

load('trips.RData')

# count the number of trips (= rows in the data frame)

NROW(trips) #[1] 5370361
# find the earliest and latest birth years (see help for max and min to deal with NAs)
> max(trips$birth_year, na.rm = TRUE)
#[1] 1998
> min(trips$birth_year, na.rm = TRUE)
#[1] 1899
# use filter and grepl to find all trips that either start or end on broadway
broadway <- filter(trips, grepl('Broadway', start_station_name) | grepl('Broadway', end_station_name))
# do the same, but find all trips that both start and end on broadway
broadway <- filter(trips, grepl('Broadway', start_station_name), grepl('Broadway', end_station_name))
# use filter, select, and distinct to find all unique station names
trips %>% distinct(start_station_name) %>% select(start_station_name)
# count trips by gender
count(trips, gender, sort = TRUE)
# find the 10 most frequent station-to-station trips
count(trips, start_station_name, end_station_name, sort = TRUE)[1:10,]
#count all trips that start and end on broadway
count(filter(trips, grepl('Broadway', start_station_name), grepl('Broadway', end_station_name)))

