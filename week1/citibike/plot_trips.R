########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)
library(tidyr)
library(lubridate)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(tripduration)) + geom_histogram() + xlim(0, 60*100)
# plot the distribution of trip times by rider type
ggplot(trips, aes(x = tripduration, fill = usertype)) + geom_histogram() + xlim(0, 60*100)
# plot the number of trips over each day
ggplot(trips, aes(x = ymd)) + geom_histogram()
# plot the number of trips by gender and age

# plot the ratio of male to female trips by age
# hint: use the spread() function to reshape things to make it easier to compute this ratio

########################################
# plot weather data
########################################
# plot the minimum temperature over each day
ggplot(weather, mapping = aes(x = ymd, y = tmin)) + geom_point()

# plot the minimum temperature and maximum temperature over each day
# hint: try using the gather() function for this to reshape things before plotting

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>% group_by(ymd) %>% summarise(num = n()) %>% inner_join(weather, by="ymd") %>% ggplot(aes(x = tmin, y = num)) + geom_point() + geom_smooth()

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
trips_with_weather %>% group_by(ymd) %>% summarise(num = n()) %>% inner_join(weather, by="ymd") %>% mutate(sub_percip = prcp > 40) %>% ggplot(aes(x = tmin, y = num, color = sub_percip)) + geom_point() + geom_smooth()

# add a smoothed fit on top of the previous plot, using geom_smooth

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
View(trips %>% mutate(hourOfDay = hour(starttime)) %>% select(starttime, ymd, hourOfDay) %>% group_by(hourOfDay, ymd) %>% mutate(numPerDay = n()) %>% group_by(hourOfDay) %>% summarise(avg = mean(numPerDay), sdTrips = sd(numPerDay)))

# plot the above
ggplot(tripsHourDay, aes (x = hourOfDay, y = avg)) + geom_ribbon(ymin = (tripsHourDay$avg - tripsHourDay$sdTrips), ymax = (tripsHourDay$avg + tripsHourDay$sdTrips) )

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
 #facet_wrap()
#dayOfTheWeek using wday column group_by(ymd, dayOfWeek, hour) then group_by(dayOfWeek, hour), mean = mean(trips), sd = sd(trips)