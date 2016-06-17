########################################
# load libraries
########################################

# load some packages that we'll need
library(dplyr)
library(ggplot2)
library(reshape)
library(scales)

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

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the minimum temperature over each day
ggplot(weather, mapping = aes(x = ymd, y = tmin)) + geom_point()

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>% group_by(ymd) %>% summarise(num = n()) %>% inner_join(weather, by="ymd") %>% ggplot(aes(x = tmin, y = num)) + geom_point() + geom_smooth()

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

# add a smoothed fit on top of the previous plot, using geom_smooth
