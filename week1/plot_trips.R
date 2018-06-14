########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)
library(lubridate)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides
ggplot(trips, aes(x = tripduration)) +
  geom_histogram(bins = 50) +
  scale_x_log10(label = comma, breaks = c(1, 3, 10, 30, 100, 300, 1e3, 3e3, 1e4, 3e4, 1e5)) +
  scale_y_log10(label = comma)

# plot the distribution of trip times by rider type
ggplot(trips, aes(x = tripduration)) +
  geom_histogram(bins = 50) +
  facet_wrap(~ usertype, scale = "free") +
  scale_x_log10(label = comma, breaks = c(1, 3, 10, 30, 100, 300, 1e3, 3e3, 1e4, 3e4, 1e5))
  
# plot the total number of trips over each day
ggplot(trips, aes(x = as.Date(starttime))) +
  geom_histogram(bins = 50) + 
  xlab('Date') 

# plot the total number of trips (on the y axis) by age (on the x axis) and age (indicated with color)
trips %>%
  mutate(age = year(ymd) - birth_year) %>% 
  group_by(age) %>%
  summarize(n=n()) %>%
  ggplot(aes(x = age, y = n, color = age)) +
  geom_point() + 
  ylab('count') 

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")

# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this

# add a smoothed fit on top of the previous plot, using geom_smooth

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
