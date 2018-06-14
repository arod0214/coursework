#!/bin/bash
#
# add your solution after each of the 10 comments below

# count the number of unique stations
cut -d, -f4 201402-citibike-tripdata.csv | tail -n+2 | sort | uniq -c | wc -l
cut -d, -f8 201402-citibike-tripdata.csv | tail -n+2 | sort | uniq -c | wc -l # to check if both values are equal

# count the number of unique bikes
cut -d, -f12 201402-citibike-tripdata.csv | tail -n+2 | sort | uniq -c | wc -l

# count the number of trips per day
cut -d, -f2 201402-citibike-tripdata.csv | tail -n+2 | sort | cut -d ' ' -f1 | uniq -c | sort -rn

# find the day with the most rides
cut -d, -f2 201402-citibike-tripdata.csv | tail -n+2 | sort | cut -d ' ' -f1 | uniq -c | head -n1

# find the day with the fewest rides
cut -d, -f2 201402-citibike-tripdata.csv | tail -n+2 | sort | cut -d ' ' -f1 | uniq -c | tail -n1

# find the id of the bike with the most rides
cut -d, -f12 201402-citibike-tripdata.csv | tail -n+2 | sort | uniq -c | sort -rn | head -n1

# count the number of rides by gender and birth year
cut -d, -f14,15 201402-citibike-tripdata.csv | tail -n+2 | sort | uniq -c | sort -rn

# count the number of trips that start on cross streets that both contain numbers (e.g., "1 Ave & E 15 St", "E 39 St & 2 Ave", ...)
cut -d, -f5 201402-citibike-tripdata.csv | tail -n+2 | grep '.*[0-9].*&.*[0-9].*' | sort -rn | wc -l

# compute the average trip duration
cut -d, -f1 201402-citibike-tripdata.csv | tail -n+2 | tr '"' ' ' | awk '{sum+=$1; n++} END{print sum/n}'

# standard deviation of the trip duration
cut -d, -f1 201402-citibike-tripdata.csv | tail -n+2 | tr -d '"' | awk '{sum+=$1; n++} END{print ($1-(sum/n))^2/n}'
