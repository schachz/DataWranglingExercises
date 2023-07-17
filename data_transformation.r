library(nycflights13)
library(tidyverse)


head(flights)

# Flights w/ arrival delay > 2hours 
delay2ormore <- filter(flights, arr_delay >= 120)
delay2ormore


#Flights to IAH or HOU
Houston <- filter(flights, dest == "IAH" | dest == "HOU")
Houston


#Flights operated by United, American, Delta.
Big3 <- filter(flights, carrier == "UA" | carrier == "DL" | carrier == "AA")
Big3


#Flights in July, August, September
summer <- filter(flights, month == 7 | month == 8 | month == 9)
summer



#Arrived > than two hours late but didn’t leave late
lateArival <- filter(flights, dep_delay <= 0, arr_delay >= 120)
lateArival



#Delayed by 60 minutes or more but made up over 30 minutes in flight
madeuptime <- filter(flights, dep_delay >= 60, arr_delay <= -30)
madeuptime


#Departed from midnight (000) to 6:00AM (600) (inclusive)
redeye <- filter(flights, dep_time >= 0, dep_time <= 600)
redeye


df <- redeye

#Finding missing values
arrange(flights, desc(is.na(dep_time)), dep_time)


#Sort flights to find the most delayed flights. Find the flights that left earliest.

arrange(flights, desc(dep_delay))

arrange(flights, dep_delay)



#What does the any_of() function do? Why might it be helpful in conjunction with this vector?

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

flights %>% select(any_of(vars))



#Any of function allows for the inclusion of missing variables and also can be used to remove or isolate columns. Since the column names are stored in the vector we can easily manipulate the simple "Var" vector.


#Currently dep_time and sched_dep_time are convenient to look at, but hard to compute with because they’re not really continuous numbers. Convert them to a more convenient representation of number of minutes since midnight.

clean_times <- mutate(flights, clean_dep_time = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440, 
clean_sched_dep_time = (sched_dep_time %/% 100 * 60 + sched_dep_time %% 100) %% 1440)

clean_times

select(clean_times,dep_time,clean_dep_time,sched_dep_time, clean_sched_dep_time)



minutes <- function(x) {
  (x %/% 100 * 60 + x %% 100) %% 1440
}


clean_times <- mutate(flights, clean_dep_time = minutes(dep_time),clean_sched_dep_time = minutes(sched_dep_time))
clean_times

#Compare dep_time, sched_dep_time, and dep_delay. How would you expect those three numbers to be related?
#We should expect the departure delay to be equal to ... Departure Time minus Schedule Departure Time.

departures <- mutate(clean_times,clean_dep_time,clean_sched_dep_time,diffindelay = dep_delay - clean_dep_time + clean_sched_dep_time)
departures


filter(departures, diffindelay != 0)


# Which carrier has the worst delays?
 

flightsbycarrier <- flights %>% group_by(carrier)

flightsbycarrier %>% summarise(dep_delay = mean(arr_delay, na.rm = TRUE))

#F9 has the worst delays with an average departure delay time of 21.9 min.

airlines
