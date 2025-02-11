#Practical 2

#Generate Data 
set.seed(1)
x <- seq(1,100)
err <- rnorm(100, mean = 0, sd = 0.2)
y <- sin(x/10)+err 
f <- 0.5
#create function
#customLowess <- function(x, y, f){
  k <- f*length(x)
  yi <- c()
  for (i in 1:length(x)){
    w <- c()
    B <- c()
    closest_indices <- order(abs(x - x[i]))[1:k+1]
    closest_indices <- sort(closest_indices)
    dmax <- max(abs(closest_indices-x[i])) 
    for (j in closest_indices){
      wj = integer()
      wj = (1-(abs(x[j]-x[i])/dmax)^3)^3  
      w=c(w,wj)
      
    }}
    b = solve(t(closest_indices)%*%w%*%closest_indices)%*%t(closest_indices)%*%w%*%y
    yi = c(yi,b[1]+b[2]*xi)
  } 
  
  return('Smoothed Values'=yi)
  }

closest_indices <- order(abs(x - 37))[1:k]  
sort(closest_indices)
ifelse(max(closest_indices-37)==(k/2)
#compare results
lowess(x,y,0.5,iter=0)
customLowess(x,y,0.5)


#prac 5
# Install tidyverse if not already installed
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(tidyverse)

# Install and load nycflights13 for flight data
if (!requireNamespace("nycflights13", quietly = TRUE)) {
  install.packages("nycflights13")
}
library(nycflights13)
#1
dplyr::data_frame(nycflights13::flights)

#2
tbl = nycflights13::flights 
flight1 <- tbl |> dplyr::filter(tbl$month==1) 
dist_tbl <- flight1  |> group_by(carrier) |> summarise(mean_dist = mean(distance,na.rm = TRUE),sd_dist = sd(distance,na.rm = TRUE))
dist_tbl |> arrange(mean_dist,sd_dist)

#3
dist

#4
tbl = nycflights13::flights
dist_tbl <- tbl  |> group_by(carrier,month) |> summarise(mean_delay = mean(dep_delay,na.rm = TRUE))

dist_tbl |> pivot_wider(names_from =carrier ,values_from = mean_delay)
#5
tbl = nycflights13::flights
numerator <- tbl |> filter(dep_delay>0 & arr_delay<=0) |> tally()
denominator <- tally(tbl)
numerator/denominator

#6
flights <- nycflights13::flights
airlines <- nycflights13::airlines

#routes that more than one airline flies

Routes <- flights|> unite(Route,origin, dest, sep = " to ") |> select(Route,carrier) |>distinct()
DuplicateRoutes <- Routes|> filter(duplicated(Route)) |> select(Route) |> distinct()

#For each such route, calculate the average arrival delay for each airline (exclude NAs). Find the names of these airlines

Avg_delay_routes <- flights|> group_by(origin,dest,carrier) |>  summarise(mean_delay = mean(dep_delay,na.rm = TRUE)) 
Avg_delay_routes |> filter(duplicated(Route)) |> select(Route) |> distinct()

