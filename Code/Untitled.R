setwd("~/Documents/DraftKings/Data")


library(dplyr)
options(dplyr.print_max = 1e9)

#data pulled from http://rotoguru1.com/cgi-bin/fyday.pl?game=dk

#get all the data files in the folder
file_list <- list.files()

#create blank data frame
football <- data.frame()

#binds all the data together
for (f in file_list){
  temp <- read.csv2(f)
  football <- rbind(football, temp)
}

#correction for incorrect data
football[388,9:10] <- c(2, 2400)

#corrects a character error
football$DK.points <- as.numeric(football$DK.points)

names(football)
summarize(group_by(football,Pos), 
          max(DK.points), 
          mean(DK.points),
          min(DK.points))

names(football)

#creates a sorting function
good_sort <- function(position)
  football %>%
  filter(Pos==position) %>%
  group_by(Name) %>%
  summarize(avg_points = mean(DK.points), salary = mean(DK.salary),
            ppm = mean(DK.points)/mean(DK.salary)* 10000, 
            low = mean(DK.points)-sd(DK.points), 
            high = max(DK.points)) %>%
  arrange(desc(low)) %>%
  slice(1:15)
  
good_sort('QB')
good_sort('RB')
good_sort('WR')
good_sort('TE')
good_sort('Def')

#good_sort <- function(position)
  football %>%
 # filter(Pos==position) %>%
  group_by(Name) %>%
  summarize(avg_points = mean(DK.points), salary = mean(DK.salary),
            ppm = mean(DK.points)/mean(DK.salary)* 10000, 
            low = mean(DK.points)-sd(DK.points), 
            high = max(DK.points)) %>%
  arrange(desc(low)) %>%
  slice(1:15)

series <- football %>%
 filter(Pos=='QB') %>%
  filter(Name %in% 'Flacco, Joe') %>%
   group_by(Name) %>%
  select(DK.points)

filter(football, Pos=='QB')


flacco <- series$DK.points

flacco


max(fx)
max(hx)

mean(flacco)
mean(stafford)

fx <-dnorm(x, mean(flacco), sd(flacco))

plot(x, fx, col='red')
lines(x, hx, col='blue')

??standard deviation
  
  
  x <- seq(-60, 60, length=100)
  hx <- dnorm(x, mean(stafford), sd(stafford))
  fx <-dnorm(x, mean(flacco), sd(flacco))
  
  
  
  
  degf <- c(1, 3, 8, 30)
  colors <- c("red", "blue", "darkgreen", "gold", "black")
  labels <- c("df=1", "df=3", "df=8", "df=30", "normal")
  
  plot(x, hx, type="l", lty=2, xlab="x value",
       ylab="Density", main="Comparison of t Distributions")
  
  for (i in 1:4){
    lines(x, dt(x,degf[i]), lwd=2, col=colors[i])
  }
  
  legend("topright", inset=.05, title="Distributions",
         labels, lwd=2, lty=c(1, 1, 1, 1, 2), col=colors)
