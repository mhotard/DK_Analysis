setwd("~/Documents/DraftKings/Data/weeks")


library(dplyr)
library(tidyr)
options(dplyr.print_max = 1e9)

#data pulled from http://rotoguru1.com/cgi-bin/fyday.pl?game=dk
# http://rotoguru1.com/cgi-bin/fstats.cgi?pos=0&sort=4&game=p&colA=0&daypt=0&xavg=0&inact=0&maxprc=99999&outcsv=1


#get all the data files in the folder
file_list <- list.files()
list.files()

file_list

#create blank data frame
football <- data.frame()



#binds all the data together
for (f in file_list){
  temp <- read.csv2(f)
  football <- rbind(football, temp)
}

sal <- read.csv2('../salary')
sal


#correction for incorrect data
football[388,9:10] <- c(2, 2400)

head(football)

#corrects a character error
football$DK.points <- as.numeric(as.character((football$DK.points)))


names(football)
summarize(group_by(football,Pos), 
          max(DK.points), 
          mean(DK.points),
          min(DK.points))

names(football)

#creates a sorting function
good_sort <- function(position){
  football %>%
  filter(Pos==position) %>%
  group_by(Name) %>%
  summarize(avg_points = mean(DK.points), salary = mean(DK.salary),
            ppm = mean(DK.points)/mean(DK.salary)* 10000, 
            low = mean(DK.points)-sd(DK.points), 
            high = max(DK.points)) %>%
  arrange(desc(low)) %>%
  slice(1:15)}
  
good_sort('QB')
good_sort('RB')
good_sort('WR')
good_sort('TE')
good_sort('Def')






#going to simulate teams with expected points and some variabtion accounted for

#just getting some of columns we are interesetd in
football2 <- select(football, Name, Pos, GID, DK.points, Week)
football2


#adding week to make the column names not be numbers
football2$Week <- sprintf('Week%i', football2$Week)

football2

#melting the data to be in a wide format
football_wide <- spread(football2, Week, DK.points)
head(football_wide)

?spread

#getting all the weeks so far
time <- c(names(football_wide[-(1:3)]))
time

#getting the average across all the weeks
#r requried an apply function to do this easily
average <- apply(football_wide[, time], 1, mean, na.rm=TRUE)

#getting the standard deviation across all the weeks for each player
stdev <- apply(football_wide[, time], 1, sd, na.rm=TRUE)

#imputing an average standard deviation is there was not one
avg_sd <- mean(stdev, na.rm=TRUE)
stdev[is.na(stdev)] <- avg_sd

#adding the average and the stdev columns
f_wide <- cbind(football_wide, average, stdev)



#function that takes a team and makes simulated points
sample_team_func <- function(samp_team){
  #1000 draws
  samp_size <- 10000
  
  #creating empty data_frame
  team_sim <- data.frame(matrix(nrow=samp_size, ncol=9))
  
  #getting number of players to create points for
  num_players <- seq(1:nrow(samp_team))

  #doing a normal draw based on each players average and sddev
  for (i in num_players){
    team_sim[,i] <- rnorm(samp_size, samp_team[i,'average'], samp_team[i, 'stdev'])
  }
  #summing all the rows
  points <- rowSums(team_sim)
  return(points)
}

#picking nine top players for the team
top_team <- f_wide %>% 
  arrange(desc(average)) %>%
  slice(1:9)

#getting points for the TOP TEAM, they are good
sample_team_func(top_team)
hist(sample_team_func(top_team))


#creates a random team with the correct numbers
random_team <- function(){
  QB <- f_wide %>%
    filter(Pos=='QB') %>%
    sample_n(1)
  
  RB <- f_wide %>%
    filter(Pos=='RB') %>%
    sample_n(2)
  
  TE <- f_wide %>%
    filter(Pos=='TE') %>%
    sample_n(1)
  
  WR <- f_wide %>%
    filter(Pos=='WR') %>%
    sample_n(4)
  
  DEF <- f_wide %>%
    filter(Pos=='Def') %>%
    sample_n(1)
  
  return(rbind(QB, RB, TE, WR, DEF))
}

#getting points for a random team
new_team <- random_team()
new_team
sample_team_func(new_team)


#take a simulated teams points and compares how many you would have won
winning_percent <- function(points, threshold){
  return(sum(points>threshold) / length(points))
}

#random teams are not very good
wins <- sample_team_func(random_team())
winning_percent(wins, 100)

#top teams are very good
winning_percent(sample_team_func(top_team), 200)


#install.packages("lpSolve")

library(lpSolve)


#website I'm basing my code on
#http://pena.lt/y/2014/07/24/mathematically-optimising-fantasy-football-teams/
#browser()
#getting the salary for week 4
#this is not perfect
#need to find a way to get updated salaries

####TRYING WITH FLEX constraints


#our player number constraints
#the flex is because you get 7 total from the RB, WR, and TE positions
num_qb = 1
num_def = 1 
num_wr = 3
num_rb = 2
num_te = 1
num_flex = 7
max_cost = 50000

#website I'm basing my code on
#http://pena.lt/y/2014/07/24/mathematically-optimising-fantasy-football-teams/
  
  #getting the salary for week 4
  #this is not perfect
  #need to find a way to get updated salaries

salaries <- sal %>%
  select(GID, Name, Salary)

#linking the salaries to the f_wide DF 
full_mat <- left_join(f_wide, salaries, by=c('GID'))

#reshaping to get rid of some unwanted columns and rename Name again
full_mat <- full_mat %>%
  rename(Name = Name.x) %>%
  select(-Name.y)

#creating dummy variables for the positions
#also filtering out salries that are 0 or NA
est_mat <- full_mat %>%
  select(Name, Pos, Salary, average, stdev) %>%
  na.omit() %>%
  filter(Salary > 0) %>%
  mutate(QB = ifelse(Pos=='QB', 1, 0)) %>%
  mutate(RB = ifelse(Pos=='RB', 1, 0)) %>%
  mutate(WR = ifelse(Pos=='WR', 1, 0)) %>%
  mutate(TE = ifelse(Pos=='TE', 1, 0)) %>%
  mutate(DEF = ifelse(Pos=='Def', 1, 0)) %>%
  mutate(FLEX = ifelse(Pos %in% c('RB', 'WR','TE'), 1, 0))

#setting the constraint directions
#QB, RB, WR, TE, FLEX(7), DEF(1), salary(50000)
const_dir <- c("=", ">=", ">=", ">=", "=", "=", "<=")

#setting the goals
objective = est_mat$average

#creating the matrix of positions
const_mat = matrix(c(est_mat$QB, est_mat$RB, est_mat$WR,
                     est_mat$TE, est_mat$FLEX, est_mat$DEF, 
                     est_mat$Salary), nrow=(7),
                   byrow=TRUE)

#setting the constraints together
const_rhs = c(num_qb, num_rb, num_wr, num_te, num_flex, num_def,
              max_cost)


x = lp("max", objective, const_mat, const_dir, const_rhs,
       all.bin=TRUE, )

#this shows up the team
est_mat[x$solution==1,]

opt_team <- est_mat[x$solution==1,]
#this is their estimated pointed
sum(est_mat[x$solution==1,]$average)
#this is a histogram from estimated points
hist(sample_team_func(opt_team))
winning_percent(sample_team_func(opt_team), 180)



#####Now trying to look a ten different teams and see how variable their win shares are

#linking the salaries to the f_wide DF 
full_mat <- left_join(f_wide, salaries, by=c('GID'))

#reshaping to get rid of some unwanted columns and rename Name again
full_mat <- full_mat %>%
  rename(Name = Name.x) %>%
  select(-Name.y)

#creating dummy variables for the positions
#also filtering out salries that are 0 or NA
est_mat <- full_mat %>%
  select(Name, Pos, Salary, average, stdev) %>%
  na.omit() %>%
  filter(Salary > 0) %>%
  mutate(QB = ifelse(Pos=='QB', 1, 0)) %>%
  mutate(RB = ifelse(Pos=='RB', 1, 0)) %>%
  mutate(WR = ifelse(Pos=='WR', 1, 0)) %>%
  mutate(TE = ifelse(Pos=='TE', 1, 0)) %>%
  mutate(DEF = ifelse(Pos=='Def', 1, 0)) %>%
  mutate(FLEX = ifelse(Pos %in% c('RB', 'WR','TE'), 1, 0))

#we want to modify est to get ride of each player in our optimum team

#optimizing team matrix, takes matrix of all players with the dummy variables already made
optimize_team <- function(player_mat){
  
  num_qb = 1
  num_def = 1 
  num_wr = 3
  num_rb = 2
  num_te = 1
  num_flex = 7
  max_cost = 50000
  
  const_dir <- c("=", ">=", ">=", ">=", "=", "=", "<=")
  
  #setting the goals
  objective = player_mat$average
  
  #creating the matrix of positions
  const_mat = matrix(c(player_mat$QB, player_mat$RB, player_mat$WR,
                       player_mat$TE, player_mat$FLEX, player_mat$DEF, 
                       player_mat$Salary), nrow=(7),
                     byrow=TRUE)
  
  #setting the constraints together
  const_rhs = c(num_qb, num_rb, num_wr, num_te, num_flex, num_def,
                max_cost)
  
  x = lp("max", objective, const_mat, const_dir, const_rhs,
         all.bin=TRUE, )
  
  #this shows up the team
  return(player_mat[x$solution==1,])
  
}

dim(est_mat)



opt_team <- optimize_team(est_mat)

team_list <- list()
?list

for (j in 1:9) {
  print(j)
  print((j+1) %% 9 + 1)
  print((j+3) %% 9 + 1)
  matrix_test <- est_mat %>%
    filter(Name != opt_team$Name[j],
           Name != opt_team$Name[(j+1) %% 9 + 1],
           Name != opt_team$Name[(j+4) %% 9 + 1])
  
  temp_team <- (optimize_team(matrix_test))
  team_list[j] <- list(temp_team)
}

hist(sample_team_func(team_list[[3]]))
winning_percent(sample_team_func(opt_team), 180)

win_list <- rep(NA, 9)

for (j in 1:9){
  win_list[j] <- winning_percent((sample_team_func(team_list[[j]])), 180)
}

team_list[[9]]
sum(opt_team$average)
sum(team_list[[9]]$Salary)

win_list

