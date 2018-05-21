## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## ----message=FALSE-------------------------------------------------------
library(readr) # Load the package 'readr' in order to read .csv files into R
players <- read_csv("data/players.csv")
colnames(players) <- gsub(" ","_",colnames(players))
colnames(players)[colnames(players) %in% "Time_On_Ground_%"] <- "Time_On_Ground_prop"

## ------------------------------------------------------------------------
library(knitr) # package knitr allows to print a dataset on screen in a nicer way. Compare the two ways below.
head(players)
kable(head(players))

## ------------------------------------------------------------------------
#View(players)

## ------------------------------------------------------------------------
players$Club[1:10]

## ------------------------------------------------------------------------
players$Kicks_TOT[1:10]

## ------------------------------------------------------------------------
players$Kicks_AVG[1:10]

## ------------------------------------------------------------------------
table(players$Club)

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(data = players,aes(x=Club,fill=Club)) + geom_bar() + theme_bw()
ggplot(data = players,aes(x=Club,fill=Club)) + geom_bar() + theme_bw() + coord_flip()

## ------------------------------------------------------------------------
table(players$Kicks_TOT)
summary(players$Kicks_TOT)

## ------------------------------------------------------------------------
ggplot(data = players,aes(x=Kicks_TOT)) + geom_histogram(colour="white") + theme_bw()
# Alternative for continous variables: Boxplot
ggplot(data = players,aes(x="Tot kicks",y=Kicks_TOT)) + geom_boxplot() + theme_bw()
# alternative way of producing a boxplot
boxplot(players$Kicks_TOT)

## ------------------------------------------------------------------------
library(dplyr)
kicks_by_team <- players %>% group_by(Year,Club) %>%
summarise(Tot.kicks = sum(Kicks_TOT))
kicks_by_team

## ------------------------------------------------------------------------
ggplot(data = players,aes(x = Club, y = Kicks_TOT)) + geom_bar(position="dodge",stat="identity") + theme_bw() + facet_wrap(~Year)

# Add title
ggplot(data = players,aes(x = Club, y = Kicks_TOT,fill=factor(Year))) + geom_bar(position="dodge",stat="identity") + theme_bw() + ggtitle("Total kicks by club by year (2017-1018)")

# Flip coordinate and colour by year
ggplot(data = players,aes(x = Club, y = Kicks_TOT,fill=factor(Year))) + geom_bar(position="dodge",stat="identity") + theme_bw() + ggtitle("Total kicks by club by year (2017-1018)") + coord_flip()

# Plot total number of goals instead of kicks
ggplot(data = players,aes(x = Club, y = Goals_TOT,fill=factor(Year))) + geom_bar(position="dodge",stat="identity") + theme_bw() + ggtitle("Total kicks by club by year (2017-1018)") + coord_flip()


## ------------------------------------------------------------------------
# Kicks by goal
ggplot(data = players,aes(x = Kicks_TOT, y = Goals_TOT)) + geom_point() + theme_bw() + ggtitle("Total kicks by Total goals (2017-1018)") + coord_flip()

ggplot(data = players,aes(x = Kicks_TOT, y = Goals_TOT)) + geom_point() + theme_bw() + ggtitle("Total kicks by Total goals (2017-1018)") + coord_flip() + facet_wrap(~Year)

# Kicks by handballs
ggplot(data = players,aes(x = Kicks_TOT, y = Handballs_TOT)) + geom_point() + theme_bw() + ggtitle("Total kicks by Total goals (2017-1018)") + coord_flip()

ggplot(data = players,aes(x = Kicks_TOT, y = Handballs_TOT)) + geom_point() + theme_bw() + ggtitle("Total kicks by Total goals (2017-1018)") + coord_flip() + facet_wrap(~Year)


## ------------------------------------------------------------------------
library(plotly)

ggplotly(ggplot(data = players,aes(x = Kicks_TOT, y = Handballs_TOT,label=Player,label=Club)) + geom_point() + theme_bw() + ggtitle("Total kicks by Total goals (2017-1018)") + coord_flip() + facet_wrap(~Year))


## ------------------------------------------------------------------------
purl("explore_teams_and_players.Rmd")

