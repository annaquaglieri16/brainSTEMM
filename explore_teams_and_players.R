## ----include=FALSE-------------------------------------------------------
library(png)
library(grid)
library(here)
knitr::opts_chunk$set(echo = TRUE, message = FALSE)

## ----echo=FALSE,message=FALSE,fig.width=9,fig.height=9,fig.align='center',cache=TRUE----
img <- readPNG(file.path(here(),"Figures/Download.png"))
grid.raster(img)

## ----echo=FALSE,message=FALSE,fig.width=8,fig.height=8,fig.align='center',cache=TRUE----
img <- readPNG(file.path(here(),"Figures/OpenRstudioProject.png"))
grid.raster(img)

## ----echo=FALSE,message=FALSE,fig.width=10,fig.height=9,fig.align='center',cache=TRUE----
img <- readPNG(file.path(here(),"Figures/RStudioOpen.png"))
grid.raster(img)

## ----echo=FALSE,message=FALSE,fig.width=10,fig.height=8,fig.align='center',cache=TRUE----
img <- readPNG(file.path(here(),"Figures/Rcode.png"))
grid.raster(img)

## ----message=FALSE-------------------------------------------------------
library(readr) # Load the package 'readr' in order to read .csv files into R
players <- read_csv("data/players.csv")
# The following two lines are simply a way to clean up the names of the columns
colnames(players) <- gsub(" ","_",colnames(players))
colnames(players)[colnames(players) %in% "Time_On_Ground_%"] <- "Time_On_Ground_prop"

## ------------------------------------------------------------------------
library(knitr) # package knitr allows to print a dataset on screen in a nicer way. Compare the two ways below.
head(players) # print the first 5 rows of the dataset players
kable(head(players))

## ------------------------------------------------------------------------
players$Club[1:10]

## ------------------------------------------------------------------------
players$Kicks_TOT[1:10]

## ------------------------------------------------------------------------
players$Kicks_AVG[1:10]

## ------------------------------------------------------------------------
table(players$Club)

## ------------------------------------------------------------------------
barplot(table(players$Club),main="Number of players in each club")

## ------------------------------------------------------------------------
library(ggplot2)
ggplot(data = players,aes(x=Club,fill=Club)) + geom_bar() + theme_bw()
ggplot(data = players,aes(x=Club,fill=Club)) + geom_bar() + theme_bw() + coord_flip()

## ------------------------------------------------------------------------
table(players$Kicks_TOT)
summary(players$Kicks_TOT)

## ------------------------------------------------------------------------
hist(players$Kicks_TOT,main="Total number of kicks")

## ------------------------------------------------------------------------
ggplot(data = players,aes(x=Kicks_TOT)) + geom_histogram(colour="white") + theme_bw()

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


## ----eval=FALSE----------------------------------------------------------
## library(plotly)
## 
## ggplotly(ggplot(data = players,aes(x = Kicks_TOT, y = Handballs_TOT,label=Player,label=Club)) + geom_point() + theme_bw() + ggtitle("Total kicks by Total goals (2017-1018)") + coord_flip() + facet_wrap(~Year))
## 

## ----echo=FALSE,message=FALSE,fig.width=5,fig.height=5,fig.align='center',cache=TRUE----
img <- readPNG(file.path(here(),"Figures/Runchunck.png"))
grid.raster(img)

## ----echo=FALSE,message=FALSE,fig.width=5,fig.height=5,fig.align='center',cache=TRUE----
img <- readPNG(file.path(here(),"Figures/stop_shinyApp.png"))
grid.raster(img)

## ----eval=TRUE-----------------------------------------------------------
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(plotly)

players <- read_csv("data/players.csv")
teams <- read_csv("data/teams.csv")
colnames(players) <- gsub(" ","_",colnames(players))
realvars <- c("Kicks_TOT", "Handballs_TOT", "Disposals_TOT", "Marks_TOT", "Frees_Agst_TOT", "Goals_TOT", "Behinds_TOT", "Goal_assists_TOT",
              "Time_On_Ground_prop")
colnames(players)[colnames(players) %in% "Time_On_Ground_%"] <- "Time_On_Ground_prop"
catvars <- c("Player", "Club")
clubs <- unique(players$Club)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Exploring the AFLW statistics"),
    tabsetPanel(
      tabPanel("Data",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          selectInput('x', "X", realvars, realvars[1]),
          selectInput('y', "Y", realvars, realvars[2]),
          selectInput('label', "Label", catvars),
          radioButtons('clr', "Colour by club:", c("None", clubs))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("scatterplot")
        )
      )
    ),
    tabPanel("Players",
     sidebarLayout(
       sidebarPanel(
         radioButtons('year', "Year", c("2017", "2018"), "2018"),
         checkboxGroupInput('vars', "Variables to use:", realvars, realvars[1:3])
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("mds")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$scatterplot <- renderPlotly({
     p <- ggplot(players,
            aes_string(x = input$x, y = input$y,
                       label = input$label)) +
       geom_point(alpha = 0.8) + labs(x=input$x,y=input$y)+
       facet_wrap(~Year, ncol=2) + theme_bw()
     if (input$clr != "None") {
       players$Clubclr <- "no"
       players$Clubclr[players$Club == input$clr] <- "yes"
       p <- p + aes(colour=players$Clubclr) +
         scale_colour_brewer(palette="Dark2",name=input$clr) +
         #theme(legend.position = "none")
         theme(legend.position = "bottom")+
         theme_bw()
     }
     ggplotly(p)
   })

   output$mds <- renderPlotly({
     players_sub <- players %>%
       filter(Year == input$year) %>%
       select(input$vars)
     players_sub_mat <- as.matrix(players_sub)
     players_mds <- cmdscale(dist(players_sub_mat), k=2)
     players_mds_df <- as_tibble(players_mds)
     players_mds_df$Player <- players$Player[players$Year == input$year]
     p2 <- ggplot(players_mds_df, aes(x=V1, y=V2, label=Player)) + geom_point() +theme_bw()
     ggplotly(p2, tooltip=c("label"))
   })

}

# Run the application
shinyApp(ui = ui, server = server)



## ----eval=TRUE-----------------------------------------------------------
purl("explore_teams_and_players.Rmd")

