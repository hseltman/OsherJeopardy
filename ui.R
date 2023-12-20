#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyFiles)
library(shinyalert)
library(shinyjs)


# For JColList(), make one dollar amount box
makeJBox = function(n, prefix, multiple=1, width=NULL, height=height) {
  box(actionButton(paste0(prefix, n), paste0("$", n*100*multiple), 
                   style='width:110%; height:65px; font-size:150%'), 
      width=12, height=height)
}

# For makeJHColumn(), make the five dollar amounts
makeJColList = function(prefix, width=NULL, height=height, multiple=1) {
  JColumns = list()
  for (n in 1:5) {
    JColumns = c(JColumns, list(makeJBox(n, prefix, multiple=multiple,
                                         width=width, height=height)))
  }
  return(JColumns)
}

# Make one Jeopardy board column including category and five dollar amounts
makeJHColumn = function(prefix, width=2, height=75, multiple=1) {
  list(column(width=width, 
              div(style=paste("text-align:center;",
                  "border-style: solid;",
                  "margin: 2px 2px 8px 1px;",
                  "width:100%;",
                  "white-space: no-wrap;",
                  "height: 200px;",
                  "text-align: justify;",
                  "overflow: auto;",
                  "font-size: 150%;",
                  ")"), # end style()
                  box(textOutput(prefix), height=100)),
       makeJColList(prefix, width=width, height=height, 
                           multiple=multiple)))
}

###########################
## Create User Interface ##
###########################

ui = navbarPage("Jeopardy Game", id="myNavbar", theme = shinytheme("flatly"),
                #tags$head(tags$style(HTML("pre { white-space: pre-wrap; word-break: keep-all; }"))),
                #tags$style(type = "text/css", ".navbar{padding-left:2px;
                #           padding-right:2px ; margin-right:2px; margin-left:2px;}"),
                # Intro Panel  
                tabPanel("Intro",
                         useShinyjs(),  # Set up shinyjs
                         sidebarLayout(
                            sidebarPanel(width=0),
                            mainPanel(
                              column(width=12,
                                box(textOutput("gameNameText"), width=12, height=30),
                                box(shinyFilesButton("inputFile", "Select a game",
                                                     "Please select a game file", 
                                                      multiple=FALSE),
                                    width=12, height=50),
                                box(p("Player names"), width=12, height=25),
                                box(textInput("P1Name", "1: "), width=4),
                                box(textInput("P2Name", "2: "), width=4),
                                box(textInput("P3Name", "3: "), width=4),
                                box(selectInput("AQSeparator",
                                                "Separator between Answer and Question",
                                                choices=c("|", "/", "\\", "&"),
                                                selected="|", selectize=TRUE),
                                    width=12),
                                box(actionButton("quitApp", "Quit Jeopardy"))
                              ), # end column()
                            width=12) # end mainPanel()
                          ) # end sidebarLayout()
                        ), # end tabPanel
                
                # Jeopardy Panel
                tabPanel("Jeopardy",
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(
                             fluidRow(
                               mapply(makeJHColumn, 
                                      prefix=paste0("jbs", LETTERS[1:6]),
                                      MoreArgs=list(multiple=1, width=2))),
                               fluidRow(column(width=4,
                                               box(style="text-align:center; font-size: 150%; color: blue;",
                                                   textOutput("jbP1Score"))),
                                        column(width=4,
                                               box(style="text-align:center; font-size: 150%; color: blue;",
                                                   textOutput("jbP2Score"))),
                                        column(width=4,
                                               box(style="text-align:center; font-size: 150%; color: blue;",
                                                   textOutput("jbP3Score")))
                               ), # end fluidRow()
                           width=12) # end mainPanel()
                         )),
                
                # Double Jeopardy Panel
                tabPanel("Double Jeopardy",
                  sidebarLayout(
                  sidebarPanel(width=0),
                  mainPanel(
                    fluidRow(
                      mapply(makeJHColumn, 
                             prefix=paste0("jbd", LETTERS[1:6]),
                             MoreArgs=list(multiple=2, width=2))
                    ), # end fluidRow() for category names and dollar values
                    fluidRow(column(width=4,
                                    box(style=scoreStyle,
                                        textOutput("djbP1Score"))),
                             column(width=4,
                                    box(style=scoreStyle,
                                        textOutput("djbP2Score"))),
                             column(width=4,
                                    box(style=scoreStyle,
                                        textOutput("djbP3Score")))
                             ), # end fluidRow()
                           width=12) # end mainPanel()
                         )),
                
                # Answer Panel
                tabPanel("Answer",
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(
                             fluidRow(box(textOutput("categoryReminder"),
                                          style="font-size: 220%;",
                                          width=12, height=100),
                                      box(htmlOutput("selectedAnswer"),
                                          style="font-size: 200%;",
                                          width=12, height=400)
                             ),  # end fluidRow() for category and selected answer 
                             fluidRow(column(width=4,
                                             box(style=scoreStyle,
                                                 textOutput("answerP1Score"))),
                                      column(width=4,
                                             box(style=scoreStyle,
                                                 textOutput("answerP2Score"))),
                                      column(width=4,
                                             box(style=scoreStyle,
                                                 textOutput("answerP3Score")))
                             ), # end fluidRow()
                             fluidRow(column(width=3,
                                             box(actionButton("P1Correct", "P1 correct"),
                                                 width=12, height=50),
                                             box(actionButton("P1Incorrect", "P1 incorrect"),
                                                 width=12, height=50)
                                      ), # end column() for Player 1
                                      column(width=3,
                                             box(actionButton("P2Correct", "P2 correct"),
                                                 width=12, height=50),
                                             box(actionButton("P2Incorrect", "P2 incorrect"),
                                                 width=12, height=50)
                                      ), # end column() for Player 2
                                      column(width=3,
                                              box(actionButton("P3Correct", "P3 correct"),
                                                  width=12, height=50),
                                              box(actionButton("P3Incorrect", "P3 incorrect"),
                                                  width=12, height=50),
                                      ), # end column() for Player 3
                                      column(width=3,
                                              box(actionButton("backToAnswers", "Back to Answers"),
                                                  width=12, height=50),
                                              hidden(box(textInput("ddBet", "Bet", "0"),
                                                  width=12, height=50))
                                      ), # end column() for "backToAnswers" and daily double bet
                                      width=12
                             )  # end fluidRow() for player buttons
                           )  # end mainPanel()
                         ))  # end sidebarLayout() and tabPanel()
) # end NavbarPage



