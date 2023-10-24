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


makeJBox = function(n, prefix, multiple=1, width=NULL, height=height) {
  box(actionButton(paste0(prefix, n), paste0("$", n*100*multiple), 
                   style='width:120%; height:140px; font-size:100%'), 
      width=12, height=height)
}

makeJColList = function(prefix, width=NULL, height=height, multiple=1) {
  JColumns = list()
  for (n in 1:5) {
    JColumns = c(JColumns, list(makeJBox(n, prefix, multiple=multiple,
                                         width=width, height=height)))
  }
  return(JColumns)
}

makeJHColumn = function(prefix, width=2, height=150, multiple=1) {
  list(column(width=width, 
              #div(style = "text-align: center;", 
              div(style=paste("text-align:center;",
                  "box-shadow: 10px 10px 5px #888888;",
                  "width:100%;",
                  "height:100px;",
                  #"padding-top:70px;",
                  #"position:relative':,
                  ";"),
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
                          sidebarLayout(
                            sidebarPanel(width=0),
                            mainPanel(
                              column(width=12,
                                box(textOutput("gameNameText"), width=12, height=30),
                                box(shinyFilesButton("inputFile", "Select a game",
                                                     "Please select a game file", 
                                                      multiple=FALSE),
                                    width=12, height=50),
                                box(selectInput("AQSeparator",
                                                "Separator between Answer and Question",
                                                choices=c("|", "/", "\\", "&"),
                                                selected="|", selectize=TRUE), 
                                    width=12, height=80),
                                box(actionButton("quitApp", "Quit Jeopardy"),
                                    width=12, height=50)
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
                                      MoreArgs=list(multiple=1, width=2))
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
                             ), # end fluidRow()
                           width=12) # end mainPanel()
                         )),
                
                # Question Panel
                tabPanel("Question",
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(
                             fluidRow(
                               box(textOutput("categoryReminder"),
                                   width=12, height=100),
                               box(htmlOutput("selectedAnswer"),
                                       width=12, height=400)
                              )
                           )
                         ))
) # end NavbarPage



