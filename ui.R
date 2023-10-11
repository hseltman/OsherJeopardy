#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyjs)


makeJBox = function(n, prefix, multiple=1, width=NULL, height=100) {
  box(actionButton(paste0(prefix, n), paste0(n*100*multiple)), 
      width=NULL, height=height)
}

makeJColList = function(prefix, width=NULL, height=100, multiple=1) {
  JColumns = list()
  for (n in 1:5) {
    JColumns = c(JColumns, list(makeJBox(n, prefix, multiple=multiple,
                                         width=widht, height=height)))
  }
  return(JColumns)
}

makeJHColumn = function(label, prefix, width=NULL, height=100, multiple=1) {
  list(column(width=2, 
              div(style = "text-align: center;", 
                  actionButton("x2", label)),
              makeJColList(prefix, width=width, height=height, 
                           multiple=multiple)))
}

ui = navbarPage("Jeopardy Game", theme = shinytheme("flatly"),
                tabPanel("Jeopardy",
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(
                             fluidRow(
                               mapply(makeJHColumn, tecateg, 
                                      prefix=paste0("jbs", LETTERS[1:6]),
                                      MoreArgs=list(multiple=1))
                             )
                           )
                         )),
                tabPanel("Double Jeopardy",
                         sidebarLayout(
                           sidebarPanel(width=0),
                           mainPanel(
                             fluidRow(
                               mapply(makeJHColumn, paste(tecateg, 2), 
                                      prefix=paste0("jbs", LETTERS[1:6]),
                                      MoreArgs=list(multiple=2))
                             )
                           )
                ))
)



