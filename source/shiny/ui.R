#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
  
  # Application title
  tags$h1("Swift Key (next word predictor)", style="font-size:40px;text-align: center"),
  tags$hr(style="border-color: purple; height: 30px;border-width: 3px;"),
  conditionalPanel(condition="$('#AppDescriptionText').text().length==0",
                   tags$h2("Loading, please wait...",id="loadmessage")),
  # Sidebar with a slider input for number of bins 
  fillPage(

          htmlOutput("AppDescriptionText", container = tags$h3),
          tags$textarea(id="UserInput",  class="form-control col-xs-12", 
                        style="margin-top:30px;margin-bottom:30px;font-size:20px;", rows=8),
          htmlOutput("DetectedSubject", container = tags$h2),
          htmlOutput("NextWord", container = tags$h2) 
          
  )
))
