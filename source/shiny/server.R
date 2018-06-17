#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(qdap)
library(pryr)
source("wordPrediction.R")

categories <- getCategories("data")
categoriesDiscriminatingWords <- lapply(file.path("data", categories), getCategoryData)
lookupTables <- getLookupTables("data", categories)
print(mem_used())

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
      output$AppDescriptionText <- renderUI({
         HTML("This application tries to predict the next word you are going to write. 
              It also tries to detect which subject you are writing about, and uses 
              this information to make a better prediction. 
              I trained it with texts about 4 subjects: computers, health, math and  
              politics, so whatever you type will be categorized as one of these matters.<br/>
              Type or paste text in the box below, 
              and the detected subject and predicted next word will be shown.")
      })
    
      output$DetectedSubject <- renderText({ 
          if (input$UserInput != ""){
              cleanedInputText <- cleanInput(input$UserInput)
              subjectInfo<- getSubjectAndConfidence(cleanedInputText, categories,
                                                    categoriesDiscriminatingWords)
             if (subjectInfo$subject != "Unknown"){
                     paste("The subject is <b>", subjectInfo$subject, "</b> 
                    with confidence <b>", subjectInfo$confidence, "%</b>")
             }
              else{
                  "Couldn't determine the subject. By default, assume subject is computing"
              }
          }
      })
      output$NextWord <- renderText({ 
          if (input$UserInput != ""){
              cleanedInputText <- cleanInput(input$UserInput)
              subjectInfo <- getSubjectAndConfidence(cleanedInputText, categories, 
                                                     categoriesDiscriminatingWords)
              if (subjectInfo$subject != "Unknown"){
                  subjectIndex <- subjectInfo$index
              }
              else{
                  subjectIndex <- 1
              }
              match <-predictNextWord(subjectIndex, cleanedInputText, lookupTables)
              if (match$nextWord != "?????"){
                  if (match$lastWordIsComplete){
                    paste("Predicted word is <big><b>", match$nextWord, "</b></big> having matched", 
                        match$matchedWords, "words")
                  }
                  else{
                      paste("The word you are typing is <big><b>", match$nextWord, "</b></big> having matched", 
                            match$matchedWords, "words")
                  }
              }
              else{
                  "Can't predict next word."
              }
              
          }
      })
  
})


