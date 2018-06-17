dataPath <- "/home/jose/cursos/dataScience/capstone/work_wiki"

produceGenericNgrams <- function(){
    library(stringr)
    library(textmineR)
    library(textTinyR)
    library(dplyr)
    library(data.table)
    
    ngramsFolder <- file.path("data", "ngrams")
    outputFolder <- file.path("data", "generic.ngrams")
    
    for (n in 1:5){
        accumulator <- data.frame(term=character(), freq=numeric(), stringsAsFactors=FALSE) 
        categories <- list.dirs(ngramsFolder, recursive = F, full.names = F)

        for (category in categories){
            ngramFile <- file.path(ngramsFolder, category, paste("ngram", n, sep="."))
            logActivity(paste("Processing file", ngramFile))
            nGramData <- read.csv(ngramFile, stringsAsFactors = F, header = F)
            names(nGramData) <- c("term", "freq")
            accumulator <- rbind(accumulator, nGramData)
        }
        nGramData<-setDT(accumulator)[, list(sum(freq, na.rm=TRUE)), by=term]
        names(nGramData) <- c("term", "freq")
        
        nGramData <- nGramData[order(term)] 

        outputFile <-  file.path(outputFolder, paste("ngram", n, sep="."))
        logActivity(paste("Saving file", outputFile))
        write.table(nGramData, outputFile, row.names = F, col.names = F, sep=",")
    }
}


# Creates all the tables to be used by the algorithm to predict the next word
CreateAllLookupTables <- function(){
    ngramsFolder <- file.path("data", "generic.ngrams")
    lookupTablesFolder <- file.path("data", "generic.lookup.tables")
    categories <- list.dirs(ngramsFolder, recursive = F, full.names = F)

    for (n in 2:5){
        CreateLookupTable(ngramsFolder, lookupTablesFolder, n)
    }
}

# Creates a lookup table for a specific category and a specific number of 
# consecutive words  
CreateLookupTable <- function(ngramsFolder, lookupTablesFolder, n){
    library(dplyr)
    
    if (n < 2 | n > 5) return
    
    logActivity("Reading file.")
    nGramFile <- file.path(ngramsFolder, paste("ngram", n, sep="."))
    nGram <- read.csv(nGramFile, stringsAsFactors = F, header = F)
    names(nGram) <- c("term", "freq")
    
    logActivity(paste("Generating lookup table for n=", n, sep=" "))
    nGram$prefix <- gsub("\\s*\\w*$", "", nGram$term)
    nGram$nextWord <- sapply(nGram$term, GetLastWord)
    nGram$term <- NULL
    
    outputFile <- file.path(lookupTablesFolder, paste("lookup.table", n, sep="."))
    write.table(nGram, outputFile, row.names = F, col.names = F, sep =",")
}

# Utility that returns the last word of a text
GetLastWord<- function(text){
    library(stringr)
    pattern <- paste("(?:\\S*\\s*){1}$", sep="")
    word <- str_extract(text, pattern)
    return(word[[1]])
}
