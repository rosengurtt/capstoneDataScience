source("source/shiny/wordPrediction.R")

EvaluatePredictorWithFile <- function(filepath, categories, categoriesDiscriminatingWords=NULL,
                              lookupTables, maxNgram, useGeneric=FALSE){
    goodGuesses <- list(c(0,0,0,0), c(0,0,0,0))
    badGuesses <- c(0, 0)
    couldntGuess <- c(0, 0)
    unguessable <- ""
    matchedWordsTotal <-  list(c(0,0,0,0), c(0,0,0,0))
    subject <- ""
    logActivity(paste("Processing", filepath))
    
    if (useGeneric){
        subject <- "generic"
        subjectIndex <- 1
    }
    else{
        con <- file(filepath,"r")
        firstLines <- readLines(con, n=100)
        close(con)
        subjectInfo <- getSubjectAndConfidence(cleanInput(firstLines), categories, 
                                               categoriesDiscriminatingWords)
        subject <- subjectInfo$subject
        subjectIndex <- subjectInfo$index
    }
    logActivity(paste("Subject is ", subject))

    textToEvaluate <- cleanInput(readLines(filepath))
    if (subject != "Unknown"){
        for (line in textToEvaluate){
            processedLine <- ""
            wordIndex <- 1
            while (wordIndex < str_count(line, "\\S+") + 1){
                textSoFar <- getFirstNwordsFromText(line, wordIndex)
                prediction <- predictNextWord(subjectIndex, textSoFar, 
                                              lookupTables, maxNgram)
                truth <- getTrueNextWordFromText(line, wordIndex) 
                matchedWords <- prediction$matchedWords
                if (matchedWords > 0){
                    matchedWordsTotal[[1]][[matchedWords]] <- 
                        matchedWordsTotal[[1]][[matchedWords]]+ 1
                }
                if (prediction$nextWord == truth){
                    goodGuesses[[1]][[matchedWords]] <- goodGuesses[[1]][[matchedWords]] + 1
               }
                else{
                    if (prediction$nextWord == "?????"){
                        couldntGuess[1] <- couldntGuess[1] + 1
                        unguessable <- c(unguessable, getLastNwordsFromText(textSoFar, 1))
                    }
                    else{
                        badGuesses[1] <- badGuesses[1] + 1
                    }
                }    
                # Process now firs n words plus first letter of next word
                textSoFarPlus1letter <- getFirstNwordsPlus1letter(line, wordIndex)
                prediction<- predictNextWord(subjectIndex, textSoFarPlus1letter, 
                                             lookupTables, maxNgram)
                
                matchedWords <- prediction$matchedWords
                
                if (matchedWords > 0){
                    matchedWordsTotal[[2]][[matchedWords]] <- 
                        matchedWordsTotal[[2]][[matchedWords]]+ 1
                }
                if (prediction$nextWord == truth){
                    goodGuesses[[2]][[matchedWords]] <- goodGuesses[[2]][[matchedWords]]+ 1
                }
                else{
                    if (prediction$nextWord == "?????"){
                        couldntGuess[2] <- couldntGuess[2] + 1
                    }
                    else{
                        badGuesses[2] <- badGuesses[2] + 1
                    }
                }
                wordIndex <- wordIndex + 1
            }
        }
    }
    df <- data.frame(good1word=goodGuesses[[1]][[1]],
                   good2word=goodGuesses[[1]][[2]],
                   good3word=goodGuesses[[1]][[3]],
                   good4word=goodGuesses[[1]][[4]],
                   bad=badGuesses[1],
                   couldntMatch=couldntGuess[1],
                   matched1word=matchedWordsTotal[[1]][[1]],
                   matched2word=matchedWordsTotal[[1]][[2]],
                   matched3word=matchedWordsTotal[[1]][[3]],
                   matched4word=matchedWordsTotal[[1]][[4]],
                   good1wordPlus1l=goodGuesses[[2]][[1]],
                   good2wordPlus1l=goodGuesses[[2]][[2]],
                   good3wordPlus1l=goodGuesses[[2]][[3]],
                   good4wordPlus1l=goodGuesses[[2]][[4]],
                   badPlus1l=badGuesses[2],
                   couldntMatchPlus1l=couldntGuess[2],
                   matched1wordPlus1l=matchedWordsTotal[[2]][[1]],
                   matched2wordPlus1l=matchedWordsTotal[[2]][[2]],
                   matched3wordPlus1l=matchedWordsTotal[[2]][[3]],
                   matched4wordPlus1l=matchedWordsTotal[[2]][[4]])
    return(df)
}

# Reads 1 file with sample text for each subject and passes it to EvaluatePrtedictor
# The n parameter determines the maximum number for the n grams. Setting n=5 for
# example, will use sequences of up to 5 words (5-grams) to make predictions
EvaluateAllCategories <- function(maxNgram = 5){
    lookupTablesPath <- file.path("data","lookup.tables")
    inputFolder <- file.path("data", "test", "input")
    discriminatingWordsFolder <- file.path("data","discriminating.words")
    outputFolder <- file.path("data", "test", paste("results.", maxNgram,"gram", sep=""))
    categories <- getCategories(discriminatingWordsFolder)
    categoriesDiscriminatingWords <- lapply(file.path(discriminatingWordsFolder, categories), 
                                            getCategoryData)
    lookupTables <- getLookupTables(lookupTablesPath, categories, maxNgram)
    
    files <- list.files(inputFolder, full.names = F)
    for (f in files){
        inputFile <- file.path(inputFolder, f)
        result <- EvaluatePredictorWithFile(inputFile, categories, 
                                    categoriesDiscriminatingWords, lookupTables, maxNgram)
        outputFile <- file.path(outputFolder, f)
        write.csv(result, outputFile)
        logActivity(paste("Saved", outputFile))
    }
}
EvaluateGeneric <- function(maxNgram = 5){
    lookupTablesPath <- file.path("data","generic.lookup.tables")
    inputFolder <- file.path("data", "test", "input")
    outputFolder <- file.path("data", "test", paste("results.generic", maxNgram,"gram", sep=""))
    lookupTables <- getLookupTables(lookupTablesPath, categories, maxNgram)
    
    logActivity("Reading test files")
    files <- list.files(inputFolder, full.names = F)
    for (f in files){
        inputFile <- file.path(inputFolder, f)
        result <- EvaluatePredictor(filepath=inputFile, categories=categories, 
                                    categoriesDiscriminatingWords=NULL, lookupTables=lookupTables,
                                    useGeneric=T)
        outputFile <- file.path(outputFolder, paste(f,"generic", sep="."))
        write.csv(result, outputFile)
        logActivity(paste("Saved", outputFile))
    }
    sink(NULL)
}

