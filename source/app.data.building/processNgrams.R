# For each file in the clean data folder, it builds tables of ngrams, with n=1..5
# Each ngram table is saved in a different file.
# For each input file, it saves its data in an associated subfolder inside the
# ngramsFolder
CreateAllNgrams <- function(){
    cleanDataFolder <- file.path("data", "clean")
    ngramsFolder <- file.path("data","ngrams")
    cleanFiles <-list.files(cleanDataFolder)
    for (n in 1:5){
        for (f in cleanFiles){
            inputFile <- file.path(cleanDataFolder, f)
            category <- gsub(".cleandata", "", f)
            ngramFileName <- paste("ngram", n, sep=".")
            outputFile <-  file.path(ngramsFolder, category, ngramFileName)
            CreateNgramsForFile(inputFile, outputFile, n)
        }
    }
}

# Uses the tm package to build the ngrams for a given input file
CreateNgramsForFile <-function(inputFile, outputFile, n){
    library(tm)
    library(RWeka)
    library(stringr)
    library(textmineR)
    library(textTinyR)
    library(dplyr)
    library(data.table)
    
    logActivity("Reading file")
    myData <- readLines(inputFile, skipNul = TRUE)
    logActivity("Creating corpus")
    docs <- VCorpus(VectorSource(myData))
    processCorpora(docs, n, outputFile, FALSE)
    if (n==1){
        ProcessCorpora(docs, n, paste(outputFile, "no.Stop.Words", sep="."), TRUE)
    }
}

# Utility called by createNgramsForFile
ProcessCorpora <- function(corpora, n, outputFile, stopWords){
    if (stopWords){
        corpora <- tm_map(corpora, removeWords, stopwords("english"))
    }
    myTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = n, max = n)) 
    logActivity("Creating dtm")
    dtm <- DocumentTermMatrix(corpora, control = list(tokenize = myTokenizer))
    dtm_sparse <- MakeSparseDTM(dtm)  
    nGramsFreq <- data.frame(dtm$dimnames$Terms, sparse_Sums(dtm_sparse, rowSums = FALSE), stringsAsFactors=F)
    
    names(nGramsFreq) <- c("term", "freq")
    logActivity("Sorting.")
    orderedNgrams <- nGramsFreq[order(nGramsFreq$term),] 
    logActivity("Saving file.")  
    write.table(orderedNgrams, outputFile, row.names = F, col.names = F, sep=",")
}

# Creates all the tables to be used by the algorithm to predict the next word
CreateAllLookupTables <- function(){
    ngramsFolder <- file.path("data", "ngrams")
    lookupTablesFolder <- file.path("data", "lookup.tables")
    categories <- list.dirs(ngramsFolder, recursive = F, full.names = F)
    for (category in categories){
        # list.dirs returns not only subdirs, but the "." folder, skip it
        if (category == ".") next
        for (n in 2:5){
            CreateLookupTable(ngramsFolder, lookupTablesFolder, category, n)
        }
    }
}

# Creates a lookup table for a specific category and a specific number of 
# consecutive words  
CreateLookupTable <- function(ngramsFolder, lookupTablesFolder, category, n){
    library(dplyr)

    if (n < 2 | n > 5) return
    
    logActivity("Reading file.")
    nGramFile <- file.path(ngramsFolder, category, paste("ngram", n, sep="."))
    nGram <- read.csv(nGramFile, stringsAsFactors = F, header = F)
    names(nGram) <- c("term", "freq")

    logActivity(paste("Generating lookup table for", category, "n=", n, sep=" "))
    nGram$prefix <- gsub("\\s*\\w*$", "", nGram$term)
    nGram$nextWord <- sapply(nGram$term, GetLastWord)
    nGram$term <- NULL
   
    outputFile <- file.path(lookupTablesFolder, category, paste("lookup.table", n, sep="."))
    write.table(nGram, outputFile, row.names = F, col.names = F, sep =",")
}

# Utility that returns the last word of a text
GetLastWord<- function(text){
    library(stringr)
    pattern <- paste("(?:\\S*\\s*){1}$", sep="")
    word <- str_extract(text, pattern)
    return(word[[1]])
}


# Calculates the frequencies of each word using all the raw data from all subjects
# available. This data is used to find the discriminating words
CreateGenericWordFrequencies <- function(){
    library(tm)
    library(RWeka)
    library(stringr)
    library(textmineR)
    library(textTinyR)
    library(dplyr)
    library(data.table)
    
    ngramsFolder <- file.path("data", "ngrams")
    discriminatingWordsFolder <- file.path("data", "discriminating.words")
    accumulator <- data.frame(term=character(), freq=numeric(), stringsAsFactors=FALSE)
    categories <- list.dirs(ngramsFolder, recursive = F, full.names = F)
    
    for (category in categories){
        wordFrequencyFile <- file.path("data", "ngrams", category, "ngram.1.no.Stop.Words")
        logActivity(paste("Processing file", wordFrequencyFile, sep = " "))
        nGramData <- read.csv(wordFrequencyFile, stringsAsFactors = F, header = F)
        names(nGramData) <- c("term", "freq")
        accumulator <- rbind(accumulator, nGramData)
    }
    nGramData<-setDT(accumulator)[, list(sum(freq, na.rm=TRUE)), by=term]
    names(nGramData) <- c("term", "freq")
    
    df <- nGramData[order(term)] 
    total_term_instances <- sum(df$freq)
    df_rel_freq<-data.frame(df$term, df$freq/total_term_instances, stringsAsFactors = F)
    
    logActivity("Saving file.")
    outputFile <- file.path(discriminatingWordsFolder, "Word.Frequencies.Generic")
    write.table(df_rel_freq, outputFile, row.names = F, col.names = F, sep=",")
}

CreateDiscriminatingWordsFiles <- function(){
    ngramsFolder <- file.path("data", "ngrams")
    discriminatingWordsFolder <- file.path("data", "discriminating.words")
    wordFregGenericFile <- file.path(discriminatingWordsFolder, "Word.Frequencies.Generic")
    
    wordFreqGeneric <- read.csv(wordFregGenericFile, stringsAsFactors=F, header=F)
    names(wordFreqGeneric) <- c("term", "freqGen")
    
    categories <- list.dirs(ngramsFolder, recursive = F, full.names = F)
    
    for (category in categories){
        wordFreqFile <- file.path(ngramsFolder, category, "ngram.1.no.Stop.Words")
        print(paste("Processing", wordFreqFile, sep=" "))
        
        wordFreqCategory <- read.csv(wordFreqFile, stringsAsFactors=F, header=F)
        names(wordFreqCategory) <- c("term", "freqCat")
        total_term_instances <- sum(wordFreqCategory$freq)
        wordFreqCategory <- data.frame(wordFreqCategory$term, wordFreqCategory$freqCat/total_term_instances, stringsAsFactors = F)
        names(wordFreqCategory) <- c("term", "freqCat")
        
        merged <- merge(wordFreqGeneric, wordFreqCategory)
        merged$dif <- merged$freqCat - merged$freqGen
        merged <- merged[order(-merged$dif),]
        merged <- merged[merged$dif>0,]
        merged$freqCat <- NULL
        merged$freqGen <- NULL
        
        outputFile <- file.path(discriminatingWordsFolder, category,
                                "discriminating.words")
        write.table(merged, sep=",", outputFile, 
                    row.names=FALSE, col.names = F)
    }
}

logActivity <- function(text){
    print(paste(text, Sys.time(), sep=" "))
}
