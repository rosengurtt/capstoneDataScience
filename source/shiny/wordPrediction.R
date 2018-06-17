library(stringr)
library(data.table)
library(qdap)


# For each category we have a folder. By geting the list of folders,
# we get the list of categories. In this way, to add a category we
# just have to add the folder with the category data
getCategories <- function(folder){
    categories <- list.dirs(folder, full.names = F)
    #categories <- gsub(pattern = '\\.\\/',  replacement = '', categories)
    # Remove the "." folder
    categories[!categories==""]
}

getCategoryData <- function(folder){
    discrWords <- read.csv(file.path(folder,"discriminating.words"), 
                           stringsAsFactors = F, header = F)
    names(discrWords) <- c("term", "freq")
    return(discrWords)
}


getLookupTablesForCategory <- function(folder, category, maxNgram=3){
    returnList <- list()
    for (n in 2:maxNgram){
        filepath <- file.path(folder, category, paste("lookup.table", n, sep="."))
        lookup <- fread(filepath, header = F)
        names(lookup) <- c("freq", "previousWords", "nextWord")
        setkey(lookup, previousWords)  
        returnList[[n-1]] <- lookup
    }
    return(returnList) 
}

getLookupTables <- function(folder, categories, maxNgram=3){
    returnList <- list()
    for (n in 1:length(categories)){
        returnList[[n]] <- getLookupTablesForCategory(folder, categories[n], maxNgram)
    }
    return(returnList) 
}


getSubjectLikelyhood <-function(text, subjectDiscrWords){
    words <- bag_o_words(text)
    sum <- 0
    for (word in words){
        prob <- subjectDiscrWords[subjectDiscrWords$term == word,]$freq
        if (length(prob) > 0){
            sum <- sum + prob
        }
    }
    return(sum)
}


getSubjectAndConfidence <- function(text, categories, categoriesDiscriminatingWords){
    probabilities <- vector()
    for (catDiscrWords in categoriesDiscriminatingWords){
        probabilities <- c(probabilities, getSubjectLikelyhood(text, catDiscrWords))
    }
    maxValue <- max(probabilities)
    if (maxValue > 0){
        averageValue <- mean(probabilities)
        subjectIndex <- which.max( probabilities )
        subject <- categories[subjectIndex]
        confidence <- (maxValue-averageValue)*100/maxValue
        confidence <- round(confidence, 1)
        return(data.frame(subject=subject, confidence=confidence, index=subjectIndex))
    }
    else{
        return(data.frame(subject="Unknown"))
    }
}


predictNextWord <- function(subjectIndex, inputText, lookupTables, maxNgram=3){
    for (n in maxNgram:2){
        print(paste("maxNgram",maxNgram))
        print(paste("subjectIndex",subjectIndex))
        lookup <- lookupTables[[subjectIndex]][[n-1]]
        print("pase la bosta")
        
        #First try to make a match assuming the last word is partially typed
        lastWords <- getLastNwordsFromText(inputText, n)
        lastWord <- getLastNwordsFromText(lastWords, 1)
        firstNminus1 <- getFirstNwordsFromText(lastWords, n-1)
        matchedFirstWords <- lookup[lookup$previousWords == firstNminus1]
        
        if (nrow(matchedFirstWords) > 0){
            matchedAll <- matchedFirstWords[grep(paste("^", lastWord, sep=""), matchedFirstWords$nextWord ), ]
             if (nrow(matchedAll) > 0){
                # Get the one with the highest freq
                prediction <- matchedAll[which.max(matchedAll$freq),]
                if (nchar(prediction$nextWord) > nchar(lastWord)){
                    return(data.frame(nextWord=prediction$nextWord, matchedWords=n-1, 
                                      lastWordIsComplete=FALSE))
                }
            }
        }
        lastWords <- getLastNwordsFromText(inputText, n-1)
        matches <- lookup[lookup$previousWords == lastWords]
        
        if (nrow(matches) > 0){
            prediction <- matches[which.max(matches$freq),]
            return(data.frame(nextWord=prediction$nextWord, matchedWords=n-1, 
                              lastWordIsComplete=TRUE))
        }
    } 
    return(data.frame(nextWord="?????", matchedWords=0))
}

getLastNwordsFromText <- function(text, n){
    pattern <- paste("(?:\\S*\\s*){", n, "}$", sep="")
    str_extract(text, pattern)
}


getFirstNwordsFromText <- function(text, n){
    if (n > 0){
        pattern <- paste("^((?:\\S+\\s+){", n-1, "}\\S+).*", sep="")
        gsub(pattern=pattern, perl = TRUE, replacement = '\\L\\1', text)
    }
}
getFirstNwordsPlus1letter <- function(text, n){
   firstN <- getFirstNwordsFromText(text, n)
   firstNplusOne <- getFirstNwordsFromText(text, n+1)
   lastWord <- getLastNwordsFromText(firstNplusOne, 1)
   firstLetterOfLastWord <- substring(lastWord, 1, 1)
   return(paste(firstN, firstLetterOfLastWord, sep=" "))
}

getTrueNextWordFromText <- function(text, n){
    firstNplusOneWords <- getFirstNwordsFromText(text, n + 1)
    getLastNwordsFromText(firstNplusOneWords, 1)
}

cleanInput <- function(inputText){
    library(stringr)
    # Convert the text to lower case
    inputText <-gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', inputText)
    
    # Remove urls
    urlPattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    inputText<-str_replace_all(inputText, urlPattern, " ")
    
    # Remove special chars    
    inputText <- str_replace_all(inputText, "[^A-Za-z ]", " ")
    
    # Collapse white spaces
    spacePattern <- "\\s+"
    inputText <- trimws(str_replace_all(inputText, spacePattern, " "))
    
    return(inputText)
}

logActivity <- function(text){
    print(paste(text, Sys.time(), sep=" "))
}
