# Looks for all files 
CleanAllFiles <- function(){
    rawFilesFolder <- file.path("data","raw")
    cleanFilesFolder <- file.path("data", "clean")
    inputFiles <-list.files(rawFilesFolder)
    for (f in inputFiles){
        inputFile <- file.path(rawFilesFolder, f)
        f <-  gsub(pattern = 'rawdata',  replacement = 'cleandata', f)
        outputFile <-  file.path(cleanFilesFolder, f)
        CleanFile(inputFile, outputFile)
    }
}

# Convert to lowercase, remove characters that are not letters/dashes/apostrophes,
# extra white spaces and profanities
CleanFile <- function(inputFile, outputFile){
    library(stringr)
    library(qdap)
    
    logActivity("Reading data.")
    # Read data
    myData <- readLines(inputFile, skipNul = TRUE)
    
    # Remove metadata
    myData <- gsub(pattern = '<doc id=.*$|</doc>.*',  replacement = '', myData)
    
    # Convert the text to lower case
    logActivity("Converting to lowercase.")
    myData <-gsub(pattern = '([[:upper:]])', perl = TRUE, replacement = '\\L\\1', myData)
    
    # Remove urls
    logActivity("Removing urls.")
    urlPattern <- "http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+"
    myData<-str_replace_all(myData, urlPattern, " ")
    
    # Remove special chars    
    logActivity("Removing special characters and numbers.")
    myData <-str_replace_all(myData, "[^A-Za-z ]", " ")
    
    # Collapse white spaces
    logActivity("Removing extra white spaces.")
    spacePattern <- "\\s+"
    myData<-str_replace_all(myData, spacePattern, " ")
    
    # Remove empty lines
    logActivity("Removing empty lines.")
    empty_lines = grepl('^\\s*$', myData)
    myData <- myData[! empty_lines]
    
    # Save cleaned data
    logActivity(paste("Saving ", outputFile))
    writeLines(myData, outputFile)
}


logActivity <- function(text){
    print(paste(text, Sys.time(), sep=" "))
}
