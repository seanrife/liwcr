library(qdap)
library(utils)
liwcr <- function(inputText, dict) {
  if(!file.exists(dict)) 
    stop("Dictionary file does not exist.")
  
  # Read in dictionary categories
  # Start by figuring out where the category list begins and ends
  dictionaryText <- readLines(dict)
  if(!length(grep("%", dictionaryText))==2)
    stop("Dictionary is not properly formatted. Make sure category list is correctly partitioned (using '%').")
  
  catStart <- grep("%", dictionaryText)[1]
  catStop <- grep("%", dictionaryText)[2]
  dictLength <- length(dictionaryText)
  
  dictionaryCategories <- read.table(dict, header=F, sep="\t", skip=catStart, nrows=(catStop-2))
  
  wordCount <- sum(na.omit(word_count(inputText)))
  
  outputFrame <- dictionaryCategories
  outputFrame["count"] <- 0
  outputFrame["percent"] <- 0
  
  # Now read in dictionary words
  
  no_col <- max(count.fields(dict, sep = "\t"), na.rm=T)
  dictionaryWords <- read.table(dict, header=F, sep="\t", skip=catStop, nrows=(dictLength-catStop), fill=TRUE, quote="\"", col.names=1:no_col)
  
  workingMatrix <- wfdf(inputText)
  for (i in workingMatrix[,1]) {
    if (i %in% dictionaryWords[, 1]) {
      occurrences <- 0
      foundWord <- dictionaryWords[dictionaryWords$X1 == i,]
      foundCategories <- foundWord[1,2:no_col]
      for (w in foundCategories) {
        if (!is.na(w) & (!w=="")) {
          existingCount <- outputFrame[outputFrame$V1 == w,]$count
          outputFrame[outputFrame$V1 == w,]$count <- existingCount + workingMatrix[workingMatrix$Words == i,]$all
          outputFrame[outputFrame$V1 == w,]$percent <- ((existingCount + workingMatrix[workingMatrix$Words == i,]$all)/wordCount)*100
        }
      }
    }
  }
  return(outputFrame)
}