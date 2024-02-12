# Print a Jeopardy game for the host
# Call without ".txt"
# Makes "*Cheat.txt"
# My printer does 90 columns and 64 lines
# Across is 13x6 + 10 spaces = 88
# Down is 5 + 3 blank + 5*5 with 12 blank =  45
# Need 20 blanks at the end of round 1

JTxtToDoc <- function(fName, AQSeparator="|") {
  require(stringr)
  
  # "separator" bar pattern
  sbPattern = c(rep(c(FALSE, rep(TRUE, 5)), 6*2), FALSE, TRUE)
  
  # pattern of Categories vs Answer|Question pairs in the file input
  catAQPairPattern = c(rep(c(1, 5), 12), 1, 1)
  
  # Constants
  rowsPerPage <- 64
  numCategories <- 6
  perCategory <- 5
  allowedWidth <- 13
  betweenText <- "  "
  titleLines <- 2
  categoryLength <- 5
  questionLength <- 5
  betweenLines <- 3
  
  # Derived constants
  blankLine <- paste(rep(" ", allowedWidth), collapse="")
  separatorLine <- paste(rep("-", allowedWidth), collapse="")
  sepLocs <- seq(from=categoryLength+(betweenLines-1), by=questionLength+betweenLines, length=perCategory)
  blankLocs <- c(sepLocs-1, sepLocs+1)

  # Main helper function  
  textWrapper <- function(txt, width=13, length=3, pad=TRUE) {
    words <- strsplit(txt, " ", fixed=TRUE)[[1]]
    words <- substring(words, 1, width)
    if (length(words) == 1) {
      rslt <- c(str_pad(words, width, side="right"), rep(str_pad("", width, side="right"), length-1))
       return(rslt)
    }
    lengths <- nchar(words)
    cLengths <- cumsum(lengths) + 1
    cLengths[1] <- cLengths[1] - 1
    rslt <- rep("", length)
    for (line in 1:length) {
      n = max(which(cLengths <= width))
      rslt[line] <- paste(words[1:n], collapse=" ")
      if (length(words) == n) break
      words <- words[-(1:n)]
      lengths <- nchar(words)
      cLengths <- cumsum(lengths) + 1
    }
    if (pad) rslt <- str_pad(rslt, width, side="right")
    return(rslt)
  }
  
  
  inData <- try(readLines(paste0("gameboards/", fName, ".txt")))
  if (is(inData, "try-error")) return("Bad File")


  # Delete blank lines and comment lines
  inData = inData[!grepl("(^\\s*$)|(^\\s*#)", inData)]

  # Guess that user totally forgot data format
  if (length(inData) < 12) {
    return(paste0("File must contain 12 sets of 6 lines each ",
                    "(where line 1 is a Category Name, and lines ",
                    "2 to 5 are 'answer", AQSeparator, "question' pairs), ",
                    "followed by a Final Jeopardy Category, and then a Final ",
                    "Jeopardy 'answer", AQSeparator, "question' pair."))
  } 

  # Find which lines have a single bar (or 'AQSeparator') 
  singleBar = grepl(paste0("^([^", AQSeparator, "]+?)[", AQSeparator,
                           "]([^", AQSeparator, "]+)$"), inData)

  # Handle incorrect pattern
  if (length(singleBar) != length(sbPattern) ||  any(singleBar!=sbPattern)) {
    ## Report on various problems if not successful ##
    
    # Handle first part of pattern not 'FTTTTTF'
    if (singleBar[1]) {
      return(paste0("First line of game file must contain a category name ",
                    "(no '", AQSeparator, "')."))
    }
    if (!all(singleBar[2:6])) {
      return(paste0("Lines 2 to 6 of game file must contain 'answer",
                    AQSeparator, "question' pairs."))
    }
    if (singleBar[7]) {
      return(paste0("Line 7 of game file must contain a category name (no '",
                    AQSeparator, "')."))
    }
    
    # Use run length encoding to characterize pattern of categories and A|Q pairs
    catAQPair = rle(singleBar) 
    temp = catAQPair$lengths[catAQPair$values==FALSE]
    if (length(temp) != 13) {
      return(paste("File must contain 6 Jeopardy Categories, 6 Double",
                   "Jeopardy Categories, and one Final Jeopardy Category.",
                   "You have", length(temp), "categories."))
    }
    if (any(temp != 1)) {
      index = which(temp != 1)[1]
      return(paste0("It appears that the '", AQSeparator,
                    "' is missing in 'Answer", AQSeparator, "Question' for",
                    " category number ", index, "."))
    }
    temp = catAQPair$lengths[catAQPair$values==TRUE]
    if (length(temp) != 13) {
      return(paste0("File must contain 6 groups of 5 'Answer", AQSeparator,
                    "Question' pairs ",
                    "for Jeopardy, 6 groups of 5 'Answer", AQSeparator,
                    "Question' pairs for Double Jeopardy, and one Final ",
                    "Jeopardy 'Answer", AQSeparator, "Question' pair ",
                    "for a total of 61 pairs.  ",
                    "You have", sum(temp), "pairs in ", length(temp), "groups."))
    }
    if (temp[13] != 1) {
      return(paste0("There should be just one Final Jeopardy 'Answer",
                    AQSeparator, "Question' pair."))
    }
    if (any(temp[1:12] != 5)) {
      index = which(temp != 5)[1]
      return(paste0("Category ", index, " has ", temp[index], " 'Answer",
                    AQSeparator, "Question' pairs."))
    }
    return("Bad input", "Unhandled exception", type="error")
  } else {
    ## Input is all goood
    sjCatLoc = seq(1, by=6, length.out=6)
    sjCategories = inData[sjCatLoc]
    djCatLoc = seq(37, by=6, length.out=6)
    djCategories = inData[djCatLoc]
    fjCategory = inData[73]
    tc = textConnection(inData[1:36][-sjCatLoc])
    sjAQ = read.table(tc, sep=AQSeparator, quote="",
                      col.names=c("Answer", "Question"), comment.char="")
    close(tc)
    tc = textConnection(inData[37:72][-sjCatLoc]) # [sic]
    djAQ = read.table(tc, sep=AQSeparator, quote="",
                      col.names=c("Answer", "Question"), comment.char="")
    close(tc)
    temp = strsplit(inData[74], AQSeparator, fixed=TRUE)[[1]]
    fjAQ = data.frame(Answer=temp[1], Question=temp[2])
    
    # Do the formatting for printing Jeopardy
    jMatrix <- matrix(" ", rowsPerPage-titleLines, numCategories)
    parsedText <- sapply(sjAQ$Question, textWrapper, allowedWidth, questionLength)
    for (categ in 1:numCategories) {
      jMatrix[1:categoryLength, categ] <- textWrapper(sjCategories[categ], allowedWidth, categoryLength)
      jMatrix[sepLocs, categ] <- separatorLine
      jMatrix[blankLocs, categ] <- blankLine
      for (value in 1:perCategory) {
        jMatrix[(sepLocs[value]+2):(sepLocs[value]+1+questionLength), categ] <- 
          parsedText[, (categ-1)*perCategory+value]
      }
    }
    jMatrix <- apply(jMatrix, 1, paste, collapse=betweenText)

    # Do the formatting for printing Double Jeopardy
    djMatrix <- matrix(" ", categoryLength+perCategory*(betweenLines+questionLength), 
                       numCategories)
    parsedText <- sapply(djAQ$Question, textWrapper, allowedWidth, questionLength)
    for (categ in 1:numCategories) {
      djMatrix[1:categoryLength, categ] <- textWrapper(djCategories[categ], allowedWidth, categoryLength)
      djMatrix[sepLocs, categ] <- separatorLine
      djMatrix[blankLocs, categ] <- blankLine
      for (value in 1:perCategory) {
        djMatrix[(sepLocs[value]+2):(sepLocs[value]+1+questionLength), categ] <- 
          parsedText[, (categ-1)*perCategory+value]
      }
    }
    djMatrix <- apply(djMatrix, 1, paste, collapse=betweenText)
    
    Doc <- c(paste(fName, "Jeopardy Round"), " ", jMatrix,
             paste(fName, "Double Jeopardy Round"), " ", djMatrix,
             fjAQ$Question)
    newName <- paste0("gameboards/", fName, "Cheat.txt")
    write(Doc, file=newName)
    
    write(c(paste(djCategories, collapse=", "), paste(sjCategories, collapse=", ")),
          file=paste0("gameboards/", fName, "Categories.txt"))

    return("Success!")
  }
}

  
  
