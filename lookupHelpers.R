# Helper files for lookup()
# July 2024


# Read a gameboard file
# Input: a game file name (with path and extension)
# Output: 
#   Either 
#   1) a character with an error message 
#   or
#   2) a list with:
#      fName: character
#      sjCategories: character vector of length 6
#      djCategories: character vector of length 6
#      fjCategory: character
#      sjAQ: data.frame with 30 rows and columns "Answer" and "Question"
#      djAQ: data.frame with 30 rows and columns "Answer" and "Question"
#      fjAQ: data.frame with 1 row and columns "Answer" and "Question"
#      
readOne <- function(fName) {
  AQSeparator = "|"
  inData <- try(readLines(fName))
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
  }    
  return(list(fName=fName, sjCategories=sjCategories,
              djCategories=djCategories, fjCategory=fjCategory,
              sjAQ=sjAQ, djAQ=djAQ, fjAQ=fjAQ))
}    


# Get a category type from the keyboard by partial matching
# from the list of categoryTypes
# Input: the name of a category
# Output: the category type corresponding to the user's input (may be "quit")
#
getACategoryType <- function(category, categoryTypes) {
  index <- NA
  while (TRUE) {
    input <- readline(paste0(category, "? "))
    index <- pmatch(input, categoryTypes)
    if (!is.na(index)) break
    cat(paste(categoryTypes, collapse=", "), "\n")
  }
  return(categoryTypes[index])
}

# Load one game board with categories
# Input: game board file name and path
# Output: 
#  NULL if an error
#  or
#  a data.frame from the game board (61 x 6)
#    filename: character
#    board: character ('s'x30, 'd'x30, 'f')
#    category: character
#    categoryType: character
#    answer: character
#    question: character
#    answer: character
#
loadAndCategorize <- function(fName, categoryTypes) {
  boardVec <- c(rep("s", 30), rep("d", 30), "f")
  one = readOne(fName)
  if (!is.list(one)) {
    cat(one, "\n")
    return(NULL)
  }
  myCats <- character(0)
  for (category in one$sjCategories) {
    ct <- getACategoryType(category, categoryTypes)
    if (ct == "quit") return(NULL)
    myCats <- c(myCats, rep(ct, 5))
  }
  for (category in one$djCategories) {
    ct <- getACategoryType(category, categoryTypes)
    if (ct == "quit") return(NULL)
    myCats <- c(myCats, rep(ct, 5))
  }
  ct <- getACategoryType(one$fjCategory, categoryTypes)
  if (ct == "quit") return(NULL)
  myCats <- c(myCats, ct)
  temp <- data.frame(filename=basename(fName),
                     board=boardVec,
                     category=c(rep(one$sjCategories, each=5),
                                rep(one$djCategories, each=5),
                                one$fjCategory),
                     categoryType=myCats,
                     answer=c(one$sjAQ$Answer, one$djAQ$Answer, one$fjAQ$Answer),
                     question=c(one$sjAQ$Question, one$djAQ$Question, one$fjAQ$Question))
  return(temp)
}


# Tally the current data.frame
#
tally <- function(dtf) {
  cat("# games =", length(unique(dtf$filename)), "\n")
  pie(table(dtf$categoryType), main="Per question")
  cat("Per board:\n")
  pb <- dtf[!duplicated(paste(dtf$filename, dtf$board, dtf$category)),
            "categoryType"]
  tab <- table(pb)
  tab <- round(tab/sum(tab)*100)
  tab <- data.frame(type=names(tab), percent=as.numeric(tab))
  print(tab, row.names=FALSE)
  return(NULL)
}

# Search for something in the gameboard history
#
searchHistory <- function(what, searchTerm, dtf, short, narrow) {
  codes <- c("category", "type", "answer", "question", "aq")
  what <- codes[pmatch(what, codes)]
  if (what=="aq") {
    Sel <- grepl(searchTerm, paste(dtf$answer, dtf$question))
    cols <- c("question", "answer")
    if (!narrow) {
      cols <- c("filename", "board", "category", cols)
    }
  } else if (what %in% c("answer", "question")) {
    Sel <- grepl(searchTerm, dtf[[what]])
    cols <- c("category", what)
    if (!narrow) {
      cols <- c("filename", "board", cols)
    }
  } else if (what == "category") {
    Sel <- grepl(searchTerm, dtf$category)
    cols <- "category"
    if (!narrow) {
      cols <- c("filename", "board", cols)
    }
  } else { # "type"
    Sel <- grepl(searchTerm, dtf$categoryType)
    cols <- "category"
  }
  dtf2 <- dtf[Sel, cols, drop=FALSE]
  if (what %in% c("category", "type")) {
    asone <- apply(dtf2, 1, function(x) paste(x, collapse=""))
    dtf2 <- dtf2[!duplicated(asone), , drop=FALSE]
  }
  print(dtf2, row.names=FALSE)
  #cat("look in", what, "for", searchTerm, ":", short, narrow, "\n")
  return(NULL)
}

