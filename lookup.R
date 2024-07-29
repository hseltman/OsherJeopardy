# Store Jeopardy game info and look up questions and categories
# July 2024
# Loads "/gameboards" games if loadCurrent=TRUE
# Loads "/gameboards/used" games if loadUsed=TRUE
# Saves and reads a data.frame in "games.RData"
# Reads files names "20*.txt" in gameboards and gameboards/used
# Data.frame format ("|" delimited)
# filename (incl. date) | single/double/final | category | categoryType | answer | question


lookup<- function(loadCurrent=FALSE, loadUsed=FALSE) {
  require(stringr)
  
  # Load or create the name the data.frame
  if (file.exists(file.path("gameboards", "gameboards.RData"))) {
    load(file.path("gameboards", "gameboards.RData"))
  } else {
    dtf <- data.frame(filename=character(),
                      board=character(),
                      category=character(),
                      categoryType=character(),
                      answer=character(),
                      question=character(),
                      stringsAsFactors=FALSE)
  }
  
  # Main helper function  
  textWrapper <- function(txt, width=13, length=3, pad=TRUE) {
    if (substring(txt, 1, 1) == " ") {
      txt <- substring(txt, 2)
    }
    words <- strsplit(txt, " ", fixed=TRUE)[[1]]
    ncw <- nchar(words)
    if (length(words)==1 && ncw>width) {
      words <- c(substring(words, 1, width), substring(words, width+1))
    }
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
      wlw <- cLengths <= width
      if (any(wlw)) {
        n = max(which(wlw))
      } else {
        n <- 1
      }
      rslt[line] <- paste(words[1:n], collapse=" ")
      if (length(words) == n) break
      words <- words[-(1:n)]
      lengths <- nchar(words)
      cLengths <- cumsum(lengths) + 1
    }
    if (pad) rslt <- str_pad(rslt, width, side="right")
    return(rslt)
  }

  # Read a gameboard file
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

  # Define category types
  categoryTypes <- c("geography", "history", "music", "art", "literature",
                      "science", "technology", "wordplay", "languages",
                      "infrastructure", "movies", "theater", "television",
                      "people", "sports", "culinary", "transportation",
                      "medicine", "media", "games", "other", "quit")
  
  # Input a category type by partial matching from the list of categoryTypes
  getACategoryType <- function(category) {
    index <- NA
    while (TRUE) {
      input <- readline(paste0(category, "? "))
      index <- pmatch(input, categoryTypes)
      if (!is.na(index)) break
      cat(paste(categoryTypes, collapse=", "), "\n")
    }
    return(categoryTypes[index])
  }
  
  # load one gameboard with categories
  loadAndCategorize <- function(fName) {
    boardVec <- c(rep("s", 30), rep("d", 30), "f")
    one = readOne(fName)
    if (!is.list(one)) {
      cat(one, "\n")
      return(NULL)
    }
    myCats <- character(0)
    for (category in one$sjCategories) {
      ct <- getACategoryType(category)
      if (ct == "quit") return(NULL)
      myCats <- c(myCats, rep(ct, 5))
    }
    for (category in one$djCategories) {
      ct <- getACategoryType(category)
      if (ct == "quit") return(NULL)
      myCats <- c(myCats, rep(ct, 5))
    }
    ct <- getACategoryType(one$fjCategory)
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
    dtf <- rbind(dtf, temp)
  }
  
  newGames <- FALSE
  # Load any current gameboards
  if (loadCurrent) {
    onFile <- unique(dtf$filename)
    files <- grep("20.*[.]txt", list.files("gameboards"), value=TRUE)
    files <- setdiff(files, onFile)
    if (length(files) > 0) {
      for (fName in files) {
        if (readline(paste0("Load ", fName, " (y or n)? ")) != "y") next
        temp <- loadAndCategorize(file.path("gameboards", fName))
        if (is.data.frame(temp)) {
          dtf <- rbind(dtf, temp)
          newGames <- TRUE
        }
      }      
    }
  }

    # Load any used gameboards
  if (loadUsed) {
    onFile <- unique(dtf$filename)
    fp <- paste0("gameboards", .Platform$file.sep, "used")
    files <- grep("20.*[.]txt", list.files(fp), value=TRUE)
    files <- setdiff(files, onFile)
    if (length(files) > 0) {
      for (fName in files) {
        if (readline(paste0("Load ", fName, " (y or n)? ")) != "y") next
        temp <- loadAndCategorize(file.path(fp, fName))
        if (is.data.frame(temp)) {
          dtf <- rbind(dtf, temp)
          newGames <- TRUE
        }
      }      
    }
  }
  
  if (any(newGames)) {
    save(dtf, file=file.path("gameboards", "gameboards.RData"))
  }
  
  # Query the data 
  browser()
  
  # 
  return(invisible(NULL))
}

  
  
