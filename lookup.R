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
  source("lookupHelpers.R")
  
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
  
  # Define category types
  categoryTypes <- c("geography", "history", "music", "art", "literature",
                      "science", "technology", "wordplay", "languages",
                      "infrastructure", "movies", "theater", "television",
                      "people", "sports", "culinary", "transportation",
                      "medicine", "media", "games", "other", "quit")
  
  
  newGames <- FALSE
  # Load any current game boards
  if (loadCurrent) {
    onFile <- unique(dtf$filename)
    files <- grep("20.*[.]txt", list.files("gameboards"), value=TRUE)
    files <- setdiff(files, onFile)
    if (length(files) > 0) {
      for (fName in files) {
        if (readline(paste0("Load ", fName, " (y or n)? ")) != "y") next
        temp <- loadAndCategorize(file.path("gameboards", fName), categoryTypes)
        if (is.data.frame(temp)) {
          dtf <- rbind(dtf, temp)
          newGames <- TRUE
        }
      }      
    }
  }

    # Load any used game boards
  if (loadUsed) {
    onFile <- unique(dtf$filename)
    fp <- paste0("gameboards", .Platform$file.sep, "used")
    files <- grep("20.*[.]txt", list.files(fp), value=TRUE)
    files <- setdiff(files, onFile)
    if (length(files) > 0) {
      for (fName in files) {
        if (readline(paste0("Load ", fName, " (y or n)? ")) != "y") next
        temp <- loadAndCategorize(file.path(fp, fName), categoryTypes)
        if (is.data.frame(temp)) {
          dtf <- rbind(dtf, temp)
          newGames <- TRUE
        }
      }      
    }
  }
  
  # Re-save the data if any new games were added
  if (any(newGames)) {
    save(dtf, file=file.path("gameboards", "gameboards.RData"))
  }
  
  # Query the data 
  while(TRUE) {
    #browser()
    cat("quit/tally/c/t/an/q/aq SEARCH TERMS [narrow]\n")
    text <- readline("? ")
    narrow <- FALSE
    text <- strsplit(text, " ", fixed=TRUE)[[1]]
    if (length(text) == 0) next
    if (!text[1] %in% c("quit", "tally", "c", "t", "an", "q", "aq")) {
      cat("Bad input\n")
      next
    }
    if (text[1] == "quit") break
    lt <- length(text)
    if (lt > 1 && text[lt]=="narrow") {
      narrow <- TRUE
      text <- text[-lt]
    }
    if (text[1] == "tally") {
      tally(dtf)
      next
    }
    searchTerm <- paste(text[-1], collapse=" ")
    text <- text[1]
    search(text, searchTerm, dtf, short, narrow)    
  }
  
  # 
  return(invisible(NULL))
}

  
  
