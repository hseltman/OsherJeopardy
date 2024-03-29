# OsherJeopardy global.R file

# For file input
wd = paste0(getwd(), "/gameboards")
tLoc = regexpr("/Desktop/", wd)
if (tLoc == -1 || nchar(wd) == tLoc+7) {
  roots = wd
  names(roots) = wd
} else {
  roots = c(wd, substring(wd, 1, tLoc+8))
  names(roots) = c(wd, "Desktop")
}
rm(tLoc)

# Styles
scoreStyle <- "text-align:center; font-size: 280%; color: blue; padding-left: -4px; padding-right: -4px;"
betStyle <- "text-align:center; font-size: 190%;"
frStyle <- "padding-left: -4px; padding-right: -4px;"
colStyle <- "padding-left: -8px; padding-right: -8px;"
noGrayBandStyle <- "margin-top: -60px"

# Generate locations of Daily Doubles
genDD <- function(n, restrict=NA) {
  if (!is.na(restrict)) return(sample(1:restrict, n))
  column <- sample(1:6, n)
  row <- sample(1:5, n, replace=TRUE, prob=c(0.05, 0.1, 0.2, 0.325, 0.325))
  return((column-1)*5 + row)
}


# File extensions for multimedia files
imageExtensions <- c("jpg", "jpeg", "png")
audioExtensions <- c("mp3", "m4a")
videoExtensions <- c("mov", "mp4")

# File input is plain text as follows:
# Blank lines and lines where the first non-blank character is "#" are ignored,
# "|" (or 'AQSeparator') is the separator between answer and question
# Jeopardy round: repeated 6 times 
#   Category
#   answer | question  (repeated 5 times)
#
# Double Jeopardy round: repeated 6 times 
#   Category
#   answer | question  (repeated 5 times)
#
# Final Jeopardy
#   Category
#   answer | question
#
# "separator" bar pattern
sbPattern = c(rep(c(FALSE, rep(TRUE, 5)), 6*2), FALSE, TRUE)

# pattern of Categories vs Answer|Question pairs in the file input
catAQPairPattern = c(rep(c(1, 5), 12), 1, 1)

# Used by nextBoard()
stageMatch=c(s="Jeopardy", d="Double Jeopardy", f="Answer")

# Number of answers per board
# (Set to 30, but temporarily lower for quick testing)
answersPerBoard <- 30

# Audio file
themeSong <- "Jeopardy-theme-song.mp3"

# All push buttons to update
allJeopardyButtons <- paste0("jbs", 1:answersPerBoard)
allDoubleJeopardyButtons <- paste0("jbd", 1:answersPerBoard)
