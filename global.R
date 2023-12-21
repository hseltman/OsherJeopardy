# OsherJeopardy global.R file

# For file input
wd = getwd()
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
scoreStyle <- "text-align:center; font-size: 150%; color: blue;"

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
answersPerBoard <- 3
