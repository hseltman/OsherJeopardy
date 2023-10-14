sjCateg = c("1940s", "WWII", "Interesting Pittsburgh",
            "Mars", "Stravinsky", "Railroads")

djCateg = c("In 'Utero'", "Word Origins", "Mushrooms",
            "Country of the Sport", "Dreaded Opera Category", "Plants")


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