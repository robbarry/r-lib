# BASIC R FUNCTIONS - General purpose use

# Automatically install packages and then require them.
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# Clip dataframe to clipboard.
ClipIt <- function(x) write.table(x, "clipboard-16384", sep="\t", row.names=F)

# Disable scientific notation
options(scipen = 999)

# Disable strings as factors
options(stringsAsFactors = FALSE)

# Because Excel...
excel.date <- function(stamp) {
  return(as.Date(as.numeric(stamp), origin = "1899-12-30"))
}

# This should be the table function's default behavior
table.na <- function(...) table(..., useNA='ifany')
