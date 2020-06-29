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


merge.shpdf.df <- function(shpdf, df, by.shpdf, by.df) {
  shpdf@data <-
    data.frame(shpdf@data, df[match(shpdf@data[, by.shpdf], df[, by.df]), ])
  return(shpdf)
}

# Get a fill
get_fill <- function(variable, palette = "YlOrRd", breaks = 5) {
  library("RColorBrewer")
  library("classInt")
  pal <- brewer.pal(breaks, palette)
  return(findColours(classIntervals(variable, n = breaks), pal))
}

# Tom's convenience function. Note that this will only work with clipr installed.
notate <-
  function(text) {
    library("clipr")
    len = nchar(text)
    hashes = 70 - len - 2
    lefthashes = floor(hashes/2)
    righthashes = hashes - lefthashes
    
    notation <-
      paste0(
        paste(rep("#",lefthashes),collapse=""),
        "  ",
        toupper(text),
        "  ",
        paste(rep("#",righthashes),collapse = "")
      )
    
    write_clip(notation)
  }

