# Some simple scrubbing; see inline comments for explanation
scrubber <- function(raw)
{
  # remove tabs
  tab <- gsub(pattern="\\t", replacement="", x=raw)
  # remove newlines
  tab <- gsub(pattern="\\n", replacement="", x=tab)
  # remove quotes
  tab <- gsub(pattern="\\\"", replacement="", x=tab)
  # remove multi-spaces
  tab <- gsub(pattern=" +", replacement=" ", x=tab)
  # remove url's
  tab <- gsub(pattern="http://.*$", perl=TRUE, replacement="", x=tab)
  # remove punctuation
  tab <- gsub(pattern="[[:punct:]]", perl=TRUE, replacement="", x=tab)
  # remove numbers
  tab <- gsub(pattern="[0-9]", perl=TRUE, replacement="", x=tab)
  # remove unicode spaces FUCK UNICODE SERIOUSLY
  tab <- gsub(pattern="[\\h\\v]", perl=TRUE, replacement=" ", x=tab)
  # remove trailing whitespace (might as well)
  tab <- sub(pattern=" +$", replacement="", x=tab)
  
  return( tab )
}

