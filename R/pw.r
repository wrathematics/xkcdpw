# Construct a "dictionary" (table) of words satisfying the user-specified
# (or default) length requirements for each word.
build_dict <- function(tab, max.len, min.len)
{
  dict <- unlist(strsplit(x=tab, split=" "))
  
  tmp <- strsplit(x=dict, split="")
  
  indx <- which(sapply(tmp, length) > min.len)
  tmp <- tmp[indx]
  
  indx <- intersect(indx, which(sapply(tmp, length) < max.len))
  
  dict <- dict[indx]
  
  return( dict )
}



password <- function(pw.len=4, min.len=6, max.len=10)
{
  check_inputs(pw.len=pw.len, min.len=min.len, max.len=max.len)
  
  url = "http://en.wikipedia.org/wiki/Special:Random"
  raw <- html_to_raw(url=url)
  tab <- scrubber(raw=raw)
  dict <- build_dict(tab=tab, min.len=min.len, max.len=max.len)
  
  pw <- sample(x=dict, size=pw.len, replace=FALSE)
  
  return( pw )
}

