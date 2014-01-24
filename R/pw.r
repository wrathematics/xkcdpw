# Construct a "dictionary" (table) of words satisfying the user-specified
# (or default) length requirements for each word.
build_dict <- function(tab, max.len, min.len)
{
  dict <- unlist(strsplit(x=tab, split=" "))
  dict <- unique(dict)
  
  tmp <- strsplit(x=dict, split="")
  
  indx <- which(sapply(tmp, length) > min.len)
  tmp <- tmp[indx]
  
  indx <- intersect(indx, which(sapply(tmp, length) < max.len))
  
  dict <- dict[indx]
  
  return( dict )
}



password <- function(pw.len=4, min.len=4, max.len=12, language="english", ..., num.scrapes=1, ret.type="separate")
{
  ret.type <- match.arg(tolower(ret.type), c("separate", "combined"))
  check_inputs(pw.len=pw.len, min.len=min.len, max.len=max.len, num.scrapes=num.scrapes)
  
  url <- get_wikipedia_url(language=language)
  raw <- html_to_raw(url=url, num.scrapes=num.scrapes)
  tab <- scrubber(raw=raw)
  dict <- build_dict(tab=tab, min.len=min.len, max.len=max.len)
  
  pw <- sample(x=dict, size=pw.len, replace=FALSE)
  
  if (ret.type == "combined")
    pw <- paste(pw, collapse="")
  
  return( pw )
}

