# Does exactly what it says
remove_double_space <- function(str)
{
  tmp <- unlist(strsplit(str, "  "))
  if (any(tmp == ""))
    tmp <- tmp[-which(tmp=="")]
  
  ret <- paste(tmp, collapse=" ")
  
  return( ret )
}



# All multi-spaces become single spaces
only_one_space <- function(str)
{
  gsub(" +", " ", x=str)
}



# Some simple scrubbing:
  # eventually I will utline what this does, but right now I'm tired
scrubber <- function(raw)
{
  tab <- gsub(pattern="\\t", replacement="", x=raw)
  tab <- gsub(pattern="\\n", replacement="", x=tab)
  tab <- gsub(pattern="\\\"", replacement="", x=tab)
  tab <- only_one_space(tab)
  tab <- gsub(pattern="http://[a-zA-Z]+", perl=TRUE, replacement="", x=tab)
  tab <- gsub(pattern="[[:punct:]]", perl=TRUE, replacement="", x=tab)
  tab <- gsub(pattern="[0-9]", perl=TRUE, replacement="", x=tab)
  tab <- unique(tab)

  return( tab )
}

