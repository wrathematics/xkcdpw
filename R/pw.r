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


get_wikipedia_url <- function(language)
{
  
  french <- c("french", "francais", "français")
  german <- c("deutsch", "german")
  italian <- c("italian", "italiano")
  polish <- c("polish", "polski")
  portugese <- c("portugese", "português")
  russian <- c("russian", "pусский")
  spanish <- c("spanish", "espanol", "español")
  
  languages <- c("english", french, german, italian, polish, portugese, russian, spanish)
  language <- match.arg(tolower(language), languages)
  
  # country code
  if (language == "english")
    cc <- "en"
  else if (language %in% french)
    cc <- "fr"
  else if (language %in% german)
    cc <- "de"
  else if (language %in% italian)
    cc <- "it"
  else if (language %in% polish)
    cc <- "pl"
  else if (language %in% portugese)
    cc <- "pt"
  else if (language %in% russian)
    cc <- "ru"
  else if (language %in% spanish)
    cc <- "es"
  
  
  url <- paste("http://", cc, ".wikipedia.org/wiki/Special:Random", sep="")
}



password <- function(pw.len=4, min.len=6, max.len=10, ..., language="english")
{
  check_inputs(pw.len=pw.len, min.len=min.len, max.len=max.len)
  
  url <- get_wikipedia_url(language=language)
  raw <- html_to_raw(url=url)
  tab <- scrubber(raw=raw)
  dict <- build_dict(tab=tab, min.len=min.len, max.len=max.len)
  
  pw <- sample(x=dict, size=pw.len, replace=FALSE)
  
  return( pw )
}

