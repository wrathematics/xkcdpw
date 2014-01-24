# A simple url scraper.  It's not very elegant, or intelligent, but it
# gets the job done.
html_to_raw <- function(url)
{
  html <- RCurl::getURL(url, followlocation=TRUE)
  
  html.parsed <- XML::htmlParse(html, asText=TRUE)
  text <- XML::xpathSApply(html.parsed, "//text()[not(ancestor::script)]", xmlValue)
  
  raw <- paste(text, collapse=" ")
  
  return( raw )
}

