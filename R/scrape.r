# A simple url scraper.  It's not very elegant, or intelligent, but it
# gets the job done.
html_to_raw <- function(url, num.scrapes)
{
  raw <- ""
  
  while (num.scrapes > 0)
  {
    html <- RCurl::getURL(url, followlocation=TRUE)
    
    html.parsed <- XML::htmlParse(html, asText=TRUE)
    text <- XML::xpathSApply(html.parsed, "//text()[not(ancestor::script)]", xmlValue)
    
    raw <- paste(raw, paste(text, collapse=" "))
    
    num.scrapes <- num.scrapes - 1
  }
  
  return( raw )
}

