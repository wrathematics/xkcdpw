# Tests if an input is, in a practical sense, an integer (as opposed to
# how it is stored in memory on the machine, as is.integer() does)
is.int <- function(n)
{
  if (length(n) == 1)
  {
    if (is.numeric(n))
    {
      if (n-as.integer(n) == 0)
        return( TRUE )
      else
        return( FALSE )
    }
    else
    {
      return( FALSE )
    }
  }
  else if (length(n) == 0)
  {
    return( is.integer(n) )
  }
  else
  {
    return( all(sapply(n, is.int)) )
  }
}



# Input checker for the main function, password()
# Always check your inputs, kiddos.
check_inputs <- function(pw.len, min.len, max.len, num.scrapes)
{
  # Integers only
  if (!is.int(pw.len))
    stop("argument 'pw.len' must be an integer")
  if (!is.int(min.len))
    stop("argument 'min.len' must be an integer")
  if (!is.int(max.len))
    stop("argument 'max.len' must be an integer")
  if (!is.int(num.scrapes))
    stop("argument 'num.scrapes' must be an integer")
  
  
  # Bounds
  if (pw.len < 1)
    stop("argument 'pw.len' must be positive")
  if (min.len < 1)
    stop("argument 'min.len' must be positive")
  if (max.len < 1)
    stop("argument 'max.len' must be positive")
  if (num.scrapes < 1)
    stop("argument 'num.scrapes' must be positive")
  
  if (min.len > max.len)
    stop("argument 'min.len' must be less than argument 'max.len'")
  
  if (num.scrapes > 5)
    stop("argument 'num.scrapes' must be no more than 5")
  
  
  invisible()
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
  
  return( url )
}


