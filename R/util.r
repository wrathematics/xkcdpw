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



# Input checker template
check <- function(x, check)
{
  check <- match.arg(check, c("integer", "positive"))
  nm <- deparse(substitute(x))
  
  if (check == "integer")
  {
    if (!is.int(x))
      stop(paste("argument '", nm, "' must be an integer", sep=""))
  }
  else if (check == "positive")
  {
    if (x <= 0)
      stop(paste("argument '", nm, "' must be positive", sep=""))
  }
  
  invisible()
}



# Input checker for the main function, password()
# Always check your inputs, kiddos.
check_inputs <- function(pw.len, min.len, max.len, num.scrapes)
{
  # Integers only
  check(pw.len, "int")
  check(min.len, "int")
  check(max.len, "int")
  check(num.scrapes, "int")
  
  # Bounds
  check(pw.len, "pos")
  check(min.len, "pos")
  check(max.len, "pos")
  check(num.scrapes, "pos")
  
  # Other
  if (min.len > max.len)
    stop("argument 'min.len' must be less than argument 'max.len'")
  
  if (num.scrapes > 5)
    stop("argument 'num.scrapes' must be no more than 5")
  
  invisible()
}



# Choose which wikipedia url to use based on desired language
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


