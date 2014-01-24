\name{Password}
\alias{password}


\title{ XKCD-style Password Generation }
\description{
  The function uses a random Wikipedia page to generate a
  portmanteau password, as in the XKCD comic 
  https://m.xkcd.com/936/ .
}
\usage{
  password(pw.len=4, min.len=6, max.len=10)
}
\arguments{
  \item{pw.len}{Number of words to use in the password. Default is
  4, as in the comic.}
  \item{min.len}{Minimum word length for each word in the portmanteau.}
  \item{max.len}{Maximum word length for each word in the portmanteau.}
}
\details{
  The function \code{password()} forms a portmanteau password, inspired
  by XKCD comic number 936.  Rather than using a dictionary, however, 
  we use a random Wikipedia page as our word source.
  
  By default, the password will be a portmanteau made up of 4 words
  randomly selected from a random Wikipedia page.  You may change this
  number to another (sensible) choice via the argument \code{pw.len=}.
  The arguments \code{min.len=} and \code{max.len=} refer to the length
  of each word in the portmanteau, rather than the length of the final
  password.
  
  Finally, it should be noted that the function requires an internet 
  connection, as a new, random Wikipedia page is scraped each time the
  function is evaluated.
}
\examples{
\dontrun{
library(xkcdpw, quietly=TRUE)

password()
}
}
