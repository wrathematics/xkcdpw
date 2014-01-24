\name{Password}
\alias{password}


\title{ XKCD-style Password Generation }
\description{
  The function uses a random Wikipedia page to generate a
  portmanteau password, as in the XKCD comic 
  https://m.xkcd.com/936/ .
}
\usage{
  password(pw.len=4, min.len=4, max.len=12, language="english", ..., 
  num.scrapes=1, ret.type="separate")
}
\arguments{
  \item{pw.len}{Number of words to use in the password. Default is
  4, as in the comic.}
  \item{min.len}{Minimum word length for each word in the portmanteau.}
  \item{max.len}{Maximum word length for each word in the portmanteau.}
  \item{language}{The language the resulting password will be in.}
  \item{...}{Extra arguments.}
  \item{num.scrapes}{The number of Wikipedia scrapes to perform for a 
  single password generation. Must be between 1 and 5.}
  \item{ret.type}{Controls the format of the returned password; options 
  are "separate" or "combined", with the former being the default.}
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
  
  The \code{language=} argument specifies which Wikipedia pages will be
  scraped.  Currently, options are:
  \itemize{
    \item \code{english} for English
    \item \code{french}, \code{francais}, or \code{français} for French
    \item \code{german} or \code{deutsch} for German
    \item \code{italian} or \code{italiano} for Italian
    \item \code{polish} or \code{polski} for Polish
    \item \code{portugese} or \code{português} for Portugese
    \item \code{russian} or \code{pусский} for Russian
    \item \code{spanish}, \code{espanol}, or \code{español} for Spanish
  }
  This argument is case-insensitive.
  
  The optional argument \code{num.scrapes=} determines the number of 
  Wikipedia pages to scrape for a given password generation.  The 
  default is 1, which should be fine almost every time; if not, make
  sure \code{min.len=} isn't set to something stupid and try again.
  Note that \code{num.scrapes=} is limited to a maximum of 5; much
  more than that really should not improve the password, and it just
  needlessly hammers Wikipedia (which might get your ip banned).  It 
  would also really slow the scrubbing to a crawl.
  
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
