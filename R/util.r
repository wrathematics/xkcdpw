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
check_inputs <- function(pw.len, min.len, max.len)
{
  # Integers only
  if (!is.int(pw.len))
    stop("argument 'pw.len' must be an integer")
  if (!is.int(min.len))
    stop("argument 'min.len' must be an integer")
  if (!is.int(max.len))
    stop("argument 'max.len' must be an integer")
  
  # Bounds
  if (pw.len < 1)
    stop("argument 'pw.len' must be positive")
  if (min.len < 1)
    stop("argument 'min.len' must be positive")
  if (max.len < 1)
    stop("argument 'max.len' must be positive")
  
  if (min.len > max.len)
    stop("argument 'min.len' must be less than argument 'max.len'")
  
  invisible()
}


