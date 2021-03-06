% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateExpected.R
\name{updateExpected}
\alias{updateExpected}
\title{updateExpected}
\usage{
updateExpected(
  df = NULL,
  expected = NULL,
  known = NULL,
  expectedID = NULL,
  stepDesc = NULL,
  quietly = FALSE
)
}
\arguments{
\item{df}{default  is \code{NULL}.  This is optional, but can be a dataframe, where the first 
column is populated by the "expected" values}

\item{expected}{default is \code{NULL} This is a vector of values that should be looked for - 
e.g. a specific list of VR numbers}

\item{known}{default  is \code{NULL} This is the large vector that will be searched for the 
\code{expected} values - e.g. all known VR numbers}

\item{expectedID}{default is \code{NULL}. This text can be used to describe the vector that is 
being debugged - e.g. "licences" or "VR_numbers".  It will be used to produce 
more informative messages, and is especially useful if multiple vectors are being debugged 
simultaneously.}

\item{stepDesc}{default is \code{NULL}. If present, this text will become the name of a column 
in the resultant dataframe describing the relative presence/
absence of the values of the expected vector relative to the \code{known} values.  If longer, it 
will be trimmed to 25 characters.}

\item{quietly}{default is \code{FALSE}. By default, this function will write messages to the 
screen when an expected value is lost.  
Additionally, it will report when all of the expected values have been lost.  By setting it 
to \code{TRUE}, it will only return the populated dataframe - it will not write out messages.}
}
\value{
a dataframe, where the first column (\code{expected}) is populated by the submitted 
values, and a second column, named using the value of \code{stepDesc} is logical, where 0 
indicates that the value was not found within the \code{known} values, and 1 indicates it was 
found.
}
\description{
This function was created to identify the point at which certain expected values are 
dropped from a vector.  For example, a number of records might "go missing" while performing a 
series of filtering steps, and you want to identify which step is resulting in their loss.  By 
calling this function after each step, it will report which of the expected values were lost at 
each point.  Further, 'expected' values are removed from what is looked for after they have been 
lost.  In this way, the function can be called numerous times successively, and it will only 
report the values that were lost at each step (not continue to report all of the lost values at 
each step).
}
\examples{
\dontrun{
test <- updateExpected(expected=c("val1","val2","val3"), 
                       known=c("val1","val2","val3", "val4"), 
                       expectedID = "randomvector1", 
                       stepDesc = "example1", quietly = F)
test
expected example1
1     val1     1
2     val2     1
3     val3     1

test2 <- updateExpected(df = test, 
                        expected=c("val1","val2","val3"), 
                        known=c("val1","val2"), 
                        expectedID = "randomvector2", 
                        stepDesc = "example2", quietly = F)
Lost some randomvector3 during example2: val3
test2
expected example1 example2
1     val1        1        1
2     val2        1        1
3     val3        1        0

test3 <- updateExpected(df = test2, 
                        expected=c("val1","val2","val3"), 
                       known=c("val99"), 
                       expectedID = "randomvector3", 
                       stepDesc = "example3", quietly = F)
Lost some randomvector3 during example3: val1, val2
All of the missing vector has now been lost.
test3
expected example1 example2 example3
1     val1        1        1        0
2     val2        1        1        0
3     val3        1        0        0
}
}
\seealso{
Other debugging: 
\code{\link{changeDetector}()},
\code{\link{where_now}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{debugging}
