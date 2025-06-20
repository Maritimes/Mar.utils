% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateCheck.R
\name{updateCheck}
\alias{updateCheck}
\title{updateCheck}
\usage{
updateCheck(gitPkg = NULL)
}
\arguments{
\item{gitPkg}{default is \code{NULL}. This is the URL to the DESCRIPTION file
on github.}
}
\description{
This function compares the package with the available version on 
github, and prompts the user to update.
}
\seealso{
Other util: 
\code{\link{Mode}()},
\code{\link{SQL_in}()},
\code{\link{big_in}()},
\code{\link{combine_lists}()},
\code{\link{st_err}()}
}
\author{
Mike McMahon, \email{Mike.McMahon@dfo-mpo.gc.ca}
}
\concept{util}
