% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nest.R
\name{combine_children}
\alias{combine_children}
\title{Combine child and internal child}
\usage{
combine_children(child, internal_child)
}
\arguments{
\item{child}{A parse table or \code{NULL}.}

\item{internal_child}{A parse table or \code{NULL}.}
}
\description{
Binds two parse tables together and arranges them so that the tokens are in
the correct order.
}
\details{
Essentially, this is a wrapper around vctrs::vec_rbind()], but
returns \code{NULL} if the result of vctrs::vec_rbind()] is a data frame with
zero rows.
}
\keyword{internal}
