% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style-guides.R
\name{tidyverse_style}
\alias{tidyverse_style}
\title{The tidyverse style}
\usage{
tidyverse_style(
  scope = "tokens",
  strict = TRUE,
  indent_by = 2L,
  start_comments_with_one_space = FALSE,
  reindention = tidyverse_reindention(),
  math_token_spacing = tidyverse_math_token_spacing()
)
}
\arguments{
\item{scope}{The extent of manipulation. Can range from "none" (least
invasive) to "tokens" (most invasive). See 'Details'. This argument is a
string or a vector of class \code{AsIs}.}

\item{strict}{A logical value indicating whether a set of strict
or not so strict transformer functions should be returned. Compare the
functions returned with or without \code{strict = TRUE}. For example,
\code{strict = TRUE} means force \emph{one} space e.g. after "," and \emph{one} line break
e.g. after a closing curly brace. \code{strict = FALSE} means to set spaces and
line breaks to one if there is none and leave the code untouched otherwise.
See 'Examples'.}

\item{indent_by}{How many spaces of indention should be inserted after
operators such as '('.}

\item{start_comments_with_one_space}{Whether or not comments should start
with only one space (see \code{\link[=start_comments_with_space]{start_comments_with_space()}}).}

\item{reindention}{A list of parameters for regex re-indention, most
conveniently constructed using \code{\link[=specify_reindention]{specify_reindention()}}.}

\item{math_token_spacing}{A list of parameters that define spacing around
math token, conveniently constructed using \code{\link[=specify_math_token_spacing]{specify_math_token_spacing()}}.}
}
\description{
Style code according to the tidyverse style guide.
}
\details{
The following levels for \code{scope} are available:
\itemize{
\item "none": Performs no transformation at all.
\item "spaces": Manipulates spacing between token on the same line.
\item "indention": Manipulates the indention, i.e. number of spaces at the
beginning of each line.
\item "line_breaks": Manipulates line breaks between tokens.
\item "tokens": manipulates tokens.
}

\code{scope} can be specified in two ways:
\itemize{
\item As a string: In this case all less invasive scope levels are implied, e.g.
"line_breaks" includes "indention", "spaces". This is brief and what most
users need.
\item As vector of class \code{AsIs}: Each level has to be listed explicitly by
wrapping one ore more levels of the scope in \code{\link[=I]{I()}}. This offers more
granular control at the expense of more verbosity.
}

See 'Examples' for details.
}
\examples{
style_text("call( 1)", style = tidyverse_style, scope = "spaces")
style_text("call( 1)", transformers = tidyverse_style(strict = TRUE))
style_text(c("ab <- 3", "a  <-3"), strict = FALSE) # keeps alignment of "<-"
style_text(c("ab <- 3", "a  <-3"), strict = TRUE) # drops alignment of "<-"

# styling line breaks only without spaces
style_text(c("ab <- 3", "a =3"), strict = TRUE, scope = I(c("line_breaks", "tokens")))
}
\concept{obtain transformers}
\concept{style_guides}
