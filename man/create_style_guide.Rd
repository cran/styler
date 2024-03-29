% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style-guides.R
\name{create_style_guide}
\alias{create_style_guide}
\title{Create a style guide}
\usage{
create_style_guide(
  initialize = default_style_guide_attributes,
  line_break = NULL,
  space = NULL,
  token = NULL,
  indention = NULL,
  use_raw_indention = FALSE,
  reindention = tidyverse_reindention(),
  style_guide_name = NULL,
  style_guide_version = NULL,
  more_specs_style_guide = NULL,
  transformers_drop = specify_transformers_drop(),
  indent_character = " "
)
}
\arguments{
\item{initialize}{The bare name of a function that initializes various
variables on each level of nesting.}

\item{line_break}{A list of transformer functions that manipulate line_break
information.}

\item{space}{A list of transformer functions that manipulate spacing
information.}

\item{token}{A list of transformer functions that manipulate token text.}

\item{indention}{A list of transformer functions that manipulate indention.}

\item{use_raw_indention}{Boolean indicating whether or not the raw indention
should be used.}

\item{reindention}{A list of parameters for regex re-indention, most
conveniently constructed using \code{\link[=specify_reindention]{specify_reindention()}}.}

\item{style_guide_name}{The name of the style guide. Used as a meta attribute
inside the created style guide, for example for caching. By convention,
this is the style guide qualified by the package namespace plus the
location of the style guide, separated by \code{@}. For example,
\code{"styler::tidyverse_style@https://github.com/r-lib"}.}

\item{style_guide_version}{The version of the style guide. Used as a meta
attribute inside the created style guide, for example for caching. This
should correspond to the version of the R package that exports the
style guide.}

\item{more_specs_style_guide}{Named vector (coercible to character)
with all arguments passed to the style guide and used for cache
invalidation. You can easily capture them in your style guide function
declaration with \code{as.list(environment())} (compare source code of
\code{tidyverse_style()}).}

\item{transformers_drop}{A list specifying under which conditions
transformer functions can be dropped since they have no effect on the
code to format, most easily constructed with
\code{\link[=specify_transformers_drop]{specify_transformers_drop()}}. This is argument experimental and may
change in future releases without prior notification. It was mainly
introduced to improve speed. Listing transformers here that occur almost
always in code does not make sense because the process of excluding them
also takes some time.}

\item{indent_character}{The character that is used for indention. We strongly
advise for using spaces as indention characters.}
}
\description{
This is a helper function to create a style guide, which is technically
speaking a named list of groups of transformer functions where each
transformer function corresponds to one styling rule. The output of this
function can be used as an argument for \code{style} in top-level functions
like \code{\link[=style_text]{style_text()}} and friends. Note that for caching to work properly,
unquote all inputs to the transformer function if possible with rlang's \verb{!!},
otherwise, they will be passed as references (generic variable names) instead
of literals and \code{styler:::is_cached()} won't pick up changes. See how it's
done in \code{\link[=tidyverse_style]{tidyverse_style()}} with \code{indent_by} and other arguments.
}
\examples{
set_line_break_before_curly_opening <- function(pd_flat) {
  op <- pd_flat$token \%in\% "'{'"
  pd_flat$lag_newlines[op] <- 1L
  pd_flat
}
set_line_break_before_curly_opening_style <- function() {
  create_style_guide(
    line_break = list(set_line_break_before_curly_opening),
    style_guide_name = "some-style-guide",
    style_guide_version = "some-version"
  )
}
style_text(
  "a <- function(x) { x }",
  style = set_line_break_before_curly_opening_style
)
}
