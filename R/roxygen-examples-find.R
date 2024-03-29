#' Figure out where code examples start and stop
#'
#' Finds the sequence from start to stop of the lines in `text` that are
#' code examples in roxygen comments.
#' @param text A text consisting of code and/or roxygen comments.
#' @keywords internal
identify_start_to_stop_of_roxygen_examples_from_text <- function(text) {
  starts <- grep("^#'(\\s|\t)*@examples(If\\s|\\s|\t|$)", text, perl = TRUE)
  if (length(starts) < 1L) {
    return(integer())
  }
  stop_candidates <- which(magrittr::or(
    # starts with code or a tag
    grepl("(^[^#]|^#'[\\s\t]*@)", text, perl = TRUE),
    # starts with a roxygen comment with a blank line after
    grepl("^ *\t*$", text) & grepl("^#' *", lead(text))
  )) %>%
    c(length(text) + 1L) # if ending with a roxygen example
  stops <- map(starts, match_stop_to_start, stop_candidates) %>%
    flatten_int()
  if (length(stops) < 1L) {
    return(integer())
  }

  map2(starts, stops, seq2)
}

identify_start_to_stop_of_roxygen_examples <- function(path) {
  content <- read_utf8_bare(path)
  identify_start_to_stop_of_roxygen_examples_from_text(content)
}

#' Match a stop candidate to a start
#' @param start An integer.
#' @param stop_candidates Potential stop candidates.
#' @examples
#' styler:::match_stop_to_start(1, c(3, 4, 5))
#' @keywords internal
match_stop_to_start <- function(start, stop_candidates) {
  is_stop_candidate <- stop_candidates > start
  if (any(is_stop_candidate)) {
    min(stop_candidates[is_stop_candidate]) - 1L
  } else {
    integer()
  }
}

#' Find `dontrun` and friend sequences
#'
#' Returns the indices of the lines that correspond to a `dontrun` or
#' friends sequence.
#' @param bare Bare code.
#' @keywords internal
find_dont_seqs <- function(bare) {
  dont_openings <- which(bare %in% dont_keywords())
  dont_closings <- map_int(dont_openings + 1L, find_dont_closings, bare = bare)
  map2(dont_openings, dont_closings, seq2)
}

#'
find_dont_closings <- function(bare, dont_openings) {
  opening <- cumsum(bare == "{")
  closing <- cumsum(bare == "}")
  diff <- opening - closing
  level_dont <- diff[dont_openings]
  match_closing <- intersect(
    seq2(dont_openings + 1L, length(bare)),
    which(diff == level_dont - 1L)
  )[1L]
  match_closing + 1L
}
