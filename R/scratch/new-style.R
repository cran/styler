library(styler)
tidyverse_with_pipe_expection_style <- function(..., strict = TRUE) {
  tidy_style_guide <- tidyverse_style(..., strict = strict)
  style_line_break_around_curly <- function(strict, pd) {
    if (styler:::is_curly_expr(pd) && nrow(pd) > 2) {
      closing_before <- pd$token == "'}'"
      opening_before <- (pd$token == "'{'") & (pd$token_after != "COMMENT")
      to_break <- lag(opening_before, default = FALSE) | closing_before
      len_to_break <- sum(to_break)
      pd$lag_newlines[to_break] <- ifelse(rep(TRUE, len_to_break),
        1L,
        pmax(1L, pd$lag_newlines[to_break])
      )
    }
    pd
  }
  browser()
  tidy_style_guide$line_break$style_line_break_around_curly <-
    purrr::partial(style_line_break_around_curly, strict = strict)
  tidy_style_guide
}
style_text("
           library(dplyr)
           x <- 3
           function() {ifelse(. > 5, NA, .)}
           ",
  scope = "line_breaks", style = tidyverse_with_pipe_expection_style
)
