## ============================================================================
##
## Utilities
## - reexporting the magrittr pipe - code copied from hadley/purrr
##
## ============================================================================

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

as_percent <- function(num, total, dig = 1) {
    
    # Returns (num)/(total) as a character percentage
    paste0(round((num * 100) / total, dig), "%")
}