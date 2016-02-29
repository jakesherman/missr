## ============================================================================
##
## drop_na_cols() - gets rid of columns in data.frames, matricies, or elements
##                  in lists containing a certain amount of NAs
## Requires: lazyeval, dplyr
##
## ============================================================================

drop_na_cols_ <- function(.data, tolerance, ..., .dots) {
    UseMethod("drop_na_cols_")
}

#' Drops columns that contain a percent NAs above a tolerance level
#' 
#' @param .data a data.frame, matrix, or list
#' @param ...  Comma separated list of unquoted expressions. You can treat
#' variable names like they are positions. Use positive values to select
#' variables; use negative values to drop variables. Note: this is the same
#' behavior as \code{dplyr::select}, in fact the \code{dplyr::select_vars}
#' function is used here to implement this functionality.
#' @param .dots Use \code{drop_na_cols_()} to do standard evaluation.
#' @return an object without columns/elements containing too many NAs
#' @export
#' @examples
#' 
#' drop_na_cols(mtcars)
drop_na_cols <- function(.data, tolerance = 0, ...) {
    drop_na_cols_(.data, tolerance, .dots = lazyeval::lazy_dots(...))
}

drop_na_cols_.data.frame <- function(.data, tolerance, ..., .dots) {
    
    # Evaluate ... and .dots. Vars is a vector of valid column names.
    # If nothing is specified by vars from ... and .dots, use
    # the column names of .data in place of vars
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    if (length(vars) < 1) {
        vars <- names(.data)
    }
    
    # Error handling - tolerance must be a float on [0, 1]
    stopifnot(is.double(tolerance))
    stopifnot(tolerance >= 0 & tolerance <= 1)
    
    # Get the percent NAs for all columns in vars
    percent_NA <- colSums(is.na(.data)) / nrow(.data) >= tolerance
    names(percent_NA) <- names(.data)
    tol_vars <- percent_NA[names(percent_NA) %in% vars]
    tol_vars <- tol_vars[tol_vars]
    
    # Loop over the valid vars, delete the columns
    for (var in names(tol_vars)) {
        .data[[var]] <- NULL
    }
    
    .data
}
