## ============================================================================
##
## na() - display information about the number of NAs
## Requires: lazyeval, dplyr, pander
##
## ============================================================================

#' @export
na_ <- function(.data, ..., .dots) {
    UseMethod("na_")
}

#' Gives summary information on NA values
#' 
#' Given an object, prints out a summary table describing the number and 
#' percent NAs, both overall and by column (for data.frames, matricies) or 
#' element (for lists).
#' 
#' @section Special functions:
#' As well as using existing functions like \code{:} and \code{c}, there are
#' a number of special functions that only work inside \code{select}
#'
#' \itemize{
#'  \item \code{starts_with(x, ignore.case = TRUE)}:
#'    names starts with \code{x}
#'  \item \code{ends_with(x, ignore.case = TRUE)}:
#'    names ends in \code{x}
#'  \item \code{contains(x, ignore.case = TRUE)}:
#'    selects all variables whose name contains \code{x}
#'  \item \code{matches(x, ignore.case = TRUE)}:
#'    selects all variables whose name matches the regular expression \code{x}
#'  \item \code{num_range("x", 1:5, width = 2)}:
#'    selects all variables (numerically) from x01 to x05.
#'  \item \code{one_of("x", "y", "z")}:
#'    selects variables provided in a character vector.
#'  \item \code{everything()}:
#'    selects all variables.
#' }
#'
#' To drop variables, use \code{-}. 
#'
#' @param .data a data.frame, matrix, list, or vector
#' @param ...  Comma separated list of unquoted expressions. You can treat
#' variable names like they are positions. Use positive values to select
#' variables; use negative values to drop variables. Note: this is the same
#' behavior as \code{dplyr::select}, in fact the \code{dplyr::select_vars}
#' function is used here to implement this functionality.
#' @oaram .dots Use \code{na_()} to do standard evaluation.
#' @return a table detailing the NAs in an object
#' @export
#' @examples 
#' 
#' Examining the NAs in a data.frame
#' na(mtcars)
#' 
#' The \code{na} function also works on vectors:
#' na(LETTERS)
#' 
#' Matricies:
#' na(as.matrix(mtcars))
#' 
#' and Lists:
#' na(as.list(mtcars))

na <- function(.data, ...) {
    na_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @export
na_.data.frame <- function(.data, ..., .dots) {
    
    # Evaluate ... and .dots
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    
    # Determine if we are looking at a subset of .data
    if (length(vars) > 0) {
        cols <- names(.data)
        delete_cols <- cols[!cols %in% vars]
        for (col in delete_cols) {
            .data[[col]] <- NULL
        }
    }
    
    # Get metadata on .data
    cols <- colnames(.data)
    nrows <- nrow(.data)
    ncols <- ncol(.data)
    all_NAs <- sum(is.na(.data))
    percent_NA <- as_percent(all_NAs, nrows * ncols)
    
    # Get the number of NAs by column
    col_NAs <- colSums(is.na(.data))
    col_Non_NAs <- nrows - col_NAs
    percent_NAs <- paste0(round((col_NAs * 100) / nrows, 1), "%")
    
    # Initialize the table
    NA_table <- data.frame(
        Column = cols, NAs = col_NAs, NonNAs = col_Non_NAs, 
        PercentNA = percent_NAs, stringsAsFactors = FALSE)
    attributes(NA_table)$row.names <- 1:ncols
    
    # Print the output
    cat("Source: data.frame/matrix [", nrows, "x", ncols, "]\n")
    cat(percent_NA, "of all cells are NAs.\n")
    pander::pander(NA_table)
}

#' @export
na_.matrix <- function(.data, ..., .dots) {
    na_.data.frame(.data, ..., .dots)
}

#' @export
na_.list <- function(.data, ..., .dots) {
    
    # List method  - create a table with a column for each element of the list,
    # OR loop over the list, and use the NAs function on each element?
    
    # Evaluate ... and .dots
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    
    # Determine if we are looking at a subset of .data
    if (length(vars) > 0) {
        cols <- names(.data)
        delete_cols <- cols[!cols %in% vars]
        for (col in delete_cols) {
            .data[[col]] <- NULL
        }
    }
    
    # Get metadata on .data
    lengths <- vapply(.data, length, numeric(1))
    all_NAs <- vapply(.data, function(f) sum(is.na(f)), numeric(1))
    non_NAs <- lengths - all_NAs
    percent_NAs <- unlist(Map(as_percent, all_NAs, lengths))
    vars <- names(.data)
    classes <- vapply(.data, class, character(1))
    num_ele <- length(vars)
    total_percent_NA <- as_percent(sum(all_NAs), sum(lengths))
    
    # Initialize the NA table
    NA_table <- data.frame(Element = vars, Length = unname(lengths),
                           Class = unname(classes), NAs = unname(all_NAs), 
                           NonNAs = unname(non_NAs), 
                           PercentNA = unname(percent_NAs), 
                           stringsAsFactors = FALSE)
    
    # Output the results
    cat("Source: local list [", 
        paste0("Lengths of ", range(lengths)[1], " to ", range(lengths)[2], 
               " x ", num_ele, " elements"), "]\n")
    cat(total_percent_NA, "of all elements of elements are NAs.\n")
    pander::pander(NA_table)
}

#' @export
na_.default <- function(.data, ..., .dots) {
    
    # Performs on an arbitrary vector
    
    # Get metadata on .data
    len <- length(.data)
    all_NAs <- sum(is.na(.data))
    non_NAs <- len - all_NAs
    percent_NAs <- as_percent(all_NAs, len)
    
    # Initialize the table
    NA_table <- data.frame(NAs = all_NAs, NonNAs = non_NAs,
                           PercentNA = percent_NAs, stringsAsFactors = FALSE)
    
    # Print the output
    cat("Source: local vector [", len, "]\n")
    pander::pander(NA_table)
}

#' @export
na_.character <- function(.data, ..., .dots) {
    na_.default(.data, ..., .dots)
}

#' @export
na_.numeric <- function(.data, ..., .dots) {
    na_.default(.data, ..., .dots)
}

#' @export
na_.factor <- function(.data, ..., .dots) {
    na_.default(.data, ..., .dots)
}

#' @export
na_.logical <- function(.data, ..., .dots) {
    na_.default(.data, ..., .dots)
}
