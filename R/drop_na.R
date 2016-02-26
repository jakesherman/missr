## ============================================================================
##
## drop_na() - gets rid of rows with NAs
## Requires: lazyeval, dplyr, magrittr
##
## ============================================================================

#' @describeIn drop_na
#' @export
drop_na_ <- function(.data, ..., .dots) {
    UseMethod("drop_na_")
}

#' Drops rows containing NAs
#' 
#' Identical to \code{data[complete.cases(data)]} or 
#' \code{dplyr::filter(complete.cases(.))}.
#' 
#' @param .data a data.frame, data.table, matrix, or list
#' @param ... Specification of columns to fill, leaving blank will select all
#' columns. Use bare variable names. Select all variables between x and z with 
#' x:z, exclude y with -y. For more options, see the \code{dplyr::select} 
#' documentation. For lists, elements are treated as columns.
#' @param .dots Use \code{drop_na_()} to do standard evaluation. See dplyr's 
#' NSE vignette for more information with \code{vignette("nse", "dplyr")}.
#' @return an object of the same class as \code{.data} with the NA rows dropped
#' @export
drop_na <- function(.data, ...) {
    drop_na_(.data, .dots = lazyeval::lazy_dots(...))
}

#' @describeIn drop_na
#' @export
drop_na_.data.frame <- function(.data, ..., .dots) {
    
    # Drop the NA rows in a data.frame
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    all <- FALSE
    if (length(vars) < 1) {
        all <- TRUE
    }
    
    if (all) {
        
        # When no inputs are provided to ... and .dots, we are droping rows
        # with NAs in any column
        .data <- dplyr::filter(.data, complete.cases(.data))
        
    } else {
        
        # Given the selected columns, drop any rows that contain NAs in
        # these columns
        NAs <- lapply(.data[vars], is.na) %>%
            Reduce(`+`, .) == 0
        .data <- dplyr::filter(.data, NAs)
    }
    
    .data
}

#' @describeIn drop_na
#' @export
drop_na_.matrix <- function(.data, ..., .dots) {
    as.matrix(drop_na(as.data.frame(.data), ..., .dots = .dots))
}

#' @describeIn drop_na
#' @export
drop_na_.list <- function(.data, ..., .dots) {
    
    # Drop the NAs from a list
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    all <- FALSE
    if (length(vars) < 1) {
        all <- TRUE
    }
    
    if (all) {
        
    } else {
        
    }
}
