## ============================================================================
##
## drop_na() - gets rid of rows with NAs
## Requires: lazyeval, dplyr
##
## ============================================================================

drop_na_ <- function(.data, ..., .dots) {
    UseMethod("drop_na_")
}

drop_na <- function(.data, ...) {
    drop_na_(.data, .dots = lazyeval::lazy_dots(...))
}

drop_na_.data.frame <- function(.data, ..., .dots) {
    
    # Evaluate ... and .dots. Vars is a vector of valid column names to fill
    # in NAs for. If nothing is specified by vars from ... and .dots, use
    # the column names of .data in place of vars
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    all <- FALSE
    if (length(vars) < 1) {
        all <- TRUE
    }
    
    if (all) {
        
        # When no inputs are provided to ... and .dots, we are droping rows
        # with NAs in any column
        .data <- .dplyr::filter(.data, complete.cases(.data))
        
    } else {
        
        # Given the selected columns, drop any rows that contain NAs in
        # these columns
    }
    
    .data
}
