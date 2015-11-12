## ============================================================================
##
## fill_na() - Fills in all NAs (within a column for a data.frame or matrix, or
##             within an element for a list) with a single value
## Requires: lazyeval, dplyr
##
## ============================================================================

#' cols_of_type()
#' 
#' Identifies which columns in a data.frame are of a specific class.
#' 
#' @param .data a data.frame
#' @param type a character vector of a class name (ex. \code{"numeric"} or
#' \code{"character"}) OR a function that identifies a class given a vector
#' as input. For the former, the \code{is.YOUR_INPUT} function is used to 
#' identify the class.
#' @param cols a vector of column names - only look for columns of class [type]
#' for these specific columns. By default, [cols] is all of the columns of
#' [.data].
#' @export
cols_of_type <- function(.data, type, cols = names(.data)) {
    
    # Determine which columns in the data.frame [.data] (of the columns 
    # identified in the [cols] argument) are of type [type]. By default, the 
    # function is.[type], unless [type] is a function, in which case the
    # function is used to determine types.
    
    # First, only consider columns specified in [cols]
    delete_cols <- names(.data)[!names(.data) %in% cols]
    for (col in delete_cols) {
        .data[[col]] <- NULL
    }
    
    # Next, identify the column types matching [type]
    if (!is.function(type)) {
        type <- match.fun(paste0("is.", type))
    }
    
    # Then, determine which columns match the type
    valid_type <- vapply(.data, type, logical(1))
    valid_cols <- names(.data)[valid_type]
    
    if (length(valid_cols) > 0) {
        return(valid_cols)
    } else {
        return(NULL)
    }
}

fill_na_ <- function(.data, fill, ..., .dots) {
    UseMethod("fill_na_")
}

fill_na <- function(.data, fill, ...) {
    fill_na_(.data, fill, .dots = lazyeval::lazy_dots(...))
}

fill_na_.data.frame <- function(.data, fill, ..., .dots) {
    
    # Evaluate ... and .dots. Vars is a vector of valid column names to fill
    # in NAs for. If nothing is specified by vars from ... and .dots, use
    # the column names of .data in place of vars
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    if (length(vars) < 1) {
        vars <- names(.data)
    }
    
    if (is.function(fill)) {
        
        # If fill is a function, loop over vars and assign all NAs in the col
        # corresponding to each var with the result of the function /////
        
        for (var in vars) {
            fill_value <- fill(.data[[var]])
            if (length(fill_value) != 1) {
                stop("Functions supplied to the fill argument must return",
                     "a vector of length 1 given the argument of the current",
                     "column of .data")
                
            } else {
                .data[[var]][which(is.na(.data[[var]]))] <- fill_value
            }
        }
        
    } else if (is.list(fill) && !is.null(names(fill))) {
        
        # If fill is a named list, loop over the columns /////
        
        if (!all(names(fill) %in% vars)) {
            
            # If the inputted columns aren't valid, return an error
            stop("One or more of your inputted column names are incorrect")
            
        } else {
            
            # Otherwise, fill the NAs
            for (var in names(fill)) {
                fill_value <- fill[[var]]
                if (length(fill_value) != 1) {
                    stop("Lists supplied to the fill argument must only",
                         "contain elements whose length is 1.")
                    
                } else {
                    if (class(fill_value) != class(.data[[var]])) {
                        warning("Your fill_value is not of the same as",
                                "the column you are trying to fill - ",
                                "coersion may be taking place!")
                    }
                    
                    .data[[var]][which(is.na(.data[[var]]))] <- fill_value
                }
            }
        }
        
    } else {
        
        # If fill is anything else, loop over that thing and fill in NAs for
        # columns with matching types
        
        fill_types <- vapply(fill, class, character(1))
        if (any(table(fill_types) > 1)) {
            stop("You entered more than one fill value for a given class")
        }
        
        for (i in seq_along(fill)) {
            
            # Determine the fill value, error if > 1 value
            fill_value <- fill[[i]]
            if (length(fill_value) != 1) {
                stop("Lists supplied to the fill argument must only",
                     "contain elements whose length is 1.")
            }
            
            # Determine the type of fill, and the cols that match that class
            fill_type <- fill_types[i]
            fill_type_cols <- cols_of_type(.data, fill_type, vars)
            if (length(fill_type_cols) < 1) {
                message("No columns affected by the fill value: ", fill_value)
                next
            }
            
            # Loop over the valid columns
            for (var in fill_type_cols) {
                .data[[var]][which(is.na(.data[[var]]))] <- fill_value
            }
        }
    }
    
    .data
}