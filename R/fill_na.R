## ============================================================================
##
## fill_na() - Fills in all NAs (within a column for a data.frame or matrix, or
##             within an element for a list) with a single value
## Depends: lazyeval, dplyr
##
## ============================================================================

#' @export
fill_na_ <- function(.data, .fill, ..., .dots, .args, .warning, .inplace) {
    UseMethod("fill_na_")
}

#' @export
fill_na <- function(.data, .fill, ..., .args = NULL, .warning = TRUE, 
                    .inplace = FALSE) {
    fill_na_(.data, .fill, .dots = lazyeval::lazy_dots(...), .args .warning, 
             .inplace)
}

#' @export
fill_na_.default <- function(.data, .fill, ..., .dots, .args, .warning, 
                             .inplace) {
    
    # Fill in the NAs of a vector with .fill
    if (is.function(.fill)) {
        fill_val <- .fill(.data)
    } else {
        fill_val <- .fill
    }
    if (length(fill_val) > 1) {
        stop("Only one fill value may be applied to a single vector at a time")
    }
    fill_type <- identify_type(fill_val)
    source_type <- identify_type(.data)
    if (fill_type == source_type) {
        .data[is.na(.data)] <- fill_val
    } 
    
    .data
}

#' @export
fill_na_.list <- function(.data, .fill, ..., .dots, .args, .warning, 
                          .inplace) {
    
    # Evaluate ... and .dots. Vars is a vector of valid column names to fill
    # in NAs for. If nothing is specified by vars from ... and .dots, use
    # the column names of .data in place of vars
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    if (length(vars) < 1) {
        vars <- names(.data)
    }
    
    # Do the filling
    if (length(vars) == 0) {
        return(.data)
    }
    .data[vars] <- lapply(.data[vars], fill_na, .fill)
    .data
}

#' @export
fill_na_.data.frame <- function(.data, .fill, ..., .dots, .args, .warning, 
                                .inplace) {
    
    # Loop over the columns, fill in missing values
    
    # Evaluate ... and .dots. Vars is a vector of valid column names to fill
    # in NAs for. If nothing is specified by vars from ... and .dots, use
    # the column names of .data in place of vars
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    if (length(vars) < 1) {
        vars <- names(.data)
    }
    
    # Do the filling
    if (length(vars) == 0) {
        return(.data)
    }
    .data[vars] <- lapply(.data[vars], fill_na, .fill)
    .data
}

#' @export
fill_na_.data.table <- function(.data, .fill, ..., .dots, .args, .warning, 
                                .inplace) {
    
    if (.inplace & is_package_installed) {
        
        # Replacing values by reference
        
        # Evaluate ... and .dots. Vars is a vector of valid column names to
        # fill in NAs for. If nothing is specified by vars from ... and .dots, 
        # use the column names of .data in place of vars
        dots <- lazyeval::all_dots(.dots, ...)
        vars <- dplyr::select_vars_(names(.data), dots)
        if (length(vars) < 1) {
            vars <- names(.data)
        }
        
        # Do the filling
        if (length(vars) == 0) {
            return(.data)
        }
        for (col_name in vars) {
            
            # Fill in the NAs of a vector with .fill
            if (is.function(.fill)) {
                fill_val <- .fill(.data[[col_name]])
            } else {
                fill_val <- .fill
            }
            if (length(fill_val) > 1) {
                stop("Only one fill value may be applied to a single vector ",
                     "at a time")
            }
            fill_type <- identify_type(fill_val)
            source_type <- identify_type(.data[[col_name]])
            if (fill_type == source_type) {
                data.table::set(.data, which(is.na([[col_name]])), col_name, 
                                fill_val) 
            }
        }
        
    } else {
        
        # Not replacing values by referene
        return(fill_na_.data.frame(.data, .fill, ..., .dots, .args, .warning, 
                            .inplace))
    }
}
