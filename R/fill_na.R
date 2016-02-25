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
    fill_na_(.data, .fill, .dots = lazyeval::lazy_dots(...), .args = .args, 
             .warning = .warning, .inplace = .inplace)
}

#' @export
fill_na_.default <- function(.data, .fill, ..., .dots, .args, .warning, 
                             .inplace) {
    
    # Fill in the NAs of a vector with .fill
    if (is.function(.fill)) {
        if (!is.null(.args)) {
            fill_val <- do.call(.fill, c(list(.data), .args))
        } else {
            fill_val <- .fill(.data)
        }
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
    } else {
        if (.warning) {
            warning("Source vector of type <", source_type, 
                    "> does not match fill value of type <", fill_type, 
                    ">, no filling occured.")
        }
    }
    
    .data
}

#' @export
fill_na_.list <- function(.data, .fill, ..., .dots, .args, .warning, 
                          .inplace) {
    
    # Loop over the elements, fill in missing values
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    if (length(vars) < 1) {
        vars <- names(.data)
    }
    if (length(vars) == 0) {
        return(.data)
    }
    .data[vars] <- lapply(.data[vars], fill_na, .fill, .args = .args, 
                          .warning = .warning)
    .data
}

#' @export
fill_na_.data.frame <- function(.data, .fill, ..., .dots, .args, .warning, 
                                .inplace) {
    
    # Loop over the columns, fill in missing values
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    if (length(vars) < 1) {
        vars <- names(.data)
    }
    if (length(vars) == 0) {
        return(.data)
    }
    .data[vars] <- lapply(.data[vars], fill_na, .fill, .args = .args, 
                          .warning = .warning)
    .data
}

#' @export
fill_na_.data.table <- function(.data, .fill, ..., .dots, .args, .warning, 
                                .inplace) {
    
    if (.inplace & is_package_installed("data.table")) {
        
        # Replacing values by reference
        dots <- lazyeval::all_dots(.dots, ...)
        vars <- dplyr::select_vars_(names(.data), dots)
        if (length(vars) < 1) {
            vars <- names(.data)
        }
        if (length(vars) == 0) {
            return(.data)
        }
        
        for (col_name in vars) {
            
            # Fill in the NAs of a vector with .fill
            if (is.function(.fill)) {
                if (!is.null(.args)) {
                    fill_val <- do.call(
                        .fill, c(list(.data[[col_name]]), .args))
                } else {
                    fill_val <- .fill(.data)
                }
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
                data.table::set(.data, which(is.na(.data[[col_name]])), 
                                col_name, fill_val) 
            } else {
                if (.warning) {
                    warning("Source vector of type <", source_type, 
                            "> does not match fill value of type <", fill_type, 
                            ">, no filling occured.")
                }
            }
        }
        
    } else {
        
        # Not replacing values by referene
        return(fill_na_.data.frame(.data, .fill, ..., .dots = .dots, 
                                   .args = .args, .warning = .warning, 
                                   .inplace = .inplace))
    }
    
    invisible(.data)
}
