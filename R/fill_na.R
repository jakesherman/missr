## ============================================================================
##
## fill_na() - Fills in all NAs (within a column for a data.frame or matrix,
##             or within an element for a list) with a single value
## Depends: lazyeval, dplyr
##
## ============================================================================

## Common functions ===========================================================

vector_fill <- function(.data, .fill, ..., .args, .warning) {
    
    # Fill in the NAs of a vector with .fill
    
    # Create a fill value based on .fill, if .fill is a function or formula,
    # run that function against .data
    if (!is.null(.args) & length(list(...)) > 0) {
        stop("... and .args cannot both be used at the same time")
    }
    .fill <- as_function(.fill)
    if (is.function(.fill)) {
        if (!is.null(.args)) {
            fill_val <- do.call(.fill, c(list(.data), .args))
        } else if (length(list(...)) > 0) {
            fill_val <- do.call(.fill, c(list(.data), list(...)))
        } else {
            fill_val <- .fill(.data)
        }
    } else {
        fill_val <- .fill
    }
    if (length(fill_val) > 1) {
        stop("Only one fill value may be applied to a single vector at a time")
    }
    
    # If the fill and source types don't match potentially throw a warning
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

list_fill <- function(.data, .fill, ..., .dots, .args, .warning, .p = NULL,
                      .inplace = NULL) {
    
    # Loop over the elements of the list/data.frame, fill in missing values
    
    # Determine which columns to operate on depending on ..., .dots, and .p
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(colnames(.data), dots)
    if (!is.null(.p)) {
        valid_cols <- vapply(.data, .p, logical(1))
        vars <- vars[vars %in% valid_cols]
    }
    if (length(vars) < 1) {
        vars <- names(.data)
    }
    if (length(vars) == 0) {
        return(.data)
    }
    
    # Do the filling
    .data[vars] <- lapply(.data[vars], vector_fill, .fill, .args = .args, 
                          .warning = .warning)
    .data
}

data_table_fill <- function(.data, .fill, ..., .dots, .args, .warning, 
                            .inplace, .p = NULL) {
    
    # Loop over the columns, fill in NAs either by reference if .inplace, or
    # not by reference
    if (.inplace & is_package_installed("data.table")) {
        
        # Determine which columns to operate on depending on ..., .dots, and .p
        dots <- lazyeval::all_dots(.dots, ...)
        vars <- dplyr::select_vars_(colnames(.data), dots)
        if (!is.null(.p)) {
            valid_cols <- vapply(.data, .p, logical(1))
            vars <- vars[vars %in% valid_cols]
        }
        if (length(vars) < 1) {
            vars <- names(.data)
        }
        if (length(vars) == 0) {
            return(.data)
        }
        
        for (col_name in vars) {
            
            # Fill in the NAs of a vector with .fill
            .fill <- as_function(.fill)
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
        return(list_fill(.data, .fill, ..., .dots = .dots, .args = .args, 
                         .warning = .warning, .inplace = .inplace, .p = .p))
    }
    
    invisible(.data)
}

## Regular fill_na ============================================================

#' @describeIn fill_na
#' @export
fill_na_ <- function(.data, .fill, ..., .dots, .args = NULL, .warning = TRUE, 
                     .inplace = FALSE) {
    UseMethod("fill_na_")
}

#' Fills in (replaces) NAs with non-NA values
#' 
#' @param .data a data.frame, data.table, matrix, list, or vector
#' @param .fill x
#' @param ... a selection of columns to fill when \code{.data} is a data.frame, 
#' data.table, matrix, or list, or arguments to \code{.fill} when \code{.data}
#' is a vector. 
#' 
#'     If \code{.data} is a data.frame, data.table, matrix, or list, a 
#'     specification of columns to fill, leaving blank will select all
#'     columns. Use bare variable names. Select all variables between x and z 
#'     with x:z, exclude y with -y. For more options, see the 
#'     \code{dplyr::select} documentation. For lists, elements are treated as 
#'     columns.
#'     
#'     If \code{.data} is a vector, \code{...} are arguments to pass to 
#'     \code{.fill}. You can still use \code{.args} to supply arguments to 
#'     \code{.fill}, but you cannot use both \code{...} and \code{.args} at
#'     the same time.
#' @param .dots Use \code{fill_na_()} to do standard evaluation. See dplyr's 
#' NSE vignette for more information with \code{vignette("nse", "dplyr")}.
#' @param .args for non-vector inputs, a list of arguments to \code{.fill} if
#' \code{.fill} is a function (note that the first argument to \code{.fill}
#' is \code{.data})
#' @param .warning \code{TRUE} by default, display a warning when the type of 
#' \code{.fill} is different from the type of \code{.data}
#' @param .inplace \code{FALSE} by default, if \code{.data} is a 
#' \code{data.table}, fill in the NAs in place (modify \code{.data} by 
#' reference) if this argument is \code{TRUE}
#' @return an object of the same class as \code{.data} with the NAs filled in
#' @export
fill_na <- function(.data, .fill, ..., .args = NULL, .warning = TRUE, 
                    .inplace = FALSE) {
    if (is_atomic(.data)) {
        vector_fill(.data, .fill, ..., .args = .args, 
                    .warning = .warning)
    } else {
        fill_na_(.data, .fill, ..., .dots = lazyeval::lazy_dots(...),
                 .args = .args, .warning = .warning, .inplace = .inplace)
    }
}

# For vectors: ////////////////////////////////////////////////////////////////
# 
#' @describeIn fill_na
#' @export
fill_na.default <- function(.data, .fill, ..., .args = NULL, 
                            .warning = TRUE, .inplace = FALSE) {
    vector_fill(.data, .fill, ..., .dots = NULL, .args = .args, 
                .warning = .warning)
}

# Standard-evaluation versions of fill_na /////////////////////////////////////

#' @describeIn fill_na
#' @export
fill_na_.list <- function(.data, .fill, ..., .dots, .args, .warning, 
                          .inplace) {
    list_fill(.data, .fill, ..., .dots = .dots, .args = .args,
              .warning = .warning)
}

#' @describeIn fill_na
#' @export
fill_na_.data.frame <- function(.data, .fill, ..., .dots, .args, .warning, 
                                .inplace) {
    list_fill(.data, .fill, ..., .dots = .dots, .args = .args,
              .warning = .warning)
}

#' @describeIn fill_na
#' @export
fill_na_.matrix <- function(.data, .fill, ..., .dots, .args, .warning, 
                            .inplace) {
    
    print(.data)
    print(.fill)
    print(list(...))
                                
    as.matrix(fill_na(as.data.frame(.data), .fill, ..., .dots = .dots, 
                      .args = .args, .warning = .warning, .inplace = .inplace))
}

#' @describeIn fill_na
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
            .fill <- as_function(.fill)
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

## Predicate function to fill_na ==============================================

fill_na_if_ <- function(.data, .p, .fill, ..., .args = NULL, .warning = TRUE,
                       .inplace = FALSE) {
    UseMethod("fill_na_if_")
}

fill_na_if <- function(.data, .p, .fill, ..., .args = NULL, .warning = TRUE, 
                    .inplace = FALSE) {
    fill_na_if_(.data, .p, .fill, .dots = lazyeval::lazy_dots(...), 
                .args = .args, .warning = .warning, .inplace = .inplace)
}

fill_na_if_.list <- function(.data, .p, .fill, ..., .dots, .args, .warning, 
                             .inplace) {
    
    # Loop over the elements, fill in missing values
    dots <- lazyeval::all_dots(.dots, ...)
    vars <- dplyr::select_vars_(names(.data), dots)
    valid_cols <- vapply(.data, .p, logical(1))
    vars <- vars[vars %in% valid_cols]
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

## fill_na_mean/median/mode ===================================================

#' @rdname fill_na
#' @export
fill_na_mean <- function(.data, ..., .args = NULL, .warning = TRUE, 
                         .inplace = FALSE) {
    fill_na_mean_(.data, .dots = lazyeval::lazy_dots(...), .args = .args,
                  .warning = .warning, .inplace = .inplace)
}

#' @rdname fill_na
#' @export
fill_na_mean_ <- function(.data, ..., .dots, .args = NULL, .warning = TRUE, 
                          .inplace = FALSE) {
    fill_na_(.data, function(x) mean(x, na.rm = TRUE), ..., .dots = .dots,
             .args = .args, .warning = .warning, .inplace = .inplace)
}

#' @rdname fill_na
#' @export
fill_na_median <- function(.data, ..., .args = NULL, .warning = TRUE, 
                           .inplace = FALSE) {
    fill_na(.data, function(f) median(f, na.rm = TRUE), ..., .args = .args, 
            .warning = .warning, .inplace = .inplace)
}

most_frequent_value <- function(x) {
    
    # Returns the most frequent value in a vector. Chooses the first value
    # in the case of ties. SO #2547402, will use an improved version of mode
    # in the future.
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
}

#' @rdname fill_na
#' @export
fill_na_mode <- function(.data, ..., .dots, .args = NULL, .warning = TRUE, 
                         .inplace = FALSE) {
    fill_na_(.data, most_frequent_value, ..., .dots = .dots,
            .args = .args, .warning = .warning, .inplace = .inplace)
}
