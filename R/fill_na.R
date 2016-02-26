## ============================================================================
##
## fill_na() - Fills in all NAs (within a column for a data.frame or matrix,
##             or within an element for a list) with a single value
## Depends: lazyeval, dplyr
##
## ============================================================================

#' @describeIn fill_na
#' @export
fill_na_ <- function(.data, .fill, ..., .dots, .args, .warning, .inplace) {
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
    fill_na_(.data, .fill, .dots = lazyeval::lazy_dots(...), .args = .args, 
             .warning = .warning, .inplace = .inplace)
}

#' @describeIn fill_na
#' @export
fill_na_.default <- function(.data, .fill, ..., .dots, .args, .warning, 
                             .inplace) {
    
    # Fill in the NAs of a vector with .fill
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

#' @describeIn fill_na
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

#' @describeIn fill_na
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

#' @describeIn fill_na
#' @export
fill_na_.matrix <- function(.data, .fill, ..., .dots, .args, .warning, 
                            .inplace) {
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

#' @rdname fill_na
#' @export
fill_na_mean <- function(.data, ..., .dots, .args, .warning, .inplace) {
    fill_na(.data, function(f) mean(f, na.rm = TRUE), ..., .dots = .dots,
            .args = .args, .warning = .warning, .inplace = .inplace)
}

#' @rdname fill_na
#' @export
fill_na_median <- function(.data, ..., .dots, .args, .warning, .inplace) {
    fill_na(.data, function(f) median(f, na.rm = TRUE), ..., .dots = .dots,
            .args = .args, .warning = .warning, .inplace = .inplace)
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
fill_na_mode <- function(.data, ..., .dots, .args, .warning, .inplace) {
    fill_na(.data, most_frequent_value, ..., .dots = .dots,
            .args = .args, .warning = .warning, .inplace = .inplace)
}
