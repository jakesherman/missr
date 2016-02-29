## ============================================================================
##
## Utilities
##
## ============================================================================

as_percent <- function(num, total, dig = 1) {
    
    # Returns (num)/(total) as a character percentage
    paste0(round((num * 100) / total, dig), "%")
}

create_random_na_vector <- function(vector, num_NAs) {
    
    # Randomly select #[num_NAs] indicies from [vector] and turn them into NAs
    na_indicies <- sample(seq(length(vector)), num_NAs)
    vector[seq(length(vector)) %in% na_indicies] <- NA
    vector
}

create_random_na <- function(data, prop_na_range = c(.25, .75), 
                             ignore_cols = NULL, seed = NULL) {
    
    # Given a dataframe [data], create a random number of NA values in each 
    # column not in [ignore_cols] with proportions ranging from the beginning
    # value of [prop_na_range] to the last value of it
    if (!is.null(seed)) {
        set.seed(seed)
    }
    data <- data
    cols <- names(data)
    if (!is.null(ignore_cols)) 
        cols <- cols[!cols %in% ignore_cols]
    col_props <- round(
        runif(length(cols), prop_na_range[1], 
              prop_na_range[2]) * nrow(data), 1)
    for (i in seq_along(cols)) {
        data[[cols[i]]] <- create_random_na_vector(
            data[[cols[i]]], col_props[i])
    }
    
    data
}

#' Identifies the type of a vector
#' 
#' \code{identify_type} identifies what type a vector is. Generally it agrees
#' with \code{typeof}, except in the case of factors.
#' @param vector - a vector to identify the type of
#' @export
#' @examples
#' 
#' identify_type(mtcars$mpg)
#' sapply(iris, identify_type)
identify_type <- function(vector) {
    
    # Given a vector, identifies what the type if. Generally it agrees with
    # the \code{typeof} fuction, except for factors.
    type <- typeof(vector)
    if (type == "integer") {
        vec_class <- class(vector)
        if (vec_class == "factor") {
            type <- vec_class
        }
    }
    
    type
}

cols_of_type <- function(.data, .type) {
    
    # Returns the names of columns of .data that are of type .type
    names(.data)[vapply(.data, identify_type, character(1)) == .type]
}

cols_of_types <- function(.data, .types) {
    
    # Plural of cols_of_type
    cols <- c()
    for (.type in .types) {
        cols <- append(cols, cols_of_type(.data, .type))
    }
    
    unique(cols)
}

installed_packages <- function() {
    
    # Names of all installed packages
    unname(utils::installed.packages()[, 1])
}

is_package_installed <- function(package_name) {
    
    # Returns TRUE if a package is installed
    package_name %in% installed_packages()
}

make_function <- function(args, body, env = parent.frame()) {
    
    # purrr:::make_function
    # https://github.com/hadley/purrr/R/partial.R
    args <- as.pairlist(args)
    stopifnot(is.call(body) || is.name(body) || is.atomic(body))
    env <- as.environment(env)
    eval(call("function", args, body), env)
}

#' Convert an object into a function.
#'
#' \code{as_function} is the powerhouse behind the varied function
#' specifications that purrr functions allow. This is a a non-exported,
#' modified version that does not turn character vectors into functions, 
#' instead it simply returns objects that are not functions or formulas as-is.
#'
#' @param .f A function, formula, or atomic vector.
#'
#'   If a \strong{function}, it is used as is.
#'
#'   If a \strong{formula}, e.g. \code{~ .x + 2}, it is converted to a
#'   function with two arguments, \code{.x} or \code{.} and \code{.y}. This
#'   allows you to create very compact anonymous functions with up to
#'   two inputs.
#'
#'   If anything else, return it as-is.
as_function <- function(.f) {
    
    # purrr::as_function, w/ modifications
    # https://github.com/hadley/purrr/R/utils.R
    UseMethod("as_function")
}

as_function.function <- function(.f) .f

as_function.formula <- function(.f) {
    .x <- NULL # hush R CMD check NOTE
    
    if (length(.f) != 2) {
        stop("Formula must be one sided", call. = FALSE)
    }
    make_function(alist(.x = , .y = , . = .x), .f[[2]], environment(.f))
}

as_function.default <- function(.f) .f

delete_row <- function(df, row) {
    
    # Deletes row [row] from data.frame [df]
    stopifnot(row > 0 & row <= nrow(df))
    if (row == 1) {
        return(dplyr::slice(df, 2:nrow(df)))
    } else if (row == nrow(df)) {
        return(dplyr::slice(df, 1:(nrow(df) - 1)))
    } else {
        return(rbind(dplyr::slice(df, 1:(row - 1)), 
              dplyr::slice(df, (row + 1):nrow(df))))
    }
}
