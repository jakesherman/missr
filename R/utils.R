## ============================================================================
##
## Utilities - common functions, reexported functions, etc.
## Depends: magrittr
##
## ============================================================================

#' Pipe operator
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL

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

installed_packages <- function() {
    
    # Names of all installed packages
    unname(utils::installed.packages()[, 1])
}

is_package_installed <- function(package_name) {
    
    # Returns TRUE if a package is installed
    package_name %in% installed_packages()
}
