library(missr)
context("Testing the fill_na family of functions")

## vector tests ---------------------------------------------------------------

test_that("fill_na works on a double vector", {
    myvec <- c(1, 2, NA, NA, 5)
    myvec_good <- c(1, 2, 1, 1, 5)
    expect_identical(fill_na(myvec, 1), myvec_good)
})

test_that(
    "fill_na on a vector works with an anonymous function .fill argument", {
    myvec <- c(1, 2, NA, NA, 3)
    myvec_good <- c(1, 2, 2, 2, 3)
    expect_identical(
        fill_na(myvec, function(x) mean(x, na.rm = TRUE)), myvec_good)
})

test_that("fill_na on a vector works with a formula function .fill argument", {
    myvec <- c(1, 2, NA, NA, 3)
    myvec_good <- c(1, 2, 2, 2, 3)
    expect_identical(fill_na(myvec, ~mean(., na.rm = TRUE)), myvec_good)
})

## data.frame tests -----------------------------------------------------------

test_that("fill_na works on a data.frame w/ column selection", {
    mydf <- data.frame(
        A = c(1L, 2L, NA, NA, 5L), B = LETTERS[1:5], stringsAsFactors = FALSE)
    mydf_good <- data.frame(
        A = c(1L, 2L, 0L, 0L, 5L), B = LETTERS[1:5], stringsAsFactors = FALSE)
    expect_identical(fill_na(mydf, 0L, A), mydf_good)
})

test_that("fill_na works outputs a warning on an incorrect data type", {
    mydf <- data.frame(
        A = c(1L, 2L, NA, NA, 5L), B = LETTERS[1:5], stringsAsFactors = FALSE)
    expect_warning(fill_na(mydf, 0L, B), 
                   paste0("Source vector of type <character> does not match ",
                          "fill value of type <integer>, no filling occured."))
})

test_that("fill_na does not output a warning when we use the .warning arg", {
    mydf <- data.frame(
        A = c(1L, 2L, NA, NA, 5L), B = LETTERS[1:5], stringsAsFactors = FALSE)
    expect_silent(fill_na(mydf, 0L, B, .warning = FALSE))
})

## matrix tests ---------------------------------------------------------------

test_that("fill_na works on a matrix", {
    mydf <- as.matrix(data.frame(
        A = c(1L, 2L, NA, NA, 5L), B = LETTERS[1:5], stringsAsFactors = FALSE))
    mydf_good <- as.matrix(data.frame(
        A = c(1L, 2L, 0L, 0L, 5L), B = LETTERS[1:5], stringsAsFactors = FALSE))
    expect_identical(fill_na(mydf, 0L), mydf_good)
})

## data.table tests -----------------------------------------------------------

if (is_package_installed("data.table")) {
    library("data.table", quietly = TRUE)
    
    test_that("fill_na works on a data.table w/o .inplace", {
        mydf <- data.table(
            A = c(1, 2, NA, NA, 5), B = c(10, 9, NA, NA, 6))
        mydf_good <- data.table(
            A = c(1, 2, 11, 11, 5), B = c(10, 9, 11, 11, 6))
        expect_identical(fill_na(mydf, 11), mydf_good)
    })
    
    test_that("fill_na works on a data.table w .inplace", {
        mydf <- data.table(
            A = c(1, 2, NA, NA, 5), B = c(10, 9, NA, NA, 6))
        mydf_good <- data.table(
            A = c(1, 2, 11, 11, 5), B = c(10, 9, 11, 11, 6))
        fill_na(mydf, 11, .inplace = TRUE)
        expect_identical(mydf, mydf_good)
    })
}
