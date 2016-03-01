library(missr)
context("Testing the drop_na function")

test_that("drop_na is working on a data.frame", {
    mt <- mtcars
    mt$mpg[1] <- NA
    mt$cyl[7] <- NA
    mt_good <- delete_row(mt, 1)
    mt_good <- delete_row(mt_good, 6)
    expect_identical(drop_na(mt), mt_good)
})

test_that("drop_na is working on a matrix", {
    mt <- mtcars
    mt$mpg[1] <- NA
    mt$cyl[7] <- NA
    mt_good <- delete_row(mt, 1)
    mt_good <- delete_row(mt_good, 6)
    mt_good <- as.matrix(mt_good)
    rownames(mt_good) <- NULL
    expect_identical(as.matrix(drop_na(mt)), mt_good)
})
