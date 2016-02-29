## ============================================================================
## 
## Testing the drop_na function
## 
## ============================================================================

library(missr)
context("Testing the drop_na function")

# Test drop_na on a data.frame
mt <- mtcars
mt$mpg[1] <- NA
mt$mpg[3] <- NA
mt$cyl[3] <- NA
mt$cyl[7] <- NA