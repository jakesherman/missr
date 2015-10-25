## ============================================================================
##
## Utilities
##
## ============================================================================

as_percent <- function(num, total, dig = 1) {
    
    # Returns (num)/(total) as a character percentage
    paste0(round((num * 100) / total, dig), "%")
}