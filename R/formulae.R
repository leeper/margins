# function to cleanup I(), etc. in formulas
gsub_bracket <- function(a, b) {
    tmp <- regmatches(a, gregexpr(paste0("(",b,"\\().+(\\))"), a))
    regmatches(a, gregexpr(paste0("(",b,"\\().+(\\))"), a)) <- 
      gsub(")$","", gsub(paste0("^",b,"\\("), "", tmp))
    a
}

# function to drop multipliers, powers, etc.
drop_operators <- function(a, dropdigits = TRUE) {
    a <- gsub(" ","",a)
    # remove mathematical operators
    if(dropdigits) {
        a <- gsub("^[:digit:]+(\\^|\\+|\\-|\\*|\\|/)", "", a)
        a <- gsub("(\\^|\\+|-|\\*|/)[[:digit:]+]$", "", a)
    } else {
        a <- gsub("(?<=[[:digit:]+])(\\^|\\+|\\-|\\*|\\|/)", "", a, perl = TRUE)
        a <- gsub("(\\^|\\+|-|\\*|/)(?=[[:digit:]+])", "", a, perl = TRUE)
    }
    # need to remove mathematical expressions
    exprs <- c("exp", "log", "sin", "cos", "tan", "sinh", "cosh", 
               "sqrt", "pnorm", "dnorm", "asin", "acos", "atan", 
               "gamma", "lgamma", "digamma", "trigamma")
    for(i in seq_along(exprs)){
        a <- gsub_bracket(a, exprs[i])
    }
    a
}

# call sub_bracket on all common formula operations
.cleanterms <- function(terms) {
    v <- gsub_bracket(terms, "factor")
    v <- gsub_bracket(v, "I")
    v <- gsub_bracket(v, "poly")
    v <- drop_operators(v)
    v <- unique(v)
    v
}

