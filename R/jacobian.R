# function returns a Jacobian matrix (a term-by-beta matrix)
jacobian <- function(FUN, coefficients, weights = NULL, eps = 1e-7) {
    F0 <- FUN(coefficients, weights = weights)
    out <- matrix(NA_real_, nrow = length(F0), ncol = length(coefficients))
    colnames(out) <- names(coefficients)
    rownames(out) <- names(F0)
    for (i in seq_along(coefficients)) {
        coeftemp <- coefficients
        coeftemp[i] <- coeftemp[i] + eps
        out[, i] <- (FUN(coeftemp, weights = weights) - F0) / eps
    }
    out
}
