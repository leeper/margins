#' @export
confint.margins <- 
function(object, parm, level = 0.95, ...) {
    mes <- marginal_effects(object, with_at = FALSE)
    names(mes) <- gsub("^dydx_", "", names(mes))
    pnames <- colnames(mes)
    if (missing(parm)) {
        parm <- pnames
    } else if (is.numeric(parm))  {
        parm <- pnames[parm]
    } else if (is.character(parm)) {
        parm[!grepl("^_", parm)] <- paste0("_", parm[!grepl("^_", parm)])
    }
    cf <- colMeans(mes, na.rm = TRUE)[parm]
    a <- (1 - level)/2
    a <- c(a, 1 - a)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, a))
    variances <- unlist(object[1L, grepl("Var_dydx_", names(object), fixed = TRUE), drop = TRUE])
    ses <- if (is.null(variances)) rep(NA_real_, length(cf)) else sqrt(variances)
    ci[] <- cf + ses %o% fac
    colnames(ci) <- c("lower", "upper")
    ci
}
