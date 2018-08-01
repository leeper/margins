#' @rdname margins
#' @param level A numeric value specifying the confidence level for calculating p-values and confidence intervals.
#' @param by_factor A logical specifying whether to order the output by factor (the default, \code{TRUE}).
#' @export
margins_summary <- function(model, ..., level = 0.95, by_factor = TRUE) {
    mar <- margins(model, ...)
    summary(mar, level = 0.95, by_factor = by_factor)
}
