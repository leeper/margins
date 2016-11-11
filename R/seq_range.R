#' @title Create a sequence over the range of a vector
#' @description Define a sequence of evenly spaced values from the minimum to the maximum of a vector
#' @param x A numeric vector
#' @param n An integer specifying the length of sequence (i.e., number of points across the range of \code{x})
#' @return A vector of length \code{n}.
#' @examples
#' seq_range(1:5, n = 2)
#' @export
seq_range <- function(x, n) {
    seq(min(x, na.rm = TRUE), 
        max(x, na.rm = TRUE), 
        length.out = n)
}
