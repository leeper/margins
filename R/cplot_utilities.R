# function to check factors
check_factors <- function(object, data, xvar, dx) {
    # check factors
    classes <- attributes(terms(object))[["dataClasses"]][-1]
    classes <- classes[names(classes) != "(weights)"]
    classes[classes == "character"] <- "factor"
    
    varslist <- find_terms_in_model(model = object)
    
    c(list(classes = classes),
      varslist,
      x_is_factor = xvar %in% varslist$fnames,
      dx_is_factor = dx %in% varslist$fnames,
      list(data = data[, c(varslist$nnames, varslist$fnames), drop = FALSE]))
}
