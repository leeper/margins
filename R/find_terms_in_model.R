find_terms_in_model <- function(model) {
    # identify classes of terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]][-1]
    # drop specially named "(weights)" variables
    if (!is.null(model[["weights"]])) {
        classes <- classes[!names(classes) %in% "(weights)"]
    }
    # handle character variables as factors
    classes[classes == "character"] <- "factor"
    ## cleanup names of terms
    terms2 <- sapply(names(classes), function(x) all.vars(parse(text = x)))
    names(classes)[names(classes) %in% names(terms2)] <- terms2[names(classes) %in% names(terms2)]
    
    # identify factors versus numeric terms in `model`
    list(
      nnames = clean_terms(names(classes)[!classes %in% c("factor", "ordered", "logical")]),
      lnames = clean_terms(names(classes)[classes == "logical"]),
      fnames = clean_terms(names(classes)[classes %in% c("factor", "ordered")]),
      fnames2 = names(classes)[classes %in% c("factor", "ordered")] # for checking stupid variable naming behavior by R
    )
}
