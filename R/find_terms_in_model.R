# function to identify terms in model formula
## @return A three-element list containing: 
## - `nnames` (numeric variables)
## - `lnames` (logical variables)
## - `fnames` (factor variables)
find_terms_in_model <- function(model, variables = NULL) {
    
    # identify classes of terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]][-1]
    # drop specially named "(weights)" variables
    wts <- weights(model)
    if (!is.null(wts)) {
        classes <- classes[!names(classes) %in% "(weights)"]
    }
    # handle character variables as factors
    classes[classes == "character"] <- "factor"
    ## cleanup names of terms
    names(classes) <- clean_terms(names(classes))
    
    # identify factors versus numeric terms in `model`, and examine only unique terms
    vars <- list(
      nnames = unique(names(classes)[!classes %in% c("factor", "ordered", "logical")]),
      lnames = unique(names(classes)[classes == "logical"]),
      fnames = unique(names(classes)[classes %in% c("factor", "ordered")])
    )
    
    # subset of variables for which to compute the marginal effects
    if (!is.null(variables)) {
        tmp <- c(vars$nnames, vars$lnames, vars$fnames)
        if (any(!variables %in% tmp)) {
            stop("Some values in 'variables' are not in the model terms.")
        }
        vars$nnames <- vars$nnames[vars$nnames %in% variables]
        vars$lnames <- vars$lnames[vars$lnames %in% variables]
        vars$fnames <- vars$fnames[vars$fnames %in% variables]
    }
    
    return(vars)
}

# call gsub_bracket on all common formula operations
clean_terms <- function(terms) {
    # the use of paste("`", x, "`") is a hack to deal with variables that have spaces in their names
    unlist(lapply(terms, function(x) all.vars(formula(paste0("~", ifelse(grepl("[[:alnum:]_.] [[:alnum:]_.]", x), paste0("`", x, "`"), x))))))
}
