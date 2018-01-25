# function to identify terms in model formula
## @return A four-element list containing: 
## - `nnames` (numeric variables)
## - `lnames` (logical variables)
## - `fnames` (factor variables)
## - `fnames2` (numeric variables made into factors in formula specification)
find_terms_in_model <- function(model, variables = NULL) {
    
    # identify classes of terms in `model`
    classes <- attributes(terms(model))[["dataClasses"]][-1]
    # drop specially named "(weights)" variables
    if (!is.null(model[["weights"]])) {
        classes <- classes[!names(classes) %in% "(weights)"]
    }
    # handle character variables as factors
    classes[classes == "character"] <- "factor"
    ## cleanup names of terms
    cleaned_terms <- clean_terms(names(classes))
    ## THIS WILL DROP TERMS WHERE A TERM IS ONLY IN AN `I()` EXPRESSION
    #classes <- classes[names(classes) %in% cleaned_terms]
    ## LET'S TRY SOMETHING DIFFERENT
    names(classes) <- cleaned_terms
    
    # identify factors versus numeric terms in `model`, and cleanup the names of terms
    vars <- list(
      nnames = names(classes)[!classes %in% c("factor", "ordered", "logical")],
      lnames = names(classes)[classes == "logical"],
      fnames = names(classes)[classes %in% c("factor", "ordered")],
      fnames2 = names(classes)[classes %in% c("factor", "ordered")] # for checking stupid variable naming behavior by R
    )
    
    # subset of variables for which to compute the marginal effects
    if (!is.null(variables)) {
        tmp <- c(vars$nnames, vars$lnames, vars$fnames, vars$fnames2)
        if (any(!variables %in% tmp)) {
            stop("Some values in 'variables' are not in the model terms.")
        }
        vars$nnames <- vars$nnames[vars$nnames %in% variables]
        vars$lnames <- vars$lnames[vars$lnames %in% variables]
        vars$fnames <- vars$fnames[vars$fnames %in% variables]
        vars$fnames2 <- vars$fnames2[vars$fnames2 %in% variables]
    }
    
    return(vars)
}

# call gsub_bracket on all common formula operations
clean_terms <- function(terms) {
    # the use of paste("`", x, "`") is a hack to deal with variables that have spaces in their names
    unlist(lapply(terms, function(x) all.vars(formula(paste0("~", ifelse(grepl("[[:alpha:]] [[:alpha:]]", x), paste0("`", x, "`"), x))))))
}
