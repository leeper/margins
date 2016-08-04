get_prediction <- function(model, data, type = c("response", "link")) {
    # @title Predicted value at values of independent variables
    # @param model The model object to pass to `predict()`
    # @param data The dataset on which to to calculate `predict(model)`
    # @param type The type of prediction. Default is \dQuote{response}.
    
    # setup data
    if (missing(data)) {
        if (!is.null(model[["call"]][["data"]])) {
            data <- eval(model[["call"]][["data"]], parent.frame()) 
        } else { 
            data <- get_all_vars(model[["terms"]], data = model[["model"]])
        }
    }
    
    type <- match.arg(type)
    
    # extract predicted value at input value (value can only be 1 number)
    out <- lapply(1:nrow(data), function(datarow) {
        # setup function through predict_factory
        FUN <- .build_predict_fun(data[datarow,], model, type = type)
        # evaluate FUN()
        FUN(unlist(data[datarow,]))
    })
    
    # obs-x-1 data.frame of predictions (always one column)
    structure(do.call("rbind.data.frame", out), names = "fitted")
}
