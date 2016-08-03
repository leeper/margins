get_prediction <- function(data, model, type = "response") {
    # @title Predicted value at values of independent variables
    # @param data The dataset on which to to calculate `predict(model)`
    # @param model The model object to pass to `predict()`
    # @param type The type of prediction. Default is \dQuote{response}.
    
    # extract predicted value at input value (value can only be 1 number)
    if (nrow(data) == 1) {
        # setup function through predict_factory
        FUN <- .build_predict_fun(data[1,], model, type = type)
        # evaluate FUN()
        out <- as.matrix(stats::setNames(FUN(unlist(data[1,])), rownames(data[1,])))
    } else {
        out <- sapply(1:nrow(data), function(datarow) {
            # setup function through predict_factory
            FUN <- .build_predict_fun(data[datarow,], model, type = type)
            # evaluate FUN()
            FUN(unlist(data[datarow,]))
        })
        out <- as.matrix(stats::setNames(out, rownames(data)))
    }
    return(as.data.frame(out)) # obs-x-term data.frame of predictions (always one column)
}

get_slopes <- function(data, model, type = c("response", "link"), method = c("Richardson", "simple", "complex")) {
    # @title Calculate slope at specified values of independent variables
    # @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    # @param model The model object to pass to `predict()`
    # @param type The type of prediction. Default is \dQuote{response}.
    # @param method The differentiation method to use. Passed to `numDeriv::grad()`. One of \dQuote{Richardson}, \dQuote{simple}, \dQuote{complex}.
    
    type <- match.arg(type)
    method <- match.arg(method)
    
    if (nrow(data) == 1) {
        # setup function through predict_factory
        FUN <- .build_predict_fun(data, model, type = type)
        # extract gradient at input values
        out <- stats::setNames(numDeriv::grad(FUN, unlist(data[1,]), method = method), names(data[1,]))
        out <- t(out)
        rownames(out) <- rownames(data[1,])
    } else {
        out <- sapply(1:nrow(data), function(datarow) {
            # setup function through predict_factory
            FUN <- .build_predict_fun(data[datarow, , drop = FALSE], model, type = type)
            # extract gradient at input value
            numDeriv::grad(FUN, unlist(data[datarow,]), method = method)
        })
        out <- t(out)
        rownames(out) <- rownames(data)
        colnames(out) <- names(data)
    }
    return(as.data.frame(out)) # obs-x-term data.frame of obs-specific marginal effects
}

get_prediction_diff <- function(data, model, type = c("response", "link"), method = c("Richardson", "simple", "complex")) {
    
    ## THIS DOESN'T WORK...IT IS WHAT WE CAN USE FOR FACTORS
    
    # @title Calculate discrete change in y at specified values of discrete independent variables
    # @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    # @param model The model object to pass to `predict()`
    # @param type The type of prediction. Default is \dQuote{response}.
    # @param method The differentiation method to use. Passed to `numDeriv::grad()`. One of \dQuote{Richardson}, \dQuote{simple}, \dQuote{complex}.
    
    type <- match.arg(type)
    method <- match.arg(method)
    
    if (nrow(data) == 1) {
        # setup functions through predict_factory
        FUN0 <- .build_predict_fun(data, model, type = type)
        FUN1 <- .build_predict_fun(data, model, type = type)
        # evaluate FUN0()
        out1 <- as.matrix(stats::setNames(FUN0(unlist(data[1,])), rownames(data[1,])))
        # evaluate FUN1()
        out2 <- as.matrix(stats::setNames(FUN1(unlist(data[1,])), rownames(data[1,])))
        out <- (out2 - out1)
        rownames(out) <- rownames(data[1,])
    } else {
        out <- sapply(1:nrow(data), function(datarow) {
            # setup functions through predict_factory
            FUN0 <- .build_predict_fun(data, model, type = type)
            FUN1 <- .build_predict_fun(data, model, type = type)
            # evaluate FUN0()
            out1 <- as.matrix(stats::setNames(FUN0(unlist(data[datarow,])), rownames(data[datarow,])))
            # evaluate FUN1()
            out2 <- as.matrix(stats::setNames(FUN1(unlist(data[datarow,])), rownames(data[datarow,])))
            (out2 - out1)
        })
        out <- as.matrix(stats::setNames(out, rownames(data)))
        rownames(out) <- rownames(data)
        colnames(out) <- names(data)
    }
    return(out) # obs-x-term matrix of obs-specific marginal effects
}
