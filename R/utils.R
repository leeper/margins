.grad_factory <- function(data, model, atmeans = TRUE, type = "response") {
    # factory function to return prediction holding data constant but varying coefficients
    function(x) {
        for (i in seq_along(x)) {
            model[["coefficients"]][names(x)[i]] <- x[i]
        }
        if (isTRUE(atmeans)) {
            data <- as.data.frame(t(colMeans(data)))
            predict(model, newdata = data, type = type)
        } else {
            mean(predict(model, newdata = data, type = type), na.rm = TRUE)
        }
    }
}


.predict_factory <- function(data, model, type = "response") {
    # factory function to return prediction as a function of a named variable `x`
    #
    # returns a one-argument function that expresses `predict(model)` with respect to named vector x (variable names and values)
    # this allows the passing of model `predict()` functions to `numDeriv::grad()`
    function(x) {
        tmpdat <- data
        if (!missing(x)) {
            for (i in seq_along(x)) {
                # this assumes numeric data, so factors will be problematic
                tmpdat[[names(x)[i]]] <- x[i]
            }
        }
        unname(predict(model, newdata = tmpdat, type = type))
    }
}

.pred <- function(data, model, type = "response") {
    #' @title Predicted value at values of independent variables
    #' @param data The dataset on which to to calculate `predict(model)`
    #' @param model The model object to pass to `predict()`
    #' @param type The type of prediction. Default is \dQuote{response}.
    
    # extract predicted value at input value (value can only be 1 number)
    if (nrow(data) == 1) {
        # setup function through predict_factory
        FUN <- .predict_factory(data[1,], model, type = type)
        # evaluate FUN()
        out <- as.matrix(stats::setNames(FUN(unlist(data[1,])), rownames(data[1,])))
    } else {
        out <- sapply(1:nrow(data), function(datarow) {
            # setup function through predict_factory
            FUN <- .predict_factory(data[datarow,], model, type = type)
            # evaluate FUN()
            FUN(unlist(data[datarow,]))
        })
        out <- as.matrix(stats::setNames(out, rownames(data)))
    }
    return(out) # obs-x-term matrix of predictions (always one column)
}

.slope <- function(data, model, type = "response", method = c("Richardson", "simple", "complex")) {
    #' @title Calculate slope at specified values of independent variables
    #' @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    #' @param model The model object to pass to `predict()`
    #' @param type The type of prediction. Default is \dQuote{response}.
    #' @param method The differentiation method to use. Passed to `numDeriv::grad()`. One of \dQuote{Richardson}, \dQuote{simple}, \dQuote{complex}.
    
    method <- match.arg(method)
    
    if (nrow(data) == 1) {
        # setup function through predict_factory
        FUN <- .predict_factory(data, model, type = type)
        # extract gradient at input values
        out <- stats::setNames(numDeriv::grad(FUN, unlist(data[1,]), method = method), names(data[1,]))
        out <- t(out)
        rownames(out) <- rownames(data[1,])
    } else {
        out <- sapply(1:nrow(data), function(datarow) {
            # setup function through predict_factory
            FUN <- .predict_factory(data[datarow, , drop = FALSE], model, type = type)
            # extract gradient at input value
            numDeriv::grad(FUN, unlist(data[datarow,]), method = method)
        })
        out <- t(out)
        rownames(out) <- rownames(data)
        colnames(out) <- names(data)
    }
    return(out) # obs-x-term matrix of obs-specific marginal effects
}

.diff <- function(data, model, type = "response", method = c("Richardson", "simple", "complex")) {
    
    ## THIS DOESN'T WORK...IT IS WHAT WE CAN USE FOR FACTORS
    
    #' @title Calculate discrete change in y at specified values of discrete independent variables
    #' @param data The dataset on which to to calculate `predict(model)` (and the slope thereof)
    #' @param model The model object to pass to `predict()`
    #' @param type The type of prediction. Default is \dQuote{response}.
    #' @param method The differentiation method to use. Passed to `numDeriv::grad()`. One of \dQuote{Richardson}, \dQuote{simple}, \dQuote{complex}.
    
    method <- match.arg(method)
    
    if (nrow(data) == 1) {
        # setup functions through predict_factory
        FUN0 <- .predict_factory(data, model, type = type)
        FUN1 <- .predict_factory(data, model, type = type)
        # evaluate FUN0()
        out1 <- as.matrix(stats::setNames(FUN(unlist(data[1,])), rownames(data[1,])))
        # evaluate FUN1()
        out2 <- as.matrix(stats::setNames(FUN(unlist(data[1,])), rownames(data[1,])))
        out <- (out2 - out1)
        rownames(out) <- rownames(data[1,])
    } else {
        out <- sapply(1:nrow(data), function(datarow) {
            # setup functions through predict_factory
            FUN0 <- .predict_factory(data, model, type = type)
            FUN1 <- .predict_factory(data, model, type = type)
            # evaluate FUN0()
            out1 <- as.matrix(stats::setNames(FUN(unlist(data[datarow,])), rownames(data[datarow,])))
            # evaluate FUN1()
            out2 <- as.matrix(stats::setNames(FUN(unlist(data[datarow,])), rownames(data[datarow,])))
            (out2 - out1)
        })
        out <- as.matrix(stats::setNames(out, rownames(data)))
        rownames(out) <- rownames(data)
        colnames(out) <- names(data)
    }
    return(out) # obs-x-term matrix of obs-specific marginal effects
}
