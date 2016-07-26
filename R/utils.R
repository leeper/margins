.predict_factory <- function(data, model, varname, type = "response") {
    # factory function to return prediction across 
    # returns a one-argument function that expresses `predict(model)` with respect to `varname`
    # the argument is the value of `varname` at which to calculate the prediction
    # this allows the passing of model `predict()` functions to `numDeriv::grad()`
    function(x) {
        tmpdat <- `[<-`(data, , varname, value = x)
        unname(predict(model, newdata = tmpdat, type = type))
    }
}

.pred <- function(data, model, varname, value = data[,varname], type = "response") {
    #' @title Predicted value at given value of variable
    #' @param data The dataset on which to to calculate predict(model)
    #' @param model The model object to pass to `predict()`
    #' @param varname A character string specifying the variable whose value will be set to `value`
    #' @param value The value to be used to calculating the predicted value `predict(x)`.
    #' @param type The type of prediction. Default is \dQuote{response}.
    
    # setup function through predict_factory
    FUN <- .predict_factory(data, model, varname, type = type)
    # extract predicted value at input value (value can only be 1 number)
    FUN(value)
}

.slope <- function(data, model, varname, value = data[,varname], type = "response") {
    #' @title Calculate slope at specified value
    #' @param data The dataset on which to to calculate predict(model)
    #' @param model The model object to pass to `predict()`
    #' @param varname A character string specifying the variable whose value will be set to `value`, holding all other variables at observed values
    #' @param value The value to be used to calculating the gradient of `predict(x)`.
    #' @param type The type of prediction. Default is \dQuote{response}.
    
    if (nrow(data) == 1) {
        # setup function through predict_factory
        FUN <- .predict_factory(data, model, varname, type = type)
        # extract gradient at input value (value can only be 1 number)
        numDeriv::grad(FUN, value)
        
    } else {
        sapply(1:nrow(data), function(datarow) {
            # setup function through predict_factory
            FUN <- .predict_factory(data[datarow, , drop = FALSE], model, varname, type = type)
            # extract gradient at input value (value can only be 1 number)
            numDeriv::grad(FUN, value[datarow])
        })
    }
    
}
