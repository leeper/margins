
# factory function to return prediction across 
.predict_factory <- function(varname, model, newdata) {
    # returns a one-argument function
    function(x) {
        dat <- `[<-`(force(newdata), , force(varname), value = x)
        unname(predict(model, newdata = dat))
    }
}

.pred <- function(varname, value, model, data) {
    # pass built data to predict_factory
    FUN <- .predict_factory(varname, model, data)
    
    # extract predicted value at input value (value can only be 1 number)
    FUN(value)
}

.slope <- function(value, varname, model, data) {
    # pass built data to predict_factory
    FUN <- .predict_factory(varname, model, data)
    
    # extract gradient at input value (value can only be 1 number)
    numDeriv::grad(FUN, value)
}


# data.frame builder, given specified `at` values
.data <- function(data, at) {
    if(any(!names(at) %in% names(data)))
        stop("Unrecognized variable name in 'at'")
    e <- expand.grid(at)
    e <- split(e, unique(e))
    data_out <- lapply(e, function(z) {
        dat <- data
        dat <- `[<-`(dat, , names(z), value = z)
        dat
        
    })
    return(setNames(data_out, names(e)))
}

# atmeans function
.atmeans <- function(data, vars, na.rm = TRUE) {
    for(i in seq_along(vars)) {
        data[,[i]] <- mean(data[,vars[i]], na.rm = TRUE)
    }
    data
}


## THIS DOESN'T WORK::::

.margins <- function(x, data, ..., at = NULL, atmeans = FALSE) {
    
    # variables in the model
    v <- attributes(terms(x))$term.labels
    
    # create a data.frame
    dat <- model.frame(x$terms, data = data, ...)
    
    # pass to .data
    if(!is.null(at)) {
        dat <- .data(dat, at = at)
    } else {
        dat <- list(dat)
    }
    
    # iterate over data.frames and pass each to .slope
    lapply(dat, function(d) {
        # optionally pass to .atmeans
        if(atmeans) {
            # need to be able to tell .atmeans which vars to set to means
            d <- .atmeans(d, vars = names(d), na.rm = TRUE)
        }
        
        # obtain gradient with respect to each variable in `v`
        lapply(v, function(variable) {
            # use sapply to evaluate gradient at each value of variable `v`
            sapply(d[,v], function(val) {
                .slope(value = val, varname = variable, model = x, data = d)
            })
        })
        
    })
}


.discretechange <- function(x, from, to, atmeans = FALSE) {
    
    
    # variables in the model
    v <- attributes(terms(x))$term.labels
    
    # create a data.frame
    dat <- model.frame(x$terms, data = data, ...)
    
    # pass to .data
    if(!is.null(at)) {
        dat <- .data(dat, at = at)
    } else {
        dat <- list(dat)
    }
    
    # iterate over data.frames and pass each (twice) to .pred
    lapply(dat, function(d) {
        # optionally pass to .atmeans
        if(atmeans) {
            # need to be able to tell .atmeans which vars to set to means
            d <- .atmeans(d, vars = names(d), na.rm = TRUE)
        }
        
        
        # something something something
        # do the same thing as `.margins` but instead of .slope, do a diff between two .pred values
        
    })
}



# need a variance function here
    # Apply chain rule
    #chain <- grad
    #chain <- grad * mean(predicted) * mean(dpredicted)
    #chain <- grad * mean(predicted) # close
    #chain <- grad * mean(dpredicted)
    #chain <- apply(grad, 2, `*`, t(MEs))
    #colnames(chain) <- betas
    # Apply delta method
    #Variances <- diag(chain %*% vc[colnames(chain), colnames(chain)] %*% t(chain))
