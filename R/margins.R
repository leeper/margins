
# factory function to return prediction across 
.predict_factory <- function(varname, model, datarow, type) {
    # returns a one-argument function
    function(x) {
        dat <- `[<-`(datarow, , varname, value = x)
        unname(predict(model, newdata = dat, type = type))
    }
}

.pfactory2 <- function(vars, model, data, type) {
    # returns a multi-argument function
    F <- function() {
        dat <- `[<-`(data, , varname, value = x)
        unname(predict(model, newdata = dat, type = type))
    }
}

.pred <- function(varname, value, model, data, type) {
    # pass built data to predict_factory
    FUN <- .predict_factory(varname, model, data, type = type)
    
    # extract predicted value at input value (value can only be 1 number)
    FUN(value)
}

.slope <- function(value, varname, model, data, type) {
    # pass built data to predict_factory
    FUNLIST <- lapply(varname, .predict_factory, model = model, data = data, type = type)
    sapply(FUNLIST, function(thisfun) (thisfun)(x = value))
    
    # extract gradient at input value (value can only be 1 number)
    numDeriv::grad(FUN, value)
}


# data.frame builder, given specified `at` values
.setdata <- function(data, at = NULL) {
    if(is.null(at))
        return(list(data))
    #if(any(!names(at) %in% names(data)))
    #    stop("Unrecognized variable name in 'at'")
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
    if(missing(vars))
        vars <- names(data)
    for(i in seq_along(vars)) {
        data[,vars[i]] <- mean(data[,vars[i]], na.rm = TRUE)
    }
    data
}
# atquantiles function
.atquantile <- function(data, vars, probs, na.rm = TRUE) {
    if(missing(vars))
        vars <- names(data)
    for(i in seq_along(vars)) {
        data[,vars[i]] <- quantile(data[,vars[i]], probs, na.rm = TRUE)
    }
    data
}
# atmedians function
.atmedians <- function(data, vars, na.rm = TRUE) {
    .atquantile(data, vars, probs = 0.5, na.rm = na.rm)
}
# atmins function
.atmins <- function(data, vars, na.rm = TRUE) {
    .atquantile(data, vars, probs = 0, na.rm = na.rm)
}
# atmaxs function
.atmaxs <- function(data, vars, na.rm = TRUE) {
    .atquantile(data, vars, probs = 1, na.rm = na.rm)
}


.margins <- 
function(x, 
         data = eval(x$call$data, parent.frame()), 
         ..., 
         at = NULL, 
         atmeans = FALSE, 
         factors = c("discrete", "continuous"), # Not implemented (should be diff between two predictions)
         type = c("response", "terms", "link")) {
    
    factors <- match.arg(factors)
    type <- match.arg(type)
    
    # variables in the model
    allvars <- attributes(terms(x))$term.labels[attributes(terms(x))$order == 1]
    allvars <- sort(.cleanterms(allvars))
    
    # variance-covariance matrix
    vc <- vcov(x)
    
    # create a data.frame and pass to .data
    dat <- .setdata(data[, allvars, drop = FALSE], at = at)
    
    # iterate over data.frames and pass each to .slope
    out <- lapply(dat, function(d) {
        # optionally pass to .atmeans
        if(atmeans) {
            # need to be able to tell .atmeans which vars to set to means
            d <- .atmeans(d, vars = names(d), na.rm = TRUE)
        }
        
        # obtain gradient with respect to each variable in `allvars`
        thisgrad <- lapply(allvars, function(variable) {
            # use sapply to evaluate gradient at each value of variable from `allvars`
            slope <- numeric(nrow(d))
            for(i in seq_along(d[,variable])) {
                slope[i] <- .slope(value = d[i,variable], varname = variable, model = x, data = d[i,,drop=FALSE], type = type)
                #pred[i] <- .slope(value = d[i,variable], varname = variable, model = x, data = d[i,,drop=FALSE])
            }
            slope
        })
        setNames(thisgrad, allvars)
    })
    
    
    # AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAH!!!
    
    # Apply chain rule: Jacobian %*% V(beta) %*% t(Jacobian)
    #chain <- grad
    #chain <- grad * mean(predicted) * mean(dpredicted)
    #chain <- grad * mean(predicted) # close
    #chain <- grad * mean(dpredicted)
    #chain <- apply(grad, 2, `*`, t(MEs))
    #colnames(chain) <- betas
    grad <- setNames(out, names(dat))
    
    # calculate variances
    for(i in seq_along(grad)) {
        jac <- t(colMeans(do.call("cbind", grad[[i]])))
        colnames(jac) <- allvars
        Variances <- diag(jac %*% vc[allvars, allvars] %*% t(jac))
        print(sqrt(Variances))
    }
    
    structure(grad, class = "margins", atmeans = atmeans)
}

