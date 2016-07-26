at_builder <- 
function(data,
         terms,
         at = NULL, 
         atmeans = FALSE, 
         ...){
    if (!is.null(at) && length(at) > 0) {
        if (any(!names(at) %in% names(data))) {
            stop("Unrecognized variable name in 'at'")
        }
        e <- expand.grid(at)
        e <- split(e, unique(e))
        data_out <- lapply(e, function(z) {
            out <- as.data.frame(model.matrix(object = terms, data = data))
            out <- `[<-`(out, , names(z), value = z)
            if (atmeans) {
                for (i in names(out)[!names(out) %in% names(at)]) {
                    out[,i] <- mean(out[,i])
                }
            }
            out
        })
    } else {
        data <- model.matrix(object = terms, data = data)
        if (atmeans) {
            data_out <- list(as.data.frame(t(colMeans(data))))
        } else {
            data_out <- list(as.data.frame(data))
        }
    }
    data_out
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
