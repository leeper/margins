at_builder <- 
function(data,
         terms,
         levels,
         at = NULL, 
         atmeans = FALSE, 
         ...){
    if (!is.null(at) && length(at) > 0) {
        # check factor levels specified in `at`
        check_factor_levels(data, at, levels)
        # check names of `at`
        if (any(!names(at) %in% names(data))) {
            stop("Unrecognized variable name in 'at'")
        }
        # expand `at` combinations
        e <- expand.grid(at)
        e <- split(e, unique(e))
        # create list of data.frames based on `at` combinations
        data_out <- lapply(e, function(atvals) {
            # create data.frame
            out <- as.data.frame(model.matrix(object = terms, data = data))
            # replace column values with `at` values
            out <- `[<-`(out, , names(atvals), value = atvals)
            # set columns to `atmeans`, if applicable
            if (atmeans) {
                for (i in names(out)[!names(out) %in% names(at)]) {
                    out[,i] <- mean(out[,i], na.rm = TRUE)
                }
            }
            # return data, with `at` attribute
            structure(out, at = paste(names(atvals), "=", atvals[1,], collapse = ", "))
        })
    } else {
        # if `at` empty, simply setup data.frame and return
        data <- model.matrix(object = terms, data = data)
        attr(data, "at") <- NULL
        if (atmeans) {
            data_out <- list(as.data.frame(t(colMeans(data, na.rm = TRUE))))
        } else {
            data_out <- list(as.data.frame(data))
        }
    }
    data_out
}

check_factor_levels <- function(data, at, levels) {
    # function to check whether factor levels in `at` are reasonable
    names(levels) <- clean_terms(names(levels))
    at <- at[names(at) %in% names(levels)]
    for (i in seq_along(at)) {
        x <- as.character(at[[i]]) %in% levels[[names(at)[i]]]
        if (!all(x)) {
            stop(paste0("Illegal factor levels for variable '", names(at)[i], "'"), call. = FALSE)
        }
    }
    invisible(NULL)
}

# data.frame builder, given specified `at` values
.setdata <- function(data, at = NULL) {
    if (is.null(at)) {
        return(list(data))
    }
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
    if (missing(vars)) {
        vars <- names(data)
    }
    for (i in seq_along(vars)) {
        data[,vars[i]] <- mean(data[,vars[i]], na.rm = TRUE)
    }
    data
}
# atquantiles function
.atquantile <- function(data, vars, probs, na.rm = TRUE) {
    if (missing(vars)) {
        vars <- names(data)
    }
    for (i in seq_along(vars)) {
        data[,vars[i]] <- stats::quantile(data[,vars[i]], probs, na.rm = TRUE)
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
