#' @title Build list of data.frames
#' @description Construct a list of data.frames based upon an input data.frame and a list of one or more \code{at} values
#' @param data A data.frame containing the original data.
#' @param at A list of one or more named vectors of values, which will be used to specify values of variables in \code{data}. See examples.
#' @param atmeans A logical indicating whether, \emph{after} replacing values in \code{at}, to return one-row data.frames containing variable means.
#' @param \dots Ignored.
#' @return A list of data.frames.
#' @author Thomas J. Leeper
#' @seealso \code{\link{margins}}
#' @examples
#' # basic examples
#' require("datasets")
#' build_datalist(head(mtcars), at = list(cyl = c(4, 6)))
#'
#' str(build_datalist(head(mtcars, at = list(cyl = c(4,6), wt = c(1,2,3)))))
#'
#' @keywords data manip
#' @export
build_datalist <- 
function(data,
         at = NULL, 
         atmeans = FALSE, 
         ...){
    
    #names(data) <- clean_terms(names(data))
    if (!is.null(at) && length(at) > 0) {
        # check `at` specification against data
        check_at(data, at)
        
        # setup list of data.frames based on at
        data_out <- set_data_to_at(data, at = at, atmeans = atmeans)
    } else {
        # if `at` empty, simply setup data.frame and return
        if (atmeans) {
            data_out <- list(.atmeans(data))
        } else {
            data_out <- list(data)
        }
        attr(data_out[[1]], "at") <- NULL
        attr(data_out[[1]], "atmeans") <- atmeans
    }
    data_out
}

check_at <- function(data, at) {
    # check names of `at`
    check_at_names(names(data), at)
    
    # check factor levels specified in `at`
    check_factor_levels(data, at)
    
    # check values of numeric values are interpolations
    check_values(data, at)
}

check_factor_levels <- function(data, at) {
    # function to check whether factor levels in `at` are reasonable
    levels <- lapply(data, function(v) {
        if (is.factor(v)) {
            levels(v)
        } else if (is.character(v)) {
            levels(factor(v))
        } else {
            NULL
        } 
    })
    levels <- levels[!sapply(levels, is.null)]
    at <- at[names(at) %in% names(levels)]
    for (i in seq_along(at)) {
        atvals <- as.character(at[[i]])
        x <- atvals %in% levels[[names(at)[i]]]
        if (!all(x)) {
            stop(paste0("Illegal factor levels for variable '", names(at)[i], "': ", 
                        paste0(shQuote(atvals[!x]), collapse = ", ")), 
                 call. = FALSE)
        }
    }
    invisible(NULL)
}

check_values <- function(data, at) {
    dat <- data[, names(at), drop = FALSE]
    dat <- dat[, !sapply(dat, class) %in% c("character", "factor"), drop = FALSE]
    limits <- do.call(rbind, lapply(dat, range, na.rm = TRUE))
    for (i in seq_along(at)) {
        out <- (at[[i]] < limits[i,1]) | (at[[i]] > limits[i,2])
        if (any( out ) ) {
            datarange <- paste0("outside observed data range (", limits[i,1], ",", limits[i,2], ")!")
            warning(ngettext(sum(out), paste0("A 'at' value for '", names(at)[i], "' is ", datarange),
                                       paste0("Some 'at' values for '", names(at)[i], "' are ", datarange)))
        }
    }
}

check_at_names <- function(names, at) {
    b <- !names(at) %in% names
    if (any(b)) {
        e <- ngettext(sum(b), "Unrecognized variable name in 'at': ", "Unrecognized variable names in 'at': ")
        stop(paste0(e, paste0("(", which(b), ") ", gsub("", "<empty>", names(at)[b]), collapse = ", ")))
    }
}

# data.frame builder, given specified `at` values
set_data_to_at <- function(data, at = NULL, atmeans = FALSE) {
    # expand `at` combinations
    e <- expand.grid(at)
    e <- split(e, unique(e))
    data_out <- lapply(e, function(atvals) {
        dat <- data
        dat <- `[<-`(dat, , names(atvals), value = atvals)
        
        if (atmeans) {
            # set columns to `atmeans`, if applicable
            dat <- .atmeans(dat, names(dat)[!names(dat) %in% names(at)])
        }
        
        # return data, with `at` attribute
        structure(dat, at = atvals, atmeans = atmeans)
    })
    return(data_out)
}

# atmeans function
.atmeans <- function(data, vars, na.rm = TRUE) {
    if (missing(vars)) {
        vars <- names(data)
    }
    for (i in seq_along(vars)) {
        data[[vars[i]]] <- mean(data[[vars[i]]], na.rm = TRUE)
    }
    data
}
# atquantiles function
.atquantile <- function(data, vars, probs, na.rm = TRUE) {
    if (missing(vars)) {
        vars <- names(data)
    }
    for (i in seq_along(vars)) {
        data[[vars[i]]] <- stats::quantile(data[[vars[i]]], probs, na.rm = TRUE)
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
