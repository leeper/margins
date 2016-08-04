#' @title Build list of data.frames
#' @description Construct a list of data.frames based upon an input data.frame and a list of one or more \code{at} values
#' @param data A data.frame containing the original data.
#' @param at A list of one or more named vectors of values, which will be used to specify values of variables in \code{data}. See examples.
#' @param atmeans A logical indicating whether, \emph{after} replacing values in \code{at}, to return one-row data.frames containing variable means.
#' @param \dots Ignored.
#' @return A list of data.frames.
#' @author Thomas J. Leeper
#' @seealso \code{\link{margins.lm}}
#' @examples
#' # basic examples
#' require("datasets")
#' build_datalist(mtcars, at = list(cyl = c(4, 6)))
#'
#' @export
build_datalist <- 
function(data,
         at = NULL, 
         atmeans = FALSE, 
         ...){
    
    #names(data) <- clean_terms(names(data))
    if (!is.null(at) && length(at) > 0) {
        # check factor levels specified in `at`
        check_factor_levels(data, at)
        
        # check names of `at`
        check_at_names(names(data), at)
        
        # setup list of data.frames based on at
        data_out <- set_data_to_at(data, at = at, atmeans = atmeans)
    } else {
        # if `at` empty, simply setup data.frame and return
        if (atmeans) {
            data_out <- list(.atmeans(data))
        } else {
            data_out <- list(as.data.frame(data))
        }
        attr(data_out[[1]], "at") <- NULL
    }
    data_out
}

check_factor_levels <- function(data, at) {
    # function to check whether factor levels in `at` are reasonable
    levels <- lapply(data, levels)
    levels <- levels[!sapply(levels, is.null)]
    at <- at[names(at) %in% names(levels)]
    for (i in seq_along(at)) {
        x <- as.character(at[[i]]) %in% levels[[names(at)[i]]]
        if (!all(x)) {
            stop(paste0("Illegal factor levels for variable '", names(at)[i], "'"), call. = FALSE)
        }
    }
    invisible(NULL)
}

check_at_names <- function(names, at) {
    b <- !names(at) %in% names
    if (any(b)) {
        e <- ngettext(sum(b), "Unrecognized variable name in 'at': ", "Unrecognized variable names in 'at': ")
        stop(paste0(e, paste0(names(at)[b], collapse = ", ")))
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
        # set columns to `atmeans`, if applicable
        if (atmeans) {
            for (i in names(dat)[!names(dat) %in% names(at)]) {
                dat[,i] <- mean(dat[,i], na.rm = TRUE)
            }
        }
        # return data, with `at` attribute
        structure(dat, at = paste(names(atvals), "=", atvals[1,], collapse = ", "))
    })
    return(data_out)
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
