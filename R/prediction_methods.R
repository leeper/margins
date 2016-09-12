#' @rdname prediction
#' @export
prediction.default <- function(model, data = find_data(model, parent.frame()), type = "response", ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              type = type)
}

#' @rdname prediction
#' @export
prediction.lm <- prediction.default

#' @rdname prediction
#' @export
prediction.glm <- function(model, data = find_data(model, parent.frame()), type = c("response", "link"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.loess <- function(model, data = find_data(model, parent.frame()), type = "response", ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.ivreg <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input values
    pred <- data.frame(fit = predict(model, newdata = data, ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.nls <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input values
    pred <- data.frame(fit = predict(model, newdata = data, ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.survreg <- 
function(model, 
         data = find_data(model, parent.frame()), 
         type = c("response", "lp", "quantile", "uquantile"), 
         ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.coxph <- function(model, data = find_data(model, parent.frame()), type = c("risk", "expected", "lp"), ...) {
    # setup data
    data <- data
    
    type <- match.arg(type)
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- predict(model, newdata = data, type = type, se.fit = TRUE, ...)
    class(pred[["fit"]]) <- c("fit", "numeric")
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]), 
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = type)
}

#' @rdname prediction
#' @export
prediction.gls <- function(model, data, ...) {
    # setup data
    data <- data
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fit = predict(model, newdata = data, type = "class", ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # obs-x-2 data.frame of predictions
    structure(list(fitted = pred[["fit"]], 
                   se.fitted = pred[["se.fit"]]),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}

#' @rdname prediction
#' @export
prediction.polr <- function(model, data = find_data(model, parent.frame()), ...) {
    # setup data
    data <- data
    
    # reduce memory profile
    model[["model"]] <- NULL
    attr(model[["terms"]], ".Environment") <- NULL
    
    # extract predicted value at input value (value can only be 1 number)
    pred <- data.frame(fit = predict(model, newdata = data, type = "class", ...))
    pred[["se.fit"]] <- NA_real_
    class(pred[["fit"]]) <- c("fit", class(pred[["fit"]]))
    class(pred[["se.fit"]]) <- c("se.fit", "numeric")
    probs <- as.data.frame(predict(model, newdata = data, type = "probs", ...))
    names(probs) <- paste0("Pr(", names(probs), ")")
    
    # obs-x-2 data.frame of predictions
    structure(cbind(list(fitted = pred[["fit"]], 
                         se.fitted = pred[["se.fit"]]),
                    probs),
              class = c("prediction", "data.frame"), 
              row.names = seq_len(length(pred[["fit"]])),
              model.class = class(model),
              type = NULL)
}
