#' @title Extract data from a model object
#' @description Find a model frame in a model object or try to reconstruct one
#' @param model The model object.
#' @param env An environment in which to look for the \code{data} argument to the modelling call.
#' @return A data.frame, typically with one column unless the variable is a factor with more than two levels.
#' @examples
#' require("datasets")
#' x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
#' find_data(x)
#' 
#' @seealso \code{\link{marginal_effects}}, \code{\link{margins}}
#' @export
find_data <- function(model, env = parent.frame()) {
    if (!is.null(model[["call"]][["data"]])) {
        data <- eval(model[["call"]][["data"]], env) 
    } else { 
        data <- get_all_vars(model[["terms"]], data = model[["model"]])
    }
    data
}
