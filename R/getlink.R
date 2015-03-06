getlink <- function(x) {
    dfun <- switch(x, 
                  probit = dnorm, 
                  logit = dlogis,
                  cauchit = dcauchy,
                  log = exp,
                  cloglog = function(z) 1 - exp(-exp(z)),
                  inverse = function(z) 1/z,
                  identity = function(z) 1,
                  sqrt = function(z) z^2,
                  "1/mu^2" = function(z) z^(-0.5),
                  stop("Unrecognized link function")
                  )
    sfun <- switch(x, 
                  probit = function(z) -z, 
                  logit = function(z) plogis(z),
                  cauchit = function(z) 1, # not setup
                  log = function(z) 1, # not setup
                  cloglog = function(z) 1, # not setup
                  inverse = function(z) 1, # not setup
                  identity = function(z) 1, # not setup
                  sqrt = function(z) 1, # not setup
                  "1/mu^2" = function(z) 1, # not setup
                  stop("Unrecognized link function")
                  )
    list(dfun = dfun, sfun = sfun)
}
