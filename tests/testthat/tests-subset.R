# tests for functionality of 'variables' argument in `marginal_effects()`
context("Test if users can choose a subset of regressors to compute margins")

# Simulated dataset and logistic regression with a factor variable
set.seed(1024)
N <- 50
dat <- data.frame('y' = sample(0:1, N, replace = TRUE),
                  'x' = rnorm(N),
                  'z' = rnorm(N),
                  'w' = sample(letters[1:5], N, replace = TRUE))
mod <- glm(y ~ x + z + w, data = dat, family = binomial())

# Tests
test_that("Correct attributes are returned by marginal_effects(variables =)", {
    mfx_subset1 <- marginal_effects(mod, variables = 'x') 
    expect_equal(names(mfx_subset1), 'dydx_x')

    mfx_subset2 <- marginal_effects(mod, variables = c('x', 'z')) 
    expect_equal(names(mfx_subset2), c('dydx_x', 'dydx_z'))

    mfx_factor <- marginal_effects(mod, variables = c('x', 'w'))
    expect_equal(names(mfx_factor), c("dydx_x", "dydx_wb", "dydx_wc",
                                      "dydx_wd", "dydx_we"))

    mfx_all <- marginal_effects(mod)
    expect_equal(names(mfx_all), c("dydx_x", "dydx_z", "dydx_wb", "dydx_wc",
                                   "dydx_wd", "dydx_we")) 
})

test_that("Correct attributes are returned by margins(variables =)", {
    margins_subset <- margins(mod, variables = 'x')
    expect_equal(names(margins_subset), c("y", "x", "z", "w", "fitted",
                                          "se.fitted", "dydx_x",
                                          "Var_dydx_x", "_weights", "_at_number"))

    margins_all <- margins(mod)
    expect_equal(names(margins_all), c("y", "x", "z", "w", "fitted", "se.fitted",
                                       "dydx_x", "dydx_z", "dydx_wb",
                                       "dydx_wc", "dydx_wd", "dydx_we",
                                       "Var_dydx_x", "Var_dydx_z",
                                       "Var_dydx_wb", "Var_dydx_wc",
                                       "Var_dydx_wd", "Var_dydx_we",
                                       "_weights", "_at_number"))
})

test_that("Expected error returned by marginal_effects(variables=)/margins(variables =)", {
    expect_error(marginal_effects(mod, variables = 'FAKEVARIABLE'))
    expect_error(margins(mod, variables = 'FAKEVARIABLE'))
})
