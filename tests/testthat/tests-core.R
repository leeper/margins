# tests core functinality and data structures

# set comparison tolerance
tol <- 0.0001

library("datasets")

context("Basic accuracy tests")
test_that("Test accuracy for lm()", {
    x <- lm(mpg ~ wt, data = mtcars)
    m <- marginal_effects(x)
    expect_equal(coef(x)[["wt"]], mean(m[["dydx_wt"]]), tolerance = tol, label = "marginal effect is coefficient in lm()")
})
test_that("Test accuracy for glm()", {
    x <- glm(am ~ wt, data = mtcars, family = binomial)
    m1 <- marginal_effects(x)
    expect_true(coef(x)[["wt"]] != mean(m1[["dydx_wt"]]), label = "marginal effect (type = 'response') is not coefficient in glm()")
    m1b <- marginal_effects(x, type = "link")
    expect_equal(coef(x)[["wt"]], mean(m1b[["dydx_wt"]]), tolerance = tol, label = "marginal effect (type = 'link') is coefficient in glm()")
    m2 <- marginal_effects(x, type = "link")
    expect_equal(coef(x)[["wt"]], mean(m2[["dydx_wt"]]), tolerance = tol, label = "marginal effect is not coefficient in glm()")
    p <- predict(x, type = "response")
    manual <- coef(x)[["wt"]] * p * (1-p)
    expect_equal(as.numeric(manual), as.numeric(m1[["dydx_wt"]]), tolerance = tol, label = "marginal effect is correct for logit glm()")
})
test_that("Test accuracy for loess()", {
    x <- loess(mpg ~ wt, data = mtcars)
    expect_true(inherits(m <- margins(x), "margins"), label = "margins works for loess()")
})


context("Test `build_datalist()` behavior")
test_that("Test build_datalist()", {
    expect_true(length(build_datalist(mtcars, at = list(cyl = c(4,6)))) == 2)
    expect_true(length(build_datalist(mtcars, at = list(cyl = c(4,6), wt = c(1.6,1.8)))) == 4)
    m <- mtcars
    m[["cyl"]] <- factor(m[["cyl"]])
    expect_error(build_datalist(m, at = list(cyl = 10)), label = "factor error in build_datalist()")
    expect_warning(build_datalist(mtcars, at = list(wt = 100)), label = "extrapolation warning in build_datalist()")
    rm(m)
})


context("Test `at` behavior")
test_that("`at` behavior works and warnings/errors occur as expected", {
    x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
    expect_true(inherits(margins(x, at = list(cyl = c(4,6))), "margins"), label = "factor works")
    expect_warning(margins(x, at = list(wt = 6)), label = "extrapolation warning with 'at' as expected")
    m <- mtcars
    m[["cyl"]] <- factor(m[["cyl"]])
    x <- lm(mpg ~ cyl * hp + wt, data = head(m))
    expect_error(margins(x, at = list(cyl = 2)), label = "illegal factor error with 'at' occurs as expected")
})

test_that("factor variables work", {
    x1 <- lm(mpg ~ factor(cyl), data = head(mtcars))
    expect_true(inherits(marginal_effects(x1), "data.frame"), label = "factors work in formula") 
    x2 <- lm(Sepal.Length ~ Species, data = iris)
    expect_true(inherits(marginal_effects(x2), "data.frame"), label = "natural factors work")
})

test_that("dydx() works", {
    mtcars$am <- as.logical(mtcars$am)
    mtcars$cyl <- factor(mtcars$cyl)
    x <- lm(mpg ~ wt + am + cyl, data = head(mtcars))
    expect_true(inherits(dydx(head(mtcars), x, "wt"), "data.frame"), label = "dydx dispatch works for numeric")
    expect_true(inherits(dydx(head(mtcars), x, "cyl"), "data.frame"), label = "dydx dispatch works for factor")
    expect_true(inherits(dydx(head(mtcars), x, "am"), "data.frame"), label = "dydx dispatch works for logical")
    expect_true(inherits(marginal_effects(x), "data.frame"), label = "dydx dispatch works via marginal_effects()")    
    rm(mtcars)
})

test_that("alternative dydx() args", {
    x <- lm(mpg ~ wt, data = head(mtcars))
    expect_true(inherits(dydx(head(mtcars), x, "wt", change = "minmax"), "data.frame"), label = "dydx w/ change = 'minimax'")
    expect_true(inherits(dydx(head(mtcars), x, "wt", change = "iqr"), "data.frame"), label = "dydx w/ change = 'iqr'")
    expect_true(inherits(dydx(head(mtcars), x, "wt", change = "sd"), "data.frame"), label = "dydx w/ change = 'sd'")
    expect_true(inherits(dydx(head(mtcars), x, "wt", change = range(mtcars[["wt"]], na.rm = TRUE)), "data.frame"), label = "dydx w/ change = c(a,b)")
    expect_error(dydx(head(mtcars), x, "wt", change = !L), label = "error in dydx w/ change = 1L")
    rm(mtcars)
})


context("print(), summary(), and confint() methods")
test_that("print()/summary() for 'margins' object", {
    x <- lm(mpg ~ wt * hp, data = head(mtcars))
    m <- margins(x)
    expect_true(inherits(print(m), "margins"), label = "print() method for margins w/o 'at' specification")
    expect_true(inherits(print(margins(x, at = list(wt = 3))), "margins"), label = "print() method for margins w 'at' specification")
    expect_true(inherits(summary(m), "data.frame"), label = "summary() method for margins")
    expect_true(inherits(print(summary(m)), "data.frame"), label = "print() method for summary.margins")
})
test_that("confint() for 'margins' object", {
    x <- lm(mpg ~ wt * hp, data = head(mtcars))
    m <- margins(x)
    expect_true(inherits(confint(m), "matrix"), label = "confint() for margins")
})


context("Variance tests")
test_that("minimum test of variance calculations", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(inherits(margins(x, vce = "delta"), "margins"), label = "margins(vce ='delta') works")
    expect_true(inherits(margins(x, vce = "simulation", iter = 5L), "margins"), label = "margins(vce ='simulation') works")
    expect_true(inherits(margins(x, vce = "bootstrap", iter = 5L), "margins"), label = "margins(vce ='bootstrap') works")
})
test_that("vcov.margins() method words", {
    m <- margins(x <- lm(mpg ~ wt * hp, data = mtcars))
    expect_true(inherits(vcov(m), "matrix"), label = "vcov() method words")
    expect_true(identical(dim(vcov(m)), c(2L, 2L)), label = "vcov.margins() has correct dimensions")
    
})
