# set comparison tolerance
tol <- 0.0001

library("datasets")

context("Test `prediction()` behavior")
test_that("Test build_datalist()", {
    expect_true(inherits(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars), "data.frame"), label = "prediction() works w data arg (LM)")
    expect_true(inherits(prediction(lm(mpg ~ cyl, data = mtcars), data = mtcars), "data.frame"), label = "prediction() works w data arg (GLM)")
    expect_true(inherits(prediction(lm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "prediction() works w/o data arg (LM)")
    expect_true(inherits(prediction(lm(mpg ~ cyl, data = mtcars)), "data.frame"), label = "prediction() works w/o data arg (GLM)")
})

context("Test `build_datalist()` behavior")
test_that("Test build_datalist()", {
    expect_true(length(build_datalist(mtcars, at = list(cyl = c(4,6)))) == 2)
    expect_true(length(build_datalist(mtcars, at = list(cyl = c(4,6), wt = c(1,1.5)))) == 4)
    #expect_error(build_datalist(mtcars, at = list(cyl = 8)), label = "factor error in build_datalist()")
    expect_warning(build_datalist(mtcars, at = list(wt = 100)), label = "extrapolation warning in build_datalist()")
})


context("Test `at` behavior")
test_that("`at` behavior works", {
    x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
    expect_true(inherits(margins(x, at = list(cyl = c(4,6))), "marginslist"), label = "factor works")
    #expect_error(margins(x, at = list(cyl = 2)), label = "factor error")
    expect_warning(margins(x, at = list(wt = 6)), label = "extrapolation warning")
})

test_that("factor variables work", {
    x1 <- lm(mpg ~ factor(cyl), data = head(mtcars))
    expect_true(inherits(marginal_effects(x1), "data.frame"), label = "factors work in formula") 
    x2 <- lm(Sepal.Length ~ Species, data = iris)
    expect_true(inherits(marginal_effects(x2), "data.frame"), label = "natural factors work")
})


context("print(), summary(), and confint() methods")
test_that("print()/summary() for 'margins' object", {
    x <- lm(mpg ~ wt * hp, data = head(mtcars))
    m <- margins(x)
    expect_true(inherits(print(m), "marginslist"), label = "print() method for marginslist")
    expect_true(inherits(print(m[[1]]), "margins"), label = "print() method for margins")
    expect_true(inherits(summary(m), "list"), label = "summary() method for marginslist")
    expect_true(inherits(summary(m[[1]]), "data.frame"), label = "summary() method for margins")
})
test_that("confint() for 'margins' object", {
    x <- lm(mpg ~ wt * hp, data = head(mtcars))
    m <- margins(x)
    expect_true(inherits(confint(m[[1]]), "matrix"), label = "confint() for margins")
})


context("Plotting")
test_that("persp() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(is.list(persp(x)))
    expect_true(is.list(persp(x, theta = c(30, 60))))
    expect_true(is.list(persp(x, theta = c(30, 60), phi = c(0, 10))))
    expect_true(is.list(persp(x, what = "effect")))
})

test_that("cplot() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(inherits(cplot(x, what = "prediction"), "data.frame"))
    expect_true(inherits(cplot(x, what = "prediction", se.type = "shade"), "data.frame"))
    expect_true(inherits(cplot(x, what = "effect"), "data.frame"))
})

test_that("plot() method for 'margins' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(inherits(plot(margins(x)), "margins"))
    expect_true(inherits(plot(margins(x), horizontal = TRUE), "margins"))
})
