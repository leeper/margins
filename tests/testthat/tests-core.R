# set comparison tolerance
tol <- 0.0001

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

test_that("AME/MEM identical in OLS models", {
    x <- lm(mpg ~ cyl * hp + wt, data = head(mtcars))
    expect_equal(mean(extract_marginal_effects(margins(x, atmeans = TRUE))), 
                 mean(extract_marginal_effects(margins(x, atmeans = FALSE))), tolerance = tol)
})

test_that("AME/MEM not identical in GLMs", {
    x <- glm(am ~ hp, data = head(mtcars), family = binomial)
    expect_false(identical(mean(extract_marginal_effects(margins(x, atmeans = TRUE))[[1]][[1]]), 
                           mean(extract_marginal_effects(margins(x, atmeans = FALSE))[[1]][[1]])))
})


context("print() and summary() methods")
test_that("print()/summary() 'margins' object", {
    x <- lm(mpg ~ wt * hp, data = head(mtcars))
    m <- margins(x)
    expect_true(inherits(print(m), "marginslist"), label = "print() method for marginslist")
    expect_true(inherits(print(m[[1]]), "margins"), label = "print() method for margins")
    expect_true(inherits(print(m), "marginslist"), label = "summary() method for marginslist")
    expect_true(inherits(print(m[[1]]), "margins"), label = "summary() method for margins")
})


context("Plotting")
test_that("persp() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(is.list(persp(x)))
})

test_that("cplot() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(inherits(cplot(x, what = "prediction"), "data.frame"))
    expect_true(inherits(cplot(x, what = "effect"), "data.frame"))
})

test_that("plot() method for 'margins' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(inherits(plot(margins(x)), "margins"))
})
