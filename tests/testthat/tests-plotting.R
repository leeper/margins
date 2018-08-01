# test that plotting methods execute correctly
context("Plotting")
test_that("persp() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(is.list(persp(x)))
    expect_true(is.list(persp(x, theta = c(30, 60))))
    expect_true(is.list(persp(x, theta = c(30, 60), phi = c(0, 10))))
    expect_true(is.list(persp(x, phi = c(0, 10))))
    expect_true(is.list(persp(x, what = "effect")))
})

test_that("image() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(is.null(image(x)))
    expect_true(is.null(image(x, what = "effect")))
})

test_that("cplot() method for 'lm' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(inherits(cplot(x, what = "prediction"), "data.frame"))
    expect_true(inherits(cplot(x, what = "prediction", se.type = "lines"), "data.frame"))
    expect_true(inherits(cplot(x, what = "prediction", se.type = "shade"), "data.frame"))
    expect_true(inherits(cplot(x, what = "effect"), "data.frame"))
    x <- lm(mpg ~ factor(cyl), data = mtcars)
    #expect_true(inherits(cplot(x, what = "prediction"), "data.frame"))
})

#test_that("cplot() work for factor variable", {
#    x <- lm(mpg ~ factor(cyl), data = mtcars)
#    expect_true(inherits(cplot(x, what = "prediction"), "data.frame"))
#    expect_true(inherits(cplot(x, what = "effect"), "data.frame"))
#})

test_that("plot() method for 'margins' works", {
    x <- lm(mpg ~ wt * hp, data = mtcars)
    expect_true(inherits(plot(margins(x)), "margins"))
    expect_true(inherits(plot(margins(x), horizontal = TRUE), "margins"))
})
