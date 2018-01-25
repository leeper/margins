# set comparison tolerance
tol <- 0.0001

library("datasets")

context("Edge case tests")
test_that("margins() works with multicollinearity", {
    x <- lm(mpg ~ wt + I(wt*2), data = mtcars)
    suppressWarnings(m <- margins(x))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 1L)
})

test_that("margins() works with only a term in I()", {
    x <- lm(mpg ~ I(wt*2), data = mtcars)
    suppressWarnings(m <- margins(x))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 1L)
})

test_that("margins() works with missing factor levels", {
    mtcars2 <- mtcars
    mtcars2$cyl <- factor(mtcars2$cyl)
    mtcars2$gear <- factor(mtcars2$gear)
    x <- lm(mpg ~ cyl * gear, data = mtcars2)
    suppressWarnings(m <- margins(x))
    expect_true(inherits(m, "data.frame"))
    expect_true(nrow(summary(m)) == 4L)
})

