# formulae from Matt Golder's examples
# http://mattgolder.com/wp-content/uploads/2015/05/standarderrors1.png
# http://mattgolder.com/wp-content/uploads/2015/05/standarderrors2.png

# example data for tests
set.seed(1)
n <- 50
w <- rnorm(n)
x <- rnorm(n)
z <- rnorm(n)
y <- w + x + z + w*x + w*z + x*z * w*x*z + rnorm(n)

context("Golder Tests (Interaction Terms)")
test_that("Golder Case 1a/1b correct", {
    f1.1 <- y ~ x + z + x:z
    m <- lm(f1.1)
    s <- summary(margins(m)
    # with respect to x
    ## NOTE: coefficient vector begins with intercept so use character extraction for clarity
    dydx <- coef(m)["x"] + (z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["z","z"] + (z^2 * vcov(m)["x:z","x:z"]) + (2 * z * vcov(m)["z","x:z"]))
    expect_equal(dydx, s[s[["Factor"]] == "x", "dy/dx"], label = "dy/dx correct")
    expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], label = "Var(dy/dx) correct")
    # with respect to z
    dydz <- coef(m)[1+2] + (x * coef(m)[3])
    sedydz <- sqrt(vcov(m)["z","z"] + (x^2 * vcov(m)["x:z","x:z"]) + (2 * x * vcov(m)["x","x:z"]))
    expect_equal(dydz, s[s[["Factor"]] == "z", "dy/dx"], label = "dy/dz correct")
    expect_equal(sedydz, s[s[["Factor"]] == "z", "Std.Err."], label = "Var(dy/dz) correct")
})

test_that("Golder Case 2 correct", {
    f1.2 <- y ~ x + z + w + x:z + z:w
    m <- lm(f1.2)
    s <- summary(margins(m)
    dydx <- coef(m)["z"] + (z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x","x"] + (z^2 * vcov(m)["x:z","x:z"]) + (2 * z * vcov(m)["x","x:z"]))
    expect_equal(dydx, s[s[["Factor"]] == "x", "dy/dx"], label = "dy/dx correct")
    expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], label = "Var(dy/dx) correct")
})

test_that("Golder Case 3 correct", {
    f1.3 <- y ~ x + z + w + x:z + x:w + z:w
    m <- lm(f1.3)
    s <- summary(margins(m)
    dydx <- coef(m)["x"] + (z * coef(m)["x:z"]) + (w * coef(m)["x:w"])
    sedydx <- sqrt(vcov(m)["x","x"] + (z^2 * vcov(m)["x:z","x:z"]) + (w^2 * vcov(m)["x:w","x:w"]) + 
                   (2 * z * vcov(m)["x","x:z"]) + (2 * w * vcov(m)["x","x:w"]) + (2 * z * w * vcov(m)["x:z","x:w"]) )
    expect_equal(dydx, s[s[["Factor"]] == "x", "dy/dx"], label = "dy/dx correct")
    expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], label = "Var(dy/dx) correct")
})

test_that("Golder Case 4 correct", {
    f1.4 <- y ~ x + z + w + x:z + x:w + z:w + x:z:w
    m <- lm(f1.4)
    s <- summary(margins(m)
    dydx <- coef(m)["x"] + (z * coef(m)["x:z"]) + (w * coef(m)["x:w"]) + (z * w * coef(m)["x:z:w"])
    sedydx <- sqrt(vcov(m)["x","x"] + (z^2 * vcov(m)["x:z","x:z"]) + (w^2 * vcov(m)["x:w","x:w"]) + 
                   (z^2 * w^2 * vcov(m)["x:z:w","x:z:w"]) + (2 * z * vcov(m)["x","x:z"]) + 
                   (2 * w * vcov(m)["x","x:w"]) + (2 * z * w * cov(m)["x","x:z:w"]) + 
                   (2 * z * w * vcov(m)["x:z","x:w"]) + (2 * w * z^2 * vcov(m)["x:z","z:x:w"]) +
                   (2 * z * w^2 * vcov(m)["x:w","x:z:w"]) )
    expect_equal(dydx, s[s[["Factor"]] == "x", "dy/dx"], label = "dy/dx correct")
    expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], label = "Var(dy/dx) correct")
})



f2.1 <- y ~ x + I(x^2)
f2.2 <- y ~ x + I(x^2) + z
f2.3a <- y ~ x + I(x^2) + z + x:z # with respect to x
f2.3b <- y ~ x + I(x^2) + z + x:z # with respect to z
f2.4a <- y ~ x + I(x^2) + z + x:z + I(x^2):z # with respect to x
f2.4b <- y ~ x + I(x^2) + z + x:z + I(x^2):z # with respect to z

