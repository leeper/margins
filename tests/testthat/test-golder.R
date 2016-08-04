# formulae from Matt Golder's examples

# example data for tests
set.seed(1)
n <- 50
d <- data.frame(w = rnorm(n),
                x = rnorm(n),
                z = rnorm(n))
d[["y"]] <- w + x + z + w*x + w*z + x*z * w*x*z + rnorm(n)

# set comparison tolerance
tol <- 0.0001

context("Golder Tests (Interaction Terms)")
# http://mattgolder.com/wp-content/uploads/2015/05/standarderrors1.png

test_that("Golder Interaction Case 1a/1b correct", {
    f1.1 <- y ~ x + z + x:z
    m <- lm(f1.1, data = d)
    e <- marginal_effects(m)
    # ME with respect to x
    dydx <- coef(m)["x"] + (z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["z","z"] + (z^2 * vcov(m)["x:z","x:z"]) + (2 * z * vcov(m)["z","x:z"]))
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
    # ME with respect to z
    dydz <- coef(m)["z"] + (x * coef(m)["x:z"])
    sedydz <- sqrt(vcov(m)["z","z"] + (x^2 * vcov(m)["x:z","x:z"]) + (2 * x * vcov(m)["x","x:z"]))
    expect_equal(unclass(e[,"z"]), dydz, tolerance = tol, label = "dy/dz correct")
    #expect_equal(sedydz, s[s[["Factor"]] == "z", "Std.Err."], tolerance = tol, label = "Var(dy/dz) correct")
})

test_that("Golder Interaction Case 2 correct", {
    f1.2 <- y ~ x + z + w + x:z + z:w
    m <- lm(f1.2, data = d)
    e <- marginal_effects(m)
    dydx <- coef(m)["x"] + (z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x","x"] + (z^2 * vcov(m)["x:z","x:z"]) + (2 * z * vcov(m)["x","x:z"]))
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
})

test_that("Golder Interaction Case 3 correct", {
    f1.3 <- y ~ x + z + w + x:z + x:w + z:w
    m <- lm(f1.3, data = d)
    e <- marginal_effects(m)
    dydx <- coef(m)["x"] + (z * coef(m)["x:z"]) + (w * coef(m)["x:w"])
    sedydx <- sqrt(vcov(m)["x","x"] + (z^2 * vcov(m)["x:z","x:z"]) + (w^2 * vcov(m)["x:w","x:w"]) + 
                   (2 * z * vcov(m)["x","x:z"]) + (2 * w * vcov(m)["x","x:w"]) + (2 * z * w * vcov(m)["x:z","x:w"]) )
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
})

test_that("Golder Interaction Case 4 correct", {
    f1.4 <- y ~ x + z + w + x:z + x:w + z:w + x:z:w
    m <- lm(f1.4, data = d)
    e <- marginal_effects(m)
    dydx <- coef(m)["x"] + (z * coef(m)["x:z"]) + (w * coef(m)["x:w"]) + (z * w * coef(m)["x:z:w"])
    sedydx <- sqrt(vcov(m)["x","x"] + (z^2 * vcov(m)["x:z","x:z"]) + (w^2 * vcov(m)["x:w","x:w"]) + 
                   (z^2 * w^2 * vcov(m)["x:z:w","x:z:w"]) + (2 * z * vcov(m)["x","x:z"]) + 
                   (2 * w * vcov(m)["x","x:w"]) + (2 * z * w * vcov(m)["x","x:z:w"]) + 
                   (2 * z * w * vcov(m)["x:z","x:w"]) + (2 * w * z^2 * vcov(m)["x:z","x:z:w"]) +
                   (2 * z * w^2 * vcov(m)["x:w","x:z:w"]) )
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
})

context("Golder Tests (Quadratic Terms)")
# http://mattgolder.com/wp-content/uploads/2015/05/standarderrors2.png

test_that("Golder Quadratic Case 1 correct", {
    f2.1 <- y ~ x + I(x^2)
    m <- lm(f2.1, data = d)
    e <- marginal_effects(m)
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * x)
    sedydx <- sqrt(vcov(m)["x","x"] + (4 * x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (4 * x * vcov(m)["x","I(x^2)"]))
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
})

test_that("Golder Quadratic Case 2 correct", {
    f2.2 <- y ~ x + I(x^2) + z
    m <- lm(f2.2, data = d)
    e <- marginal_effects(m)
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * x)
    sedydx <- sqrt(vcov(m)["x","x"] + (4 * x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (4 * x * vcov(m)["x","I(x^2)"]))
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
})

test_that("Golder Quadratic Case 3a/3b correct", {
    f2.3 <- y ~ x + I(x^2) + z + x:z
    m <- lm(f2.3, data = d)
    e <- marginal_effects(m)
    # ME with respect to x
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * x) + (z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x","x"] + (4 * x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (z^2 * vcov(m)["x:z","x:z"]) +
                   (4 * x * vcov(m)["x","I(x^2)"]) + (2 * z * vcov(m)["x","x:z"]) + (4 * x * z * vcov(m)["I(x^2)", "x:z"]) )
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
    # ME with respect to z
    dydz <- coef(m)["z"] + (x * coef(m)["x:z"])
    sedydz <- sqrt(vcov(m)["z","z"] + (x^2 * vcov(m)["x:z","x:z"]) + (2 * x * vcov(m)["z","x:z"]))
    expect_equal(unclass(e[,"z"]), dydz, tolerance = tol, label = "dy/dz correct")
    #expect_equal(sedydz, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dz) correct")
})

test_that("Golder Quadratic Case 4a/4b correct", {
    f2.4 <- y ~ x + I(x^2) + z + x:z + I(x^2):z
    m <- lm(f2.4, data = d)
    e <- marginal_effects(m)
    # ME with respect to x
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * x) + (z * coef(m)["x:z"]) + (2 * x * z * coef(m)["I(x^2):z"])
    sedydx <- sqrt(vcov(m)["x","x"] + (4 * x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (z^2 * vcov(m)["x:z","x:z"]) +
                   (4 * (x^2) * (z^2) * vcov(m)["x","I(x^2)"]) + (4 * x * vcov(m)["x","I(x^2)"]) + 
                   (4 * z * vcov(m)["x","x:z"]) + (4 * x * z * vcov(m)["I(x^2)", "x:z"]) +
                   (4 * x * z * vcov(m)["x","I(x^2):z"]) + (8 * (x^2) * z * vcov(m)["I(x^2)","I(x^2):z"]) + 
                   (4 * x * (z^2) * vcov(m)["x:z","I(x^2):z"]) )
    expect_equal(unclass(e[,"x"]), dydx, tolerance = tol, label = "dy/dx correct")
    #expect_equal(sedydx, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dx) correct")
    # ME with respect to z
    dydz <- coef(m)["z"] + (x * coef(m)["x:z"]) + (x^2 * coef(m)["I(x^2):z"])
    sedydz <- sqrt(vcov(m)["z","z"] + (x^2 * vcov(m)["x:z","x:z"]) + (x^4 * vcov(m)["I(x^2):z","I(x^2):z"]) + 
                   (2 * x * vcov(m)["z","x:z"]) + (2 * (x^2) * vcov(m)["z","I(x^2):z"]) + 
                   (2 * (x^3) * vcov(m)["x:z","I(x^2):z"]) )
    expect_equal(unclass(e[,"z"]), dydz, tolerance = tol, label = "dy/dz correct")
    #expect_equal(sedydz, s[s[["Factor"]] == "x", "Std.Err."], tolerance = tol, label = "Var(dy/dz) correct")
})
