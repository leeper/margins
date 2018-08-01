# tests based on formulae from Matt Golder's OLS examples, for numerical accuracy and precision

# example data for tests
set.seed(1)
n <- 25L
d <- data.frame(w = rnorm(n),
                x = rnorm(n),
                z = rnorm(n))
d[["y"]] <- with(d, w + x + z + w*x + w*z + x*z * w*x*z + rnorm(n))

# set comparison tolerance
tol <- 0.0001
tol_se <- 0.005

context("Golder Tests (Interaction Terms)")
# http://mattgolder.com/wp-content/uploads/2015/05/standarderrors1.png

test_that("Golder Interaction Case 1a/1b correct", {
    f1.1 <- y ~ x + z + x:z
    m <- lm(f1.1, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    # ME with respect to x
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x","x"] + (d$z^2 * vcov(m)["x:z","x:z"]) + (2 * d$z * vcov(m)["x","x:z"]))
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
    # ME with respect to z
    dydz <- coef(m)["z"] + (d$x * coef(m)["x:z"])
    sedydz <- sqrt(vcov(m)["z","z"] + (d$x^2 * vcov(m)["x:z","x:z"]) + (2 * d$x * vcov(m)["z","x:z"]))
    expect_equal(as.numeric(e[,"dydx_z"]), dydz, tolerance = tol, label = "dy/dz correct")
    expect_equal(sedydz, as.numeric(marg[["SE_dydx_z"]]), tolerance = tol_se, label = "Var(dy/dz) correct")
})

test_that("Golder Interaction Case 2 correct", {
    f1.2 <- y ~ x + z + w + x:z + z:w
    m <- lm(f1.2, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x","x"] + (d$z^2 * vcov(m)["x:z","x:z"]) + (2 * d$z * vcov(m)["x","x:z"]))
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
})

test_that("Golder Interaction Case 3 correct", {
    f1.3 <- y ~ x + z + w + x:z + x:w + z:w
    m <- lm(f1.3, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"]) + (d$w * coef(m)["x:w"])
    sedydx <- sqrt(vcov(m)["x","x"] + (d$z^2 * vcov(m)["x:z","x:z"]) + (d$w^2 * vcov(m)["x:w","x:w"]) + 
                   (2 * d$z * vcov(m)["x","x:z"]) + (2 * d$w * vcov(m)["x","x:w"]) + (2 * d$z * d$w * vcov(m)["x:z","x:w"]) )
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
})

test_that("Golder Interaction Case 4 correct", {
    f1.4 <- y ~ x + z + w + x:z + x:w + z:w + x:z:w
    m <- lm(f1.4, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    dydx <- coef(m)["x"] + (d$z * coef(m)["x:z"]) + (d$w * coef(m)["x:w"]) + (d$z * d$w * coef(m)["x:z:w"])
    sedydx <- sqrt(vcov(m)["x","x"] + (d$z^2 * vcov(m)["x:z","x:z"]) + (d$w^2 * vcov(m)["x:w","x:w"]) + 
                   (d$z^2 * d$w^2 * vcov(m)["x:z:w","x:z:w"]) + (2 * d$z * vcov(m)["x","x:z"]) + 
                   (2 * d$w * vcov(m)["x","x:w"]) + (2 * d$z * d$w * vcov(m)["x","x:z:w"]) + 
                   (2 * d$z * d$w * vcov(m)["x:z","x:w"]) + (2 * d$w * d$z^2 * vcov(m)["x:z","x:z:w"]) +
                   (2 * d$z * d$w^2 * vcov(m)["x:w","x:z:w"]) )
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
})

context("Golder Tests (Quadratic Terms)")
# http://mattgolder.com/wp-content/uploads/2015/05/standarderrors2.png

test_that("Golder Quadratic Case 1 correct", {
    f2.1 <- y ~ x + I(x^2)
    m <- lm(f2.1, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * d$x)
    sedydx <- sqrt(vcov(m)["x","x"] + (4 * d$x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (4 * d$x * vcov(m)["x","I(x^2)"]))
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
})

test_that("Golder Quadratic Case 2 correct", {
    f2.2 <- y ~ x + I(x^2) + z
    m <- lm(f2.2, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * d$x)
    sedydx <- sqrt(vcov(m)["x","x"] + (4 * d$x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (4 * d$x * vcov(m)["x","I(x^2)"]))
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
})

test_that("Golder Quadratic Case 3a/3b correct", {
    f2.3 <- y ~ x + I(x^2) + z + x:z
    m <- lm(f2.3, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    # ME with respect to x
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * d$x) + (d$z * coef(m)["x:z"])
    sedydx <- sqrt(vcov(m)["x","x"] + (4 * d$x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (d$z^2 * vcov(m)["x:z","x:z"]) +
                   (4 * d$x * vcov(m)["x","I(x^2)"]) + (2 * d$z * vcov(m)["x","x:z"]) + (4 * d$x * d$z * vcov(m)["I(x^2)", "x:z"]) )
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
    # ME with respect to z
    dydz <- coef(m)["z"] + (d$x * coef(m)["x:z"])
    sedydz <- sqrt(vcov(m)["z","z"] + (d$x^2 * vcov(m)["x:z","x:z"]) + (2 * d$x * vcov(m)["z","x:z"]))
    expect_equal(as.numeric(e[,"dydx_z"]), dydz, tolerance = tol, label = "dy/dz correct")
    expect_equal(sedydz, as.numeric(marg[["SE_dydx_z"]]), tolerance = tol_se, label = "Var(dy/dz) correct")
})

test_that("Golder Quadratic Case 4a/4b correct", {
    f2.4 <- y ~ x + I(x^2) + z + x:z + I(x^2):z
    m <- lm(f2.4, data = d)
    marg <- margins(m, unit_ses = TRUE)
    e <- marginal_effects(marg)
    # ME with respect to x
    dydx <- coef(m)["x"] + (2 * coef(m)["I(x^2)"] * d$x) + (d$z * coef(m)["x:z"]) + (2 * d$x * d$z * coef(m)["I(x^2):z"])
    sedydx <- sqrt( vcov(m)["x","x"] + (4 * d$x^2 * vcov(m)["I(x^2)","I(x^2)"]) + (d$z^2 * vcov(m)["x:z","x:z"]) +
                   (4 * (d$x^2) * (d$z^2) * vcov(m)["I(x^2):z","I(x^2):z"]) + 
                   (4 * d$x * vcov(m)["x","I(x^2)"]) + (2 * d$z * vcov(m)["x","x:z"]) + 
                   (4 * d$x * d$z * vcov(m)["I(x^2)", "x:z"]) +
                   (4 * d$x * d$z * vcov(m)["x","I(x^2):z"]) + 
                   (8 * (d$x^2) * d$z * vcov(m)["I(x^2)","I(x^2):z"]) + 
                   (4 * d$x * (d$z^2) * vcov(m)["x:z","I(x^2):z"]) )
    expect_equal(as.numeric(e[,"dydx_x"]), dydx, tolerance = tol, label = "dy/dx correct")
    expect_equal(sedydx, as.numeric(marg[["SE_dydx_x"]]), tolerance = tol_se, label = "Var(dy/dx) correct")
    # ME with respect to z
    dydz <- coef(m)["z"] + (d$x * coef(m)["x:z"]) + (d$x^2 * coef(m)["I(x^2):z"])
    sedydz <- sqrt(vcov(m)["z","z"] + (d$x^2 * vcov(m)["x:z","x:z"]) + (d$x^4 * vcov(m)["I(x^2):z","I(x^2):z"]) + 
                   (2 * d$x * vcov(m)["z","x:z"]) + (2 * (d$x^2) * vcov(m)["z","I(x^2):z"]) + 
                   (2 * (d$x^3) * vcov(m)["x:z","I(x^2):z"]) )
    expect_equal(as.numeric(e[,"dydx_z"]), dydz, tolerance = tol, label = "dy/dz correct")
    expect_equal(sedydz, as.numeric(marg[["SE_dydx_z"]]), tolerance = tol_se, label = "Var(dy/dz) correct")
})
