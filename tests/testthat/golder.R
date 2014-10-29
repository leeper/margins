set.seed(1)

n <- 50
w <- rnorm(n)
x <- rnorm(n)
z <- rnorm(n)
y <- w + x + z + w*x + w*z + x*z * w*x*z + rnorm(n)

# formulae from Matt Golder's examples
f1.1a <- y ~ x + z + x:z # with respect to x
f1.1b <- y ~ x + z + x:z # with respect to x
f1.2 <- y ~ x + z + w + x:z + z:w
f1.3 <- y ~ x + z + w + x:z + x:w + z:w
f1.4 <- y ~ x + z + w + x:z + x:w + z:w + x:z:w

f2.1 <- y ~ x + I(x^2)
f2.2 <- y ~ x + I(x^2) + z
f2.3a <- y ~ x + I(x^2) + z + x:z # with respect to x
f2.3b <- y ~ x + I(x^2) + z + x:z # with respect to z
f2.4a <- y ~ x + I(x^2) + z + x:z + I(x^2):z # with respect to x
f2.4b <- y ~ x + I(x^2) + z + x:z + I(x^2):z # with respect to z


# playing around with some code
parseformula <- function(model) {
    f <- as.character(as.expression(terms(model)))
    formula(chartr(":","*",f), env = parent.frame())
}

me <- function(model, term) {
    d <- deriv(parseformula(model), term)
    e <- eval(d, env = parent.frame())
    varcov <- vcov(model)
    list(e = e, d = d, v = varcov)
}

m1.1a <- lm(f1.1a)
me1.1a <- coef(m1.1a)[1] + (z * coef(m1.1a)[3])
vme1.1a <- vcov(m1.1a)[1,1] + (z^2 * vcov(m1.1a)[3,3]) + (2 * z * vcov(m1.1a)[1,3])
me1.1a; vme1.1a

me(lm(f1.1a), "x")
me(lm(f1.1a), "z")


