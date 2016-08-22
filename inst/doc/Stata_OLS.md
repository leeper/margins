<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{Stata Comparison: Linear Models}
-->

# Linear Models #

**margins** is intended as a port of (some of) the features of Stata's `margins` command. This vignette compares output from Stata's `margins` command for linear models against the output of **margins**.


```r
library("margins")
options(width = 100)
```

---

## OLS marginal effects ##

### Stata ###

```
. quietly reg mpg cyl hp wt
. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OLS
Expression   : Linear prediction, predict()
dy/dx w.r.t. : cyl hp wt

------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |  -.9416166   .5509165    -1.71   0.098    -2.070118    .1868846
          hp |  -.0180381   .0118763    -1.52   0.140    -.0423655    .0062893
          wt |  -3.166973    .740576    -4.28   0.000    -4.683975   -1.649972
------------------------------------------------------------------------------
```

### R ###


```r
library("margins")
x <- lm(mpg ~ cyl + hp + wt, data = mtcars)
summary(margins(x))
```

```
## Average Marginal Effects
## lm(formula = mpg ~ cyl + hp + wt, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl -0.9416   0.5509 -1.7092   0.0874 -2.0214  0.1382
##      hp -0.0180   0.0119 -1.5188   0.1288 -0.0413  0.0052
##      wt -3.1670   0.7406 -4.2764   0.0000 -4.6185 -1.7155
```

---


## OLS marginal effects with interaction ##

### Stata ###

```
. quietly reg mpg cyl c.hp##c.wt
. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OLS
Expression   : Linear prediction, predict()
dy/dx w.r.t. : cyl hp wt

------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |  -.3652391   .5086204    -0.72   0.479    -1.408842    .6783638
          hp |  -.0252715   .0105097    -2.40   0.023    -.0468357   -.0037073
          wt |  -3.837584   .6730996    -5.70   0.000     -5.21867   -2.456498
------------------------------------------------------------------------------
```

### R ###


```r
x <- lm(mpg ~ cyl + hp * wt, data = mtcars)
summary(margins(x))
```

```
## Average Marginal Effects
## lm(formula = mpg ~ cyl + hp * wt, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl -0.3652   0.5086 -0.7181   0.4727 -1.3621  0.6316
##      hp -0.0253   0.0105 -2.4046   0.0162 -0.0459 -0.0047
##      wt -3.8376   0.6731 -5.7014   0.0000 -5.1568 -2.5183
```

---

## OLS marginal effects with factor term ##

### Stata ###

```
. quietly reg mpg i.cyl hp wt
. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OLS
Expression   : Linear prediction, predict()
dy/dx w.r.t. : 6.cyl 8.cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |
          6  |  -3.359024    1.40167    -2.40   0.024    -6.235014   -.4830353
          8  |  -3.185884   2.170476    -1.47   0.154    -7.639332    1.267564
             |
          hp |  -.0231198   .0119522    -1.93   0.064    -.0476437    .0014041
          wt |  -3.181404   .7196011    -4.42   0.000    -4.657904   -1.704905
------------------------------------------------------------------------------
Note: dy/dx for factor levels is the discrete change from the base level.
```

### R ###


```r
x <- lm(mpg ~ factor(cyl) + hp + wt, data = mtcars)
summary(margins(x))
```

```
## Average Marginal Effects
## lm(formula = mpg ~ factor(cyl) + hp + wt, data = mtcars) 
## 
##        Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##            hp -0.0231   0.0120 -1.9344   0.0531 -0.0465  0.0003
##            wt -3.1814   0.7196 -4.4211   0.0000 -4.5918 -1.7710
##  factor(cyl)6 -3.3590   1.4017 -2.3964   0.0166 -6.1062 -0.6118
##  factor(cyl)8 -3.1859   2.1705 -1.4678   0.1422 -7.4399  1.0682
```


---

## OLS marginal effects with squared term ##

### Stata ###

```
. quietly reg mpg cyl c.hp##c.hp wt
. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OLS
Expression   : Linear prediction, predict()
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |  -.3696041   .6163571    -0.60   0.554    -1.634264    .8950561
          hp |  -.0429018   .0178353    -2.41   0.023    -.0794969   -.0063066
          wt |  -2.873553   .7301251    -3.94   0.001    -4.371646    -1.37546
------------------------------------------------------------------------------

```

### R ###


```r
x <- lm(mpg ~ cyl + hp + I(hp^2) + wt, data = mtcars)
summary(margins(x))
```

```
## Average Marginal Effects
## lm(formula = mpg ~ cyl + hp + I(hp^2) + wt, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl -0.3696   0.6164 -0.5997   0.5487 -1.5776  0.8384
##      hp -0.0429   0.0178 -2.4054   0.0162 -0.0779 -0.0079
##      wt -2.8736   0.7301 -3.9357   0.0001 -4.3046 -1.4425
```

---

## OLS marginal effects with squared term (but no first-order term) ##

### Stata ###

```
. gen hp2 = hp^2
. quietly reg mpg cyl hp2 wt
. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OLS
Expression   : Linear prediction, predict()
dy/dx w.r.t. : cyl hp2 wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      t    P>|t|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |   -1.21919   .5030753    -2.42   0.022    -2.249693   -.1886869
         hp2 |   -.000028   .0000276    -1.01   0.320    -.0000846    .0000286
          wt |  -3.218637   .7570747    -4.25   0.000    -4.769435    -1.66784
------------------------------------------------------------------------------
```

### R ###


```r
x <- lm(mpg ~ cyl + I(hp^2) + wt, data = mtcars)
summary(margins(x))
```

```
## Average Marginal Effects
## lm(formula = mpg ~ cyl + I(hp^2) + wt, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl -1.2192   0.5031 -2.4235   0.0154 -2.2052 -0.2332
##      hp -0.0082   0.0081 -1.0124   0.3114 -0.0241  0.0077
##      wt -3.2186   0.7571 -4.2514   0.0000 -4.7025 -1.7348
```

