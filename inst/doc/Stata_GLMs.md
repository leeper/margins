<!--
%\VignetteEngine{knitr}
%\VignetteIndexEntry{Stata Comparison: Generalized Linear Models}
-->

# Generalized Linear Models #

**margins** is intended as a port of (some of) the features of Stata's `margins` command. This vignette compares output from Stata's `margins` command for generalized linear models against the output of **margins**.


```r
library("margins")
options(width = 100)
```

## GLM (Logit) Effects on Probability Scale ##

### Stata Logit Effects on Probability and Log-Odds Scales ###

```
. quietly logit am cyl hp wt
. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Pr(am), predict()
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |   .0214527   .0469746     0.46   0.648    -.0706157    .1135212
          hp |   .0014339   .0006182     2.32   0.020     .0002224    .0026455
          wt |  -.4025475   .1154098    -3.49   0.000    -.6287466   -.1763484
------------------------------------------------------------------------------

. quietly logit am cyl hp wt
. margins, dydx(*) predict(xb)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Linear prediction (log odds), predict(xb)
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |   .4875978   1.071621     0.46   0.649    -1.612741    2.587936
          hp |   .0325917   .0188611     1.73   0.084    -.0043753    .0695587
          wt |   -9.14947   4.153326    -2.20   0.028    -17.28984   -1.009101
------------------------------------------------------------------------------
```

### R ###


```r
x <- glm(am ~ cyl + hp + wt, data = mtcars, family = binomial)
# AME
summary(margins(x, type = "response"))
```

```
## Average Marginal Effects
## glm(formula = am ~ cyl + hp + wt, family = binomial, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl  0.0215   0.0469  0.4572   0.6475 -0.0705  0.1134
##      hp  0.0014   0.0006  2.3041   0.0212  0.0002  0.0027
##      wt -0.4025   0.1138 -3.5386   0.0004 -0.6255 -0.1796
```

```r
# AME and MEM equivalent on "link" scale
summary(margins(x, type = "link"))
```

```
## Average Marginal Effects
## glm(formula = am ~ cyl + hp + wt, family = binomial, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)    2.50%  97.50%
##     cyl  0.4876   1.0716  0.4550   0.6491  -1.6127  2.5879
##      hp  0.0326   0.0189  1.7280   0.0840  -0.0044  0.0696
##      wt -9.1495   4.1533 -2.2029   0.0276 -17.2898 -1.0091
```


---

## GLM (Logit) Effects with factor variable on Log-Odds and probability scales ##


### Stata ###

```
. quietly logit am i.cyl hp wt
. margins, dydx(*) predict(xb)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Linear prediction (log odds), predict(xb)
dy/dx w.r.t. : 6.cyl 8.cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |
          6  |   2.765754   3.156829     0.88   0.381    -3.421517    8.953025
          8  |  -8.388958   13.16745    -0.64   0.524     -34.1967    17.41878
             |
          hp |    .103209   .0960655     1.07   0.283    -.0850759    .2914939
          wt |  -10.67598   5.441998    -1.96   0.050     -21.3421   -.0098575
------------------------------------------------------------------------------
Note: dy/dx for factor levels is the discrete change from the base level.

. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Pr(am), predict()
dy/dx w.r.t. : 6.cyl 8.cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |
          6  |   .1197978   .1062873     1.13   0.260    -.0885214    .3281171
          8  |  -.3478575   .2067542    -1.68   0.092    -.7530883    .0573732
             |
          hp |   .0033268   .0029852     1.11   0.265    -.0025241    .0091777
          wt |  -.3441297   .1188604    -2.90   0.004    -.5770919   -.1111675
------------------------------------------------------------------------------
Note: dy/dx for factor levels is the discrete change from the base level.
```

### R ###


```r
x <- glm(am ~ factor(cyl) + hp + wt, data = mtcars, family = binomial)
# Log-odds
summary(margins(x, type = "link"))
```

```
## Average Marginal Effects
## glm(formula = am ~ factor(cyl) + hp + wt, family = binomial,      data = mtcars) 
## 
##        Factor    dy/dx Std.Err. z value Pr(>|z|)    2.50%  97.50%
##            hp   0.1032   0.0961  1.0744   0.2826  -0.0851  0.2915
##            wt -10.6760   5.4418 -1.9618   0.0498 -21.3418 -0.0102
##  factor(cyl)6   2.7658   3.1568  0.8761   0.3810  -3.4214  8.9529
##  factor(cyl)8  -8.3890  13.1671 -0.6371   0.5240 -34.1960 17.4181
```

```r
# Probability with continuous factors
summary(margins(x, type = "response"))
```

```
## Average Marginal Effects
## glm(formula = am ~ factor(cyl) + hp + wt, family = binomial,      data = mtcars) 
## 
##        Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##            hp  0.0033   0.0030  1.0913   0.2751 -0.0026  0.0093
##            wt -0.3442   0.1221 -2.8193   0.0048 -0.5834 -0.1049
##  factor(cyl)6  0.1198   0.1067  1.1233   0.2613 -0.0892  0.3288
##  factor(cyl)8 -0.3479   0.2064 -1.6850   0.0920 -0.7525  0.0568
```


---

## GLM (Logit) with interaction on probability and Log-Odds scales ##

### Stata ###

```
. quietly logit am cyl c.hp##c.wt
. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Pr(am), predict()
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |   .0215633   .0492676     0.44   0.662    -.0749994    .1181261
          hp |   .0026673   .0023004     1.16   0.246    -.0018414     .007176
          wt |  -.5157922   .2685806    -1.92   0.055    -1.042201    .0106162
------------------------------------------------------------------------------

. margins, dydx(*) predict(xb)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Linear prediction (log odds), predict(xb)
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |   .5156396   1.169458     0.44   0.659    -1.776456    2.807735
          hp |   .0515116    .035699     1.44   0.149    -.0184571    .1214804
          wt |  -12.24264   7.678428    -1.59   0.111    -27.29208    2.806807
------------------------------------------------------------------------------
```

### R ###


```r
x <- glm(am ~ cyl + hp * wt, data = mtcars, family = binomial)
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
# AME
summary(margins(x, type = "response"))
```

```
## Average Marginal Effects
## glm(formula = am ~ cyl + hp * wt, family = binomial, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl  0.0216   0.0486  0.4434   0.6575 -0.0738  0.1169
##      hp  0.0027   0.0021  1.2414   0.2145 -0.0015  0.0069
##      wt -0.5158   0.2094 -2.4629   0.0138 -0.9263 -0.1053
```

```r
# AME and MEM equivalent on "link" scale
summary(margins(x, type = "link"))
```

```
## Average Marginal Effects
## glm(formula = am ~ cyl + hp * wt, family = binomial, data = mtcars) 
## 
##  Factor    dy/dx Std.Err. z value Pr(>|z|)    2.50% 97.50%
##     cyl   0.5156   1.1695  0.4409   0.6593  -1.7764 2.8077
##      hp   0.0515   0.0357  1.4430   0.1490  -0.0185 0.1215
##      wt -12.2426   7.6784 -1.5944   0.1108 -27.2920 2.8067
```


## GLM (Probit) Effects on Log-Odds and probability scales ##

### Stata ###

```
. quietly probit am cyl c.hp##c.wt
. margins, dydx(*) predict(xb)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Linear prediction, predict(xb)
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |   .2974758   .6629205     0.45   0.654    -1.001825    1.596776
          hp |   .0277713   .0193121     1.44   0.150    -.0100797    .0656223
          wt |  -6.626949   4.096208    -1.62   0.106    -14.65537    1.401471
------------------------------------------------------------------------------

. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Pr(am), predict()
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |    .022611   .0498253     0.45   0.650    -.0750447    .1202667
          hp |   .0025769   .0022607     1.14   0.254     -.001854    .0070077
          wt |   -.508829   .2625404    -1.94   0.053    -1.023399    .0057408
------------------------------------------------------------------------------
```

### R ###


```r
x <- glm(am ~ cyl + hp * wt, data = mtcars, family = binomial(link="probit"))
```

```
## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred
```

```r
# AME (log-odds)
summary(margins(x, type = "link"))
```

```
## Average Marginal Effects
## glm(formula = am ~ cyl + hp * wt, family = binomial(link = "probit"),      data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)    2.50% 97.50%
##     cyl  0.2975   0.6472  0.4596   0.6458  -0.9710 1.5660
##      hp  0.0278   0.0184  1.5075   0.1317  -0.0083 0.0639
##      wt -6.6269   3.9095 -1.6951   0.0901 -14.2894 1.0355
```

```r
# AME (probability)
summary(margins(x, type = "response"))
```

```
## Average Marginal Effects
## glm(formula = am ~ cyl + hp * wt, family = binomial(link = "probit"),      data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50%  97.50%
##     cyl  0.0226   0.0490  0.4615   0.6445 -0.0734  0.1186
##      hp  0.0026   0.0020  1.2722   0.2033 -0.0014  0.0065
##      wt -0.5088   0.2319 -2.1946   0.0282 -0.9633 -0.0544
```


## GLM (Poisson) Effects on Log-Odds and probability scales ##

### Stata ###

```
. quietly poisson carb cyl c.hp##c.wt
. margins, dydx(*) predict(xb)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Linear prediction, predict(xb)
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |  -.0993854   .1478936    -0.67   0.502    -.3892516    .1904808
          hp |   .0066519   .0024217     2.75   0.006     .0019054    .0113984
          wt |   .1225051   .2035185     0.60   0.547    -.2763837     .521394
------------------------------------------------------------------------------

. margins, dydx(*)

Average marginal effects                          Number of obs   =         32
Model VCE    : OIM
Expression   : Predicted number of events, predict()
dy/dx w.r.t. : cyl hp wt
------------------------------------------------------------------------------
             |            Delta-method
             |      dy/dx   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
         cyl |  -.2795214   .4169931    -0.67   0.503    -1.096813      .53777
          hp |   .0175935   .0067179     2.62   0.009     .0044267    .0307604
          wt |   .2075447   .4859868     0.43   0.669    -.7449719    1.160061
------------------------------------------------------------------------------
```


### R ###


```r
x <- glm(carb ~ cyl + hp * wt, data = mtcars, family = poisson)
# AME (linear/link)
summary(margins(x, type = "link"))
```

```
## Average Marginal Effects
## glm(formula = carb ~ cyl + hp * wt, family = poisson, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50% 97.50%
##     cyl -0.0994   0.1479 -0.6720   0.5016 -0.3893 0.1905
##      hp  0.0067   0.0024  2.7468   0.0060  0.0019 0.0114
##      wt  0.1225   0.2035  0.6019   0.5472 -0.2764 0.5214
```

```r
# AME (probability)
summary(margins(x, type = "response"))
```

```
## Average Marginal Effects
## glm(formula = carb ~ cyl + hp * wt, family = poisson, data = mtcars) 
## 
##  Factor   dy/dx Std.Err. z value Pr(>|z|)   2.50% 97.50%
##     cyl -0.2795   0.4110 -0.6801   0.4965 -1.0851 0.5261
##      hp  0.0176   0.0070  2.5043   0.0123  0.0038 0.0314
##      wt  0.2075   0.4705  0.4411   0.6591 -0.7146 1.1297
```
