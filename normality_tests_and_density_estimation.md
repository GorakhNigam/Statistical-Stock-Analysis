Gorakhnath_Nigam
================

``` r
library(quantmod)
library(ggplot2)
```

**Q1**. Again consider the two stock-price log return series {x$t$ : 1
\<= t \<= N} and {y$t$ : 1 \< t \< N} that you had analyzed in problem
set \#2. For each of these two series separately, create a kernel
density estimate and create two parametric density estimates: Student-T
(with your chosen parameter) and normal. To help estimate the scale
parameter for the generalized T distribution, make use of the median
absolute deviation (to be discussed in class). Superimpose the graphs of
the three density estimates, and compare the features of the three
graphs. Please label which graph is which. In preparing the graphs, you
can imitate the code on page 80 (and the bottom of page 79) in Ruppert
and Matteson (2nd edition). The fGarch library may be useful.

``` r
getSymbols("QCOM",src="yahoo",from=as.Date("2020-01-01"),to=as.Date("2025-02-07"))
```

    ## [1] "QCOM"

``` r
QCOMlogreturn <- diff(log(QCOM[,6]))[-1]
df = 4
```

``` r
library("fGarch")
```

    ## Warning: package 'fGarch' was built under R version 4.4.3

    ## NOTE: Packages 'fBasics', 'timeDate', and 'timeSeries' are no longer
    ## attached to the search() path when 'fGarch' is attached.
    ## 
    ## If needed attach them yourself in your R script by e.g.,
    ##         require("timeSeries")

    ## 
    ## Attaching package: 'fGarch'

    ## The following object is masked from 'package:TTR':
    ## 
    ##     volatility

``` r
x=seq(-0.1, 0.1,by = 0.001)
par(mfrow = c(1, 1))
mad_t = mad(QCOMlogreturn,constant = sqrt(df / (df - 2)) / qt(0.75, df))
plot(density(QCOMlogreturn), lwd = 2, ylim = c(0, 60))
lines(x, dstd(x, mean = mean(QCOMlogreturn), sd = mad_t, nu = df), lty = 5, lwd = 2, col = "red")
lines(x, dnorm(x, mean = mean(QCOMlogreturn), sd = sd(QCOMlogreturn)), lty = 3, lwd = 4, col = "blue")
legend("topleft", c("KDE", paste("t: df = ",df), "normal"),lwd = c(2, 2, 4), lty = c(1, 5, 3), col = c("black", "red", "blue"))
```

![](Gorakhnath_Nigam_Assignment-3_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

``` r
getSymbols("AVGO",src="yahoo",from=as.Date("2020-01-01"),to=as.Date("2025-02-07"))
```

    ## [1] "AVGO"

``` r
AVGOlogreturn <- diff(log(AVGO[,6]))[-1]
df = 3
```

``` r
library("fGarch")
x=seq(-0.1, 0.1,by = 0.001)
par(mfrow = c(1, 1))
mad_t = mad(AVGOlogreturn,constant = sqrt(df / (df - 2)) / qt(0.75, df))
plot(density(AVGOlogreturn), lwd = 2, ylim = c(0, 60))
lines(x, dstd(x, mean = mean(AVGOlogreturn), sd = mad_t, nu = df), lty = 5, lwd = 2, col = "red")
lines(x, dnorm(x, mean = mean(AVGOlogreturn), sd = sd(AVGOlogreturn)), lty = 3, lwd = 4, col = "blue")
legend("topleft", c("KDE", paste("t: df = ",df), "normal"),lwd = c(2, 2, 4), lty = c(1, 5, 3), col = c("black", "red", "blue"))
```

![](Gorakhnath_Nigam_Assignment-3_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Q2**. Use the glm (generalized linear model) instruction, with
family=binomial, to fit and test a logistic regression model for each of
your two log-return series (analogous to the model in section 4.7.2 of
the ISL2 text). Discuss your results carefully. As the response
variable, “direction”, you can use “1” for “up” and “0” for “down”
indicating whether the log return on a given day is positive or
negative. Use three predictor variables: “lag1” (log return on the
previous trading day), “lag2” (log return on the second prior trading
day), and “vol” (volume on the previous trading day).

``` r
QCOM_logRet <- diff(log(QCOM$QCOM.Adjusted))

QCOM_vol_lag <- lag(QCOM$QCOM.Volume, 1)

QCOM_data <- data.frame(
  date      = index(QCOM_logRet),
  logReturn = as.numeric(QCOM_logRet),
  vol       = as.numeric(QCOM_vol_lag[index(QCOM_logRet)])
)

QCOM_data$lag1 <- c(NA, QCOM_data$logReturn[-nrow(QCOM_data)])
QCOM_data$lag2 <- c(NA, NA, QCOM_data$logReturn[1:(nrow(QCOM_data)-2)])

QCOM_data$direction <- ifelse(QCOM_data$logReturn > 0, 1, 0)

QCOM_data <- na.omit(QCOM_data)


QCOM_train <- subset(QCOM_data, date < as.Date("2024-01-01"))
QCOM_test  <- subset(QCOM_data, date >= as.Date("2024-01-01"))


QCOM_model <- glm(direction ~ lag1 + lag2 + vol, data = QCOM_train, family = binomial)

summary(QCOM_model)
```

    ## 
    ## Call:
    ## glm(formula = direction ~ lag1 + lag2 + vol, family = binomial, 
    ##     data = QCOM_train)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)    
    ## (Intercept)  3.310e-02  1.477e-01   0.224   0.8227    
    ## lag1        -8.409e+00  2.508e+00  -3.353   0.0008 ***
    ## lag2         1.264e+00  2.445e+00   0.517   0.6053    
    ## vol          1.954e-09  1.470e-08   0.133   0.8942    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1389.9  on 1002  degrees of freedom
    ## Residual deviance: 1377.1  on  999  degrees of freedom
    ## AIC: 1385.1
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
QCOM_test$pred_prob <- predict(QCOM_model, newdata = QCOM_test, type = "response")

QCOM_test$pred_class <- ifelse(QCOM_test$pred_prob > 0.5, 1, 0)

QCOM_conf_matrix <- table(
  Predicted = QCOM_test$pred_class,
  Actual    = QCOM_test$direction
)
QCOM_conf_matrix
```

    ##          Actual
    ## Predicted  0  1
    ##         0 59 57
    ##         1 70 90

``` r
QCOM_accuracy <- sum(diag(QCOM_conf_matrix)) / sum(QCOM_conf_matrix)
QCOM_accuracy
```

    ## [1] 0.5398551

``` r
AVGO_logRet <- diff(log(AVGO$AVGO.Adjusted))
AVGO_vol_lag <- lag(AVGO$AVGO.Volume, 1)

AVGO_data <- data.frame(
  date      = index(AVGO_logRet),
  logReturn = as.numeric(AVGO_logRet),
  vol       = as.numeric(AVGO_vol_lag[index(AVGO_logRet)])
)

AVGO_data$lag1 <- c(NA, AVGO_data$logReturn[-nrow(AVGO_data)])
AVGO_data$lag2 <- c(NA, NA, AVGO_data$logReturn[1:(nrow(AVGO_data)-2)])

AVGO_data$direction <- ifelse(AVGO_data$logReturn > 0, 1, 0)

AVGO_data <- na.omit(AVGO_data)

AVGO_train <- subset(AVGO_data, date < as.Date("2024-01-01"))
AVGO_test  <- subset(AVGO_data, date >= as.Date("2024-01-01"))

AVGO_model <- glm(direction ~ lag1 + lag2 + vol, data = AVGO_train, family = binomial)

summary(AVGO_model)
```

    ## 
    ## Call:
    ## glm(formula = direction ~ lag1 + lag2 + vol, family = binomial, 
    ##     data = AVGO_train)
    ## 
    ## Coefficients:
    ##               Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)  1.021e-01  1.308e-01   0.781  0.43508   
    ## lag1        -7.773e+00  2.826e+00  -2.751  0.00595 **
    ## lag2         1.569e+00  2.708e+00   0.579  0.56230   
    ## vol          1.456e-09  4.931e-09   0.295  0.76786   
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## (Dispersion parameter for binomial family taken to be 1)
    ## 
    ##     Null deviance: 1386.5  on 1002  degrees of freedom
    ## Residual deviance: 1377.3  on  999  degrees of freedom
    ## AIC: 1385.3
    ## 
    ## Number of Fisher Scoring iterations: 4

``` r
AVGO_test$pred_prob <- predict(AVGO_model, newdata = AVGO_test, type = "response")
AVGO_test$pred_class <- ifelse(AVGO_test$pred_prob > 0.5, 1, 0)

AVGO_conf_matrix <- table(
  Predicted = AVGO_test$pred_class,
  Actual    = AVGO_test$direction
)
AVGO_conf_matrix
```

    ##          Actual
    ## Predicted   0   1
    ##         0  35  36
    ##         1  94 111

``` r
AVGO_accuracy <- sum(diag(AVGO_conf_matrix)) / sum(AVGO_conf_matrix)
AVGO_accuracy
```

    ## [1] 0.5289855

As the p-values for lag1 variable is relatively lower for both the
stocks i.e. QCOM and AVGO, significantly less than 5% for lag1 so we can
conclude that lag1 is a significant variable to predict movement of
stock in this case. Also the coefficient for lag1 for both the stocks
are negative showing that they are inversely related to the direction of
movement of stocks. Other variables are far from being significant
variables for the movement of both the stocks.

**Q3**, Use the MASS library and the instruction lda to fit and test a
linear-discriminant-analysis model for the log-return data (see section
4.7.3 of the ISL2 text). Discuss your results carefully.

``` r
library(MASS)
QCOM_model_lda <- lda(direction ~ lag1 + lag2 + vol, data = QCOM_train)
print(QCOM_model_lda)
```

    ## Call:
    ## lda(direction ~ lag1 + lag2 + vol, data = QCOM_train)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.4885344 0.5114656 
    ## 
    ## Group means:
    ##           lag1          lag2     vol
    ## 0  0.003622109 -0.0003240483 9151260
    ## 1 -0.002274870  0.0014554382 9144836
    ## 
    ## Coefficients of linear discriminants:
    ##                LD1
    ## lag1 -3.659115e+01
    ## lag2  5.420880e+00
    ## vol   8.029208e-09

``` r
lda_pred <- predict(QCOM_model_lda, newdata = QCOM_test)
predicted_class <- lda_pred$class

conf_matrix <- table(Predicted = predicted_class, Actual = QCOM_test$direction)
print(conf_matrix)
```

    ##          Actual
    ## Predicted  0  1
    ##         0 58 57
    ##         1 71 90

``` r
print(mean(predicted_class == QCOM_test$direction))
```

    ## [1] 0.5362319

The model suggests that lag1 and lag2 are significant predictors of the
stock movement. It is less affected by volume, which has a close to zero
coefficient. Also the prior probabilities shows a slight imbalance in
the dataset, more days having positive log returns.For 53.62% of the
times LDA gives correct prediction of the movement of the direction of
the market on the test dataset which is very much similar to the
accuracy that we got from logistic regression.

``` r
AVGO_model_lda <- lda(direction ~ lag1 + lag2 + vol, data = AVGO_train)
print(AVGO_model_lda)
```

    ## Call:
    ## lda(direction ~ lag1 + lag2 + vol, data = AVGO_train)
    ## 
    ## Prior probabilities of groups:
    ##         0         1 
    ## 0.4685942 0.5314058 
    ## 
    ## Group means:
    ##           lag1         lag2      vol
    ## 0  0.003791037 0.0004514509 23362517
    ## 1 -0.000713818 0.0021889694 23468692
    ## 
    ## Coefficients of linear discriminants:
    ##                LD1
    ## lag1 -3.905881e+01
    ## lag2  7.929940e+00
    ## vol   5.956667e-09

``` r
lda_pred <- predict(AVGO_model_lda, newdata = AVGO_test)
predicted_class <- lda_pred$class

conf_matrix <- table(Predicted = predicted_class, Actual = AVGO_test$direction)
print(conf_matrix)
```

    ##          Actual
    ## Predicted   0   1
    ##         0  34  35
    ##         1  95 112

``` r
print(mean(predicted_class == AVGO_test$direction))
```

    ## [1] 0.5289855

The model suggests that lag1 and lag2 are significant predictors of the
stock movement. It is less affected by volume, which has a close to zero
coefficient. Also the prior probabilities shows a slight imbalance in
the dataset, more days having positive log returns. For 52.3% of the
times LDA gives correct prediction of the movement of the direction of
the market on the test dataset which is very much similar to the
accuracy that we got from logistic regression.

**Q4**. Consider the joint distribution for the pairs {(xt; yt) : 1 \< t
\< N} of values in your sets of log-return data. Create a scatterplot
for these N pairs and then discuss the features of this scatterplot.

``` r
plot(QCOM_data$logReturn, AVGO_data$logReturn, 
     xlab = "QCOM Log Return", 
     ylab = "AVGO Log Return", 
     main = "Scatterplot of QCOM vs AVGO Log Returns",
     pch = 19, col = rgb(0, 0, 0, 0.5))
abline(h = 0, v = 0, col = "gray", lty = 2)
```

![](Gorakhnath_Nigam_Assignment-3_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

As shown in the scatter plot, the log return values for AVGO and QCOM
are positively correlated which is also expected given that both the
stocks belong to the same industry (Semiconductors). Also the values are
clustered near zero showing low return day over day.

The graph is elliptical which hints at a possibility of bivariate normal
distribution(with heavy tails because of some outlier shown in the
graph)
