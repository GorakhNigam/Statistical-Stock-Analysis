
``` r
library(quantmod)
library(ggplot2)
```

## Problem 1

Plot the two daily-closing-price series on separate sets of axes.
Describe informally the basic characteristics of the plots. Do the
series look stationary? Does the volatility seem to fluctuate over time?

``` r
getSymbols("QCOM",src="yahoo",from=as.Date("2020-01-01"),to=as.Date("2025-02-07"))
```

    ## [1] "QCOM"

``` r
head(QCOM)
```

    ##            QCOM.Open QCOM.High QCOM.Low QCOM.Close QCOM.Volume QCOM.Adjusted
    ## 2020-01-02     89.05     89.81    88.08      88.69     8413900      78.48726
    ## 2020-01-03     87.26     87.64    86.44      87.02     8340300      77.00936
    ## 2020-01-06     85.91     86.55    85.54      86.51     8381400      76.55804
    ## 2020-01-07     87.04     89.49    86.91      88.97     8377400      78.73505
    ## 2020-01-08     88.90     89.47    87.92      88.71     7619900      78.50494
    ## 2020-01-09     89.66     90.72    88.83      89.91     9155500      79.56691

``` r
ggplot(QCOM, aes(x = Index, y = QCOM.Adjusted)) + geom_line() + scale_x_date(date_breaks = "8 month",  date_labels = "%b %Y")
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

***Comment:***

The time series plot for QCOM is not stationary as overall there is
upward trend (with occasional downward trend) in the data. Also it has
high volatility which is also fluctuating over time (it is smooth in
some time interval and high in other time interval).

``` r
getSymbols("AVGO",src="yahoo",from=as.Date("2020-01-01"),to=as.Date("2025-02-07"))
```

    ## [1] "AVGO"

``` r
ggplot(AVGO, aes(x = Index, y = AVGO.Adjusted)) + geom_line() + scale_x_date(date_breaks = "8 month",  date_labels = "%b %Y")
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

***Comment:***

The time series plot for Broadcom is not stationary as there is clear
upward trend in the data. Also it has high volatility which is also
fluctuating over time (it is smooth in some time interval and high in
other time interval).

## Problem 2

Compute and plot the series of “log returns”. Describe informally the
basic characteristics of these plots as well.

``` r
QCOMlogreturn <- diff(log(QCOM[,6]))[-1]
```

``` r
ggplot(QCOMlogreturn, aes(x = Index, y = QCOM.Adjusted)) + geom_line() + scale_x_date(date_breaks = "8 month",  date_labels = "%b %Y")
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-24-1.png)<!-- -->

***Comment:***

The above plot oscillate around zero (stationary mean also more
stationary than stock prices) and high volatility of log returns of QCOM
stock (volatility around some timelines e.g. 2020, 2022 are very high as
compare to other timelines).

``` r
AVGOlogreturn <- diff(log(AVGO[,6]))[-1]
```

``` r
ggplot(AVGOlogreturn, aes(x = Index, y = AVGO.Adjusted)) + geom_line() + scale_x_date(date_breaks = "8 month",  date_labels = "%b %Y")
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

***Comment:***

The above plot oscillate around zero (stationary mean, more stationary
than stock prices) and high volatility of log returns of AVGO stocks
(volatility around some timelines e.g. 2020, 2025 are very high as
compare to other timelines).

## Problem 3

Produce normal probability plots (quantile-quantile plots) for each of
the two log-return series. Analyze the plots and draw conclusions.

``` r
qqnorm(sort(QCOMlogreturn), main = "Q-Q Plot for Log-Returns of QCOM stock")
qqline(sort(QCOMlogreturn), col = "red", lwd = 2)
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-27-1.png)<!-- -->

***Comment:***

As we can see from the graph the points deviate from the red line at
both ends and aligned in the middle portion (making it S shaped), this
indicates heavier tails as compared to a normal distribution. This
violates the normality assumption.

``` r
qqnorm(sort(AVGOlogreturn), main = "Q-Q Plot for Log-Returns of AVGO stock")
qqline(sort(AVGOlogreturn), col = "red", lwd = 2)
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

***Comment:***

As we can see from the graph the points deviate from the red line at
both ends and aligned in the middle portion (making it S shaped), this
indicates heavier tails as compared to a normal distribution. This
violates the normality assumption.

## Problem 4

Produce Student-t plots (quantile-quantile plots) for the two log-return
series with k=2, 6, 10, and 20 degrees of freedom. Analyze the plots and
draw conclusions. Then experiment with other positive integer values k,
deciding which k seems to yield the best fit for the data, and
displaying the corresponding best quantile-quantile-plot.

``` r
par(mfrow=c(2,2))
dfs <- c(2, 6, 10, 20)
for (df in dfs) {
  t_quantiles <- qt(ppoints(length(QCOMlogreturn)), df=df)
  qqplot(t_quantiles, sort(as.vector(QCOMlogreturn$QCOM.Adjusted)), 
         main=paste("QCOM QQ Plot - t-dist df =", df), 
         xlab="Theoretical Quantiles", ylab="Sample Quantiles")
  qqline(sort(as.vector(QCOMlogreturn$QCOM.Adjusted)), distribution = function(p) qt(p, df = df), col = "red")
}
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-29-1.png)<!-- -->

***Comment:***

The graphs shows the convergence of log return of QCOM stock price to a
t-distribution as we vary the degree of freedom. As shown, the first
graph with DF = 2, is far from t-distribution as both the ends of plot
is deviating a lot from the line. Distribution is aligning as we
increase DF and then again deviating slightly. In the above 4 graphs, DF
= 6 has better fit as compared to other DFs.

``` r
par(mfrow=c(2,2))
dfs <- c(2, 6, 10, 20)
for (df in dfs) {
  t_quantiles <- qt(ppoints(length(AVGOlogreturn)), df=df)
  qqplot(t_quantiles, sort(as.vector(AVGOlogreturn$AVGO.Adjusted)), 
         main=paste("AVGO QQ Plot - t-dist df =", df), 
         xlab="Theoretical Quantiles", ylab="Sample Quantiles")
  qqline(sort(as.vector(AVGOlogreturn$AVGO.Adjusted)), distribution = function(p) qt(p, df = df), col = "red")
}
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-30-1.png)<!-- -->

***Comment:***

The graphs shows the convergence of log return of AVGO stock price to a
t-distribution as we vary the degree of freedom. As shown, the first
graph with DF = 2, is far from t-distribution as both the ends of plot
is deviating a lot from the line. Distribution is aligning as we
increase DF and then again deviating slightly. In the above 4 graphs, DF
= 6 has better fit as compared to other DFs.

``` r
par(mfrow=c(1,2))
dfs <- c(1,2,3,4,5,6,7,8,9,10)
for (df in dfs) {
  t_quantiles <- qt(ppoints(length(QCOMlogreturn)), df=df)
  qqplot(t_quantiles, sort(as.vector(QCOMlogreturn$QCOM.Adjusted)), 
         main=paste("QCOM QQ Plot - t-dist df =", df), 
         xlab="Theoretical Quantiles", ylab="Sample Quantiles")
  qqline(sort(as.vector(QCOMlogreturn$QCOM.Adjusted)), distribution = function(p) qt(p, df = df), col = "red")
}
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-31-1.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-31-2.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-31-3.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-31-4.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-31-5.png)<!-- -->

***Comment:***

As we can see in the previous experiment with DF =(2, 6, 10,20) that
graph is converging around 6 and diverging again. So I decided to limit
my experiment till 10 DF but binning the DF more minutely (e.g. in the
bins of 1s). As we can see the graph with DF 4 is well fitted except for
the one point. So we can safely say that t-distribution with DF = 4 is
closely representing log return of QCOM stock data.

``` r
par(mfrow=c(1,2))
dfs <- c(1,2,3,4,5,6,7,8,9,10)
for (df in dfs) {
  t_quantiles <- qt(ppoints(length(AVGOlogreturn)), df=df)
  qqplot(t_quantiles, sort(as.vector(AVGOlogreturn$AVGO.Adjusted)), 
         main=paste("AVGO QQ Plot - t-dist df =", df), 
         xlab="Theoretical Quantiles", ylab="Sample Quantiles")
  qqline(sort(as.vector(AVGOlogreturn$AVGO.Adjusted)), distribution = function(p) qt(p, df = df), col = "red")
}
```

![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-32-1.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-32-2.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-32-3.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-32-4.png)<!-- -->![](Gorakhnath_Nigam_Assignment-2_files/figure-gfm/unnamed-chunk-32-5.png)<!-- -->

***Comment:***

As we can see the graph with DF 3 is well fitted as compared to other
graphs. So we can safely say that t-distribution with DF = 3 is closely
representing log return of AVGO stock data.

## Problem 5

Discuss the contrasts and similarities between the results for the
log-return series for the first security you chose and those for the
second log-return series you chose.

**Answer**:

**Similarities**:

- Both the stocks shows high volatility also there are clustering of
  high volatility in some regions.

- They also shows heavy-tailed log returns as shown by normal
  probability plots.

- Neither follows a normal distribution. As shown, t-distribution with a
  specific degree of freedom is a better fit.

**Differences**:

- As apparent in qqnorm plot of AVGO, the upper right corner is more
  upward from the line and more downward in lower corner from the line
  as compared to that of QCOM. We can say that AVGO has heavier tails
  (higher kurtosis) as compared to QCOM.

- AVGO shows slightly higher volatility overall (Volatility in AVGO is
  scaled from -0.2 to 0.2 and it is scaled from -0.1 to 0.1 for QCOM).

- AVGO has a stronger upward price trend compared to QCOM.
