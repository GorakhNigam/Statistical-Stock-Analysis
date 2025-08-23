Gorakhnath Nigam Assignment Solution 4A and 4B combined
================

This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you
execute code within the notebook, the results appear beneath the code.

Try executing this chunk by clicking the *Run* button within the chunk
or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*.

``` r
library(quantmod)
library(ggplot2)
library(MASS)
library(mnormt)
library(copula)
```

    ## Warning: package 'copula' was built under R version 4.4.3

``` r
library(fGarch)
```

``` r
getSymbols("QCOM",src="yahoo",from=as.Date("2020-01-01"),to=as.Date("2025-02-07"))
```

    ## [1] "QCOM"

``` r
QCOMlogreturn <- as.numeric(diff(log(QCOM[,6]))[-1])

getSymbols("AVGO",src="yahoo",from=as.Date("2020-01-01"),to=as.Date("2025-02-07"))
```

    ## [1] "AVGO"

``` r
AVGOlogreturn <- as.numeric(diff(log(AVGO[,6]))[-1])
```

**Q0**. Begin by repeating, for reference, your results for problem 4 on
Set \#3, where you produced a scatterplot for the pairs of values in
your set of log-return data.

``` r
plot(QCOMlogreturn, AVGOlogreturn, 
     xlab = "QCOM Log Return", 
     ylab = "AVGO Log Return", 
     main = "Scatterplot of QCOM vs AVGO Log Returns",
pch = 19, col = rgb(0, 0, 0, 0.5))
abline(h = 0, v = 0, col = "gray", lty = 2)
```

![](Gorakhnath_Nigam_Assignment-4B_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

**Q1** For the pair of series above, use the maximum likelihood method
to fit a bivariate t distribution. Your estimation procedure will
involve two parts. The first part is to use R to find the maximum
likelihood estimate for the tail-index parameter ν. The second part is,
for a given value of ν, to estimate the mean vector ~µ and the
correlation matrix Λ.

``` r
dat = cbind(QCOMlogreturn, AVGOlogreturn)
```

``` r
df = seq(0.1, 10, 0.1)
n = length(df)
loglik = rep(0,n)
for(i in 1:n){
  fit = cov.trob(dat,nu=df[i],cor=TRUE)
  loglik[i] = sum(log(dmt(dat, mean=fit$center, S = fit$cov, df = df[i])))
}
best_df <- df[which.max(loglik)]
```

``` r
plot(df, loglik, type = "l", xlab = "Degrees of Freedom", 
     ylab = "Log-Likelihood", main = "Profile Likelihood for df")
abline(v = best_df, col = "red", lty = 2)
```

![](Gorakhnath_Nigam_Assignment-4B_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
bestfit <- cov.trob(dat, nu = best_df, cor = TRUE)

mu_estimate <- bestfit$center 
cor_estimate <- bestfit$cor

cat("Estimated degrees of freedom:", best_df, "\n")
```

    ## Estimated degrees of freedom: 3.4

``` r
cat("Estimated Mean vector (mu):\n", mu_estimate, "\n")
```

    ## Estimated Mean vector (mu):
    ##  0.0006572009 0.001321403

``` r
cat("Estimated Correlation matrix (Lambda):\n")
```

    ## Estimated Correlation matrix (Lambda):

``` r
cor_estimate
```

    ##               QCOMlogreturn AVGOlogreturn
    ## QCOMlogreturn     1.0000000     0.7290204
    ## AVGOlogreturn     0.7290204     1.0000000

Also, determine a 95% confidence interval for . One way to build this
interval is to imitate pp. 168-169 of RM.As within the block of code on
page 169, define the vector z1. If you insert an extra line of code
having z1 by itself, you will produce a YES/NO sequence indicating which
of the possible values of df have log-likelihood lying within 2 standard
deviations of the maximum. From this YES/NO sequence, you will be able
to tell where to draw the two vertical lines which specify the
boundaries of the 95% confidence interval.

``` r
threshold <- 2*max(loglik) - qchisq(0.95, df = 1)

z1 <- 2*loglik >= 2 * max(loglik) - qchisq(0.95, 1) #2 * loglik > 2 * max(loglik) - qchisq(0.95, 1)
ci_indices <- which(z1)
ci_df <- range(df[ci_indices])

plot(df, 2*loglik, type = "l", xlab = "Degrees of Freedom", ylab = "Log-Likelihood", main = "Profile Likelihood for df")
abline(v = best_df, col = "red", lty = 3, lwd=3)
abline(h = threshold, col = "blue", lty = 3, lwd = 3)
abline(h = 2 * max(loglik), col = "orange", lty = 3, lwd = 3)
abline(v = ci_df, col = "green", lty = 3, lwd = 2)

legend("topright", 
       legend = c("Log-Likelihood", "95% CI Threshold", "Max Likelihood", 
                 "Best df", "95% CI Bounds"),
       col = c("black", "blue", "orange", "red", "green"),
       lty = c(1, 2, 2, 3, 3), 
       lwd = c(2, 2, 2, 1, 1))
```

![](Gorakhnath_Nigam_Assignment-4B_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->
**Note: ** In the above chart we can’t see the threshold line due to
scaling of y-axis. Below is the chart with adjusted scaling to bring
both the horizontal line inside the visible graph

``` r
threshold <- max(loglik) - 0.5 * qchisq(0.95, 1) 


plot(df, loglik, type = "l", lwd = 2,
     xlab = "Degrees of Freedom", 
     ylab = "Log-Likelihood",
     main = "Profile Likelihood for df",
     ylim = c(max(loglik) - 10, max(loglik) + 1))


abline(h = threshold, col = "blue", lty = 2, lwd = 3)
abline(h = max(loglik), col = "orange", lty = 2, lwd = 2)
abline(v = best_df, col = "red", lty = 3) 
abline(v = ci_df, col = "green", lty = 3)

legend("topright", 
       legend = c("Log-Likelihood", "95% CI Threshold", "Max Likelihood", 
                 "Best df", "95% CI Bounds"),
       col = c("black", "blue", "orange", "red", "green"),
       lty = c(1, 2, 2, 3, 3), 
       lwd = c(2, 2, 2, 1, 1))
```

![](Gorakhnath_Nigam_Assignment-4B_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Q2** Use the fitdistr command (“MASS” library) to fit univariate t
distributions to the two log-return series. Display the means, scale
parameters, and degrees of freedom. Also, convert estimated scale
parameters to estimated standard deviations (RM page 210).

``` r
QCOMlogreturn <- na.omit(QCOMlogreturn)  
AVGOlogreturn <- na.omit(AVGOlogreturn) 
suppressWarnings({
  est.QCOMlogreturn = as.numeric( fitdistr(QCOMlogreturn,"t")$estimate )
  est.AVGOlogreturn = as.numeric( fitdistr(AVGOlogreturn,"t")$estimate )
})

est.QCOMlogreturn[2] = est.QCOMlogreturn[2] * sqrt( est.QCOMlogreturn[3] / (est.QCOMlogreturn[3]-2) )
est.AVGOlogreturn[2] = est.AVGOlogreturn[2] * sqrt(est.AVGOlogreturn[3] / (est.AVGOlogreturn[3]-2) )
est.QCOMlogreturn
```

    ## [1] 0.0008477295 0.0263249894 4.0583330962

``` r
est.AVGOlogreturn
```

    ## [1] 0.001518574 0.026674277 3.384326332

**Q3** Use R to calculate the sample Kendall’s Tau rank correlation and
the sample Pearson correlation for the pair of log-return series.

``` r
cor_tau = cor(QCOMlogreturn, AVGOlogreturn, method = "kendall")
cor = cor(QCOMlogreturn, AVGOlogreturn, method = "pearson")
cor_tau
```

    ## [1] 0.5281177

``` r
cor
```

    ## [1] 0.6797757

**Q4** Proceed to initialize the calculation for a t-copula (as in RM,
pp.210-211) by using the tCopula command (from the copula library) and
some reasonable starting value df. To fit the t-copula to the
uniform-transformed data, imitate what is done for the corresponding
data2 on page 211 (lines 17, 19). (Instead, you could imitate what is
done for data1 (lines 14, 15, and 18), where you would apply the data1
command (from the fGarch library) to transform the log-return data into
percentiles. But the non-parametric data2 approach, which deals only
with ranks of the data, is easier and almost as precise.) Use the
univariate t-distributions identified in \#2, along with the
maximum-likelihood method contained within the fitCopula command. Give
the estimated values for the correlation and for the tail-index
parameter, along with the standard errors of these estimates.

``` r
#Estimating pearson correlation from kenall's correlation
omega = sin(pi * cor_tau / 2)
omega
```

    ## [1] 0.7376379

``` r
cop_t_dim2 = tCopula(omega, dim = 2, dispstr = "un", df = 4)
data2 = cbind(rank(QCOMlogreturn)/(length(QCOMlogreturn)+1), rank(AVGOlogreturn)/(length(AVGOlogreturn)+1)) 
ft2 = fitCopula(cop_t_dim2, data2, method="ml", start=c(omega,4) )
ft2
```

    ## Call: fitCopula(cop_t_dim2, data2, method = "ml", start = c(omega, 
    ##     4))
    ## Fit based on "maximum likelihood" and 1281 2-dimensional observations.
    ## Copula: tCopula 
    ##  rho.1     df 
    ## 0.7341 4.2739 
    ## The maximized loglikelihood is 502.6 
    ## Optimization converged

``` r
estimated_cor <- ft2@estimate[1]
estimated_df <- ft2@estimate[2]
estimated_std_error <- sqrt(diag(ft2@var.est))
estimated_cor
```

    ## [1] 0.7340597

``` r
estimated_df
```

    ## [1] 4.273864

``` r
estimated_std_error
```

    ## [1] 0.01329477 0.65069980

``` r
ft2@var.est
```

    ##              [,1]        [,2]
    ## [1,] 0.0001767509 0.002366018
    ## [2,] 0.0023660176 0.423410227

**Q5** As in RM page 212, proceed to fit a normal (Gaussian) copula, a
Clayton copula, and a Joe Copula to your data using maximum likelihood.
(You may omit the Frank and Gumbel copulas.) For each of the three, give
the estimated values for the parameter and the standard error.

``` r
# Normal Copula
fnorm = fitCopula(copula=normalCopula(dim=2),data=data2,method="ml")
estimated_cor_norm <- fnorm@estimate
estimated_std_error_norm <- sqrt(diag(fnorm@var.est))
fnorm
```

    ## Call: fitCopula(normalCopula(dim = 2), data = data2, method = "ml")
    ## Fit based on "maximum likelihood" and 1281 2-dimensional observations.
    ## Copula: normalCopula 
    ##  rho.1 
    ## 0.7211 
    ## The maximized loglikelihood is 465.8 
    ## Optimization converged

``` r
estimated_cor_norm
```

    ## [1] 0.7211016

``` r
estimated_std_error_norm
```

    ## [1] 0.01089989

``` r
#Clayton copula
fclayton = fitCopula(copula = claytonCopula(1, dim=2), data = data2, method = "ml")
estimated_alpha_clayton <- fclayton@estimate
estimated_std_error_clayton <- sqrt(diag(fclayton@var.est))
fclayton
```

    ## Call: fitCopula(claytonCopula(1, dim = 2), data = data2, method = "ml")
    ## Fit based on "maximum likelihood" and 1281 2-dimensional observations.
    ## Copula: claytonCopula 
    ## alpha 
    ## 2.238 
    ## The maximized loglikelihood is 403.2 
    ## Optimization converged

``` r
estimated_alpha_clayton
```

    ## [1] 2.238345

``` r
estimated_std_error_clayton
```

    ## [1] 0.08243626

``` r
#Joe Copula
fjoe = fitCopula(copula=joeCopula(2,dim=2),data=data2,method="ml")
estimated_alpha_joe <- fjoe@estimate
estimated_std_error_joe <- sqrt(diag(fjoe@var.est))
fjoe
```

    ## Call: fitCopula(joeCopula(2, dim = 2), data = data2, method = "ml")
    ## Fit based on "maximum likelihood" and 1281 2-dimensional observations.
    ## Copula: joeCopula 
    ## alpha 
    ## 2.181 
    ## The maximized loglikelihood is 319.7 
    ## Optimization converged

``` r
estimated_alpha_joe
```

    ## [1] 2.180999

``` r
estimated_std_error_joe
```

    ## [1] 0.06236366

**Q6** Use contour to create diagrams of the four fitted copulas (t,
Normal, Clayton, and Joe). Thus each diagram will be that of a
cumulative distribution function for a particular bivariate uniform
distribution. Note: For the t-copula code, you may have to round the
estimated df to the nearest integer.

``` r
grid <- seq(0.01, 0.99, length.out = 50)
u <- expand.grid(grid, grid)

plot_copula_contour <- function(copula, title) {
  cdf <- pCopula(as.matrix(u), copula)
  contour(
    x = seq(0.01, 0.99, length.out = 50),
    y = seq(0.01, 0.99, length.out = 50),
    z = matrix(cdf, nrow = 50),
    main = title,
    xlab = "u1", 
    ylab = "u2",
    col = "darkblue",
    lwd = 2
  )
}

par(mfrow = c(2, 2))

plot_copula_contour(
  tCopula(ft2@estimate[1], df = round(ft2@estimate[2])), 
  "t-Copula CDF"
)

plot_copula_contour(
  normalCopula(fnorm@estimate), 
  "Gaussian Copula CDF"
)

plot_copula_contour(
  claytonCopula(fclayton@estimate), 
  "Clayton Copula CDF"
)

plot_copula_contour(
  joeCopula(fjoe@estimate), 
  "Joe Copula CDF"
)
```

![](Gorakhnath_Nigam_Assignment-4B_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or
by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output
will be saved alongside it (click the *Preview* button or press
*Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the
editor. Consequently, unlike *Knit*, *Preview* does not run any R code
chunks. Instead, the output of the chunk when it was last run in the
editor is displayed.
