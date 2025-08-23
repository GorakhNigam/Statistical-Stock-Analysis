Gorakhnath Nigam
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

**Q1** In problem set \#4, you had used contour and pCopula to create
contour diagrams for the four estimated copulas (t, Normal, Clayton, and
Joe) associated with your two log-return data sets. We will now build
the empirical copula and compare it with the estimated Clayton copula
(only). \[Similarly, the empirical copula could be compared with t,
normal, and Joe, but that will not be required here.\] As on lines 39-42
of page 212 of Ruppert and Matteson (2nd ed.), create a contour diagram
for the empirical copula associated with your set of data pairs. \[As in
exercise 4 of set \#4, you can work either with data1 (lines 14-15) or
with data2 (lines 16-17), page 211.\] In creating the empirical copula,
please note that there is a typo within the assignment statement for Cn
on line 40 on page 212. It should read X=data1 and not U=data1. Compare
this empirical copula with the estimated Clayton copula by superimposing
the empirical contours on top of the contours from your fitted Clayton
copula. You can imitate page 212, lines 56-59.

``` r
data2 = cbind(rank(QCOMlogreturn)/(length(QCOMlogreturn)+1), rank(AVGOlogreturn)/(length(AVGOlogreturn)+1)) 

fclayton = fitCopula(copula = joeCopula(2, dim=2), data = data2, method = "ml")

fitted_cop <- claytonCopula(fclayton@estimate, dim = 2)

emp_cop <- empCopula(data2)

u <- seq(0.01, 0.99, length.out = 50)
grid <- expand.grid(u, u)

z_emp <- pCopula(as.matrix(grid), emp_cop)
z_clayton <- pCopula(as.matrix(grid), fitted_cop)

z_emp_mat <- matrix(z_emp, nrow=50)
z_clayton_mat <- matrix(z_clayton, nrow=50)

contour(u, u, z_clayton_mat, xlab="u1", ylab="u2",
        main="Empirical Copula vs. Fitted Clayton Copula")
contour(u, u, z_emp_mat, add=TRUE, col="blue", lty=2)
legend("bottomright", legend=c("Clayton", "Empirical"), col=c("black", "blue"), lty=c(1, 2))
```

![](Gorakhnath_Nigam_Assignment-5A_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

Sol2 : Note, below code has been taken from Ruppert and Matteson and
adjusted to fit our problem statement.

``` r
n = length(data2)
Udex = (1:n)/(n+1)
Cn = C.n(u=cbind(rep(Udex,n),rep(Udex,each=n)), X=data2)#, method="C")
EmpCop = expression(contour(Udex, Udex, matrix(Cn, n, n),col = 2, add = TRUE))

contour(claytonCopula(param=fclayton@estimate[1], dim = 2),
pCopula, main = expression(hat(C)[Cl]), xlab = expression(hat(U)[1]), ylab = expression(hat(U)[2]) )
eval(EmpCop)
```

![](Gorakhnath_Nigam_Assignment-5A_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Q2** Next, for this estimated Clayton copula (cdf), use contour and
dCopula to create a diagram of this estimate for the parametric density
(pdf) associated with this parametric Clayton copula (cdf). Then use the
kde2d command to create a diagram of the 2-dimensional kernel density
estimate for your copula’s empirical density. You can imitate lines
81-84 on page 213. Superimpose (on top of the parametric density
estimate) the kernel density estimate for the quantile pairs
(percentiles) for the bivariate log-return data. (Use the add = TRUE
instruction.) Discuss the diagram, comparing the KDE with the pdf
associated with the Clayton copula.

``` r
contour(claytonCopula(param=fclayton@estimate, dim = 2),
dCopula, main = expression(hat(c)[Cl]),
nlevels=25, xlab=expression(hat(U)[1]),ylab=expression(hat(U)[2]))
contour(kde2d(data2[,1],data2[,2]), col = "red", add = TRUE)
legend("bottomright", legend = c("Clayton PDF", "Empirical KDE"), col = c("black", "red"), lty = c(1, 2))
```

![](Gorakhnath_Nigam_Assignment-5A_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Comment : The contour plot highlights the concentrated joint density
near the lower-left corner (near to (0,0)), which is where both the
empirical KDE and the Clayton copula reaches it’s peak. This confirms
that the Clayton copula effectively captures strong lower-tail
dependence. However, the KDE shows a more spread-out and irregular shape
across the domain. This suggests that the Clayton copula, while
appropriate for capturing extreme joint losses, may not completely
captures the mid-range and upper-tail co-movements as shown by KDE.
Hence, there is trade-off between parametric simplicity and empirical
flexibility.

**Q3** Let S be the set {(xi; yi) : 1 \<= i \<= N} of daily log-returns
for the two stocks which you analyzed in previous problem sets. As in
section 5.2 of the ISLR text (2nd edition), let alpha be the number
between 0 and 1 which minimizes the variance of alphaX + (1 - alpha)Y .
Use your data and the formula derived in class to determine an estimate
alpha^ for alpha. Then perform a bootstrap analysis, where you select,
with replacement, a random sample of N pairs from S, and do this 1000
times. Use the 1000 results to obtain a good estimate for the variance
of alpha^. Also, display a histogram summarizing the 1000 estimates for
alpha^. Discuss your results thoroughly.

``` r
set.seed(123)

Z_variance <- function(alpha, X, Y) {
  Z <- alpha * X + (1 - alpha) * Y
  return(var(Z))
}

alpha_hat <- optimize(Z_variance, interval = c(0, 1), X = QCOMlogreturn, Y = AVGOlogreturn)$minimum

n <- length(QCOMlogreturn)
B <- 1000
alpha_boot <- numeric(B)

for (b in 1:B) {
  idx <- sample(1:n, n, replace = TRUE)
  alpha_boot[b] <- optimize(Z_variance, interval = c(0, 1), 
                            X = QCOMlogreturn[idx], Y = AVGOlogreturn[idx])$minimum
}

alpha_hat_var <- var(alpha_boot)

hist(alpha_boot, breaks = 30, col = "lightgreen",
     main = expression("Bootstrap Distribution of" ~ hat(alpha)),
     xlab = expression(hat(alpha)))
abline(v = alpha_hat, col = "red", lwd = 2, lty = 2)
```

![](Gorakhnath_Nigam_Assignment-5A_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
# Summary
list(alpha_hat = alpha_hat, var_hat = alpha_hat_var)
```

    ## $alpha_hat
    ## [1] 0.5433439
    ## 
    ## $var_hat
    ## [1] 0.005493987

Note : Based on the ISLR2 book

``` r
library(ISLR2)
```

    ## Warning: package 'ISLR2' was built under R version 4.4.3

    ## 
    ## Attaching package: 'ISLR2'

    ## The following object is masked from 'package:MASS':
    ## 
    ##     Boston

``` r
library(boot)
```

    ## Warning: package 'boot' was built under R version 4.4.3

``` r
datax = data.frame(X = QCOMlogreturn, Y = AVGOlogreturn)

alpha.fn <- function(data , index) {
  X <- data$X[index]
  Y <- data$Y[index]
  (var(Y) - cov(X, Y)) / (var(X) + var(Y) - 2 * cov(X, Y))
}
boot(datax , alpha.fn, R = 1000)
```

    ## 
    ## ORDINARY NONPARAMETRIC BOOTSTRAP
    ## 
    ## 
    ## Call:
    ## boot(data = datax, statistic = alpha.fn, R = 1000)
    ## 
    ## 
    ## Bootstrap Statistics :
    ##      original       bias    std. error
    ## t1* 0.5433439 -0.005871823  0.07336501

``` r
CI_95 <- quantile(alpha_boot, probs = c(0.025, 0.975))
CI_95
```

    ##      2.5%     97.5% 
    ## 0.3925355 0.6745028

As shown above, the optimal allocation parameters are alpha = 0.5433,
with a bootstrap variance of approximately 0.00549, and a standard error
of 0.0741. The histogram of bootstrap estimates is symmetric and
centered near the original estimate, which shows the stability of the
solution and bell curve distribution. This suggests that the optimal
portfolio places slightly more weight on QCOM than AVGO for minimum
variance (Risk Optimization), though the bootstrap variation highlights
some sensitivity to sampling variability and gives you risk estimation
on estimated alpha. Also we have 95% confidence interval between
\[0.3925348, 0.6745020\]

