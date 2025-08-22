
## Problem (1)

W$1$, W$2$, . . . , W$200$, where for each k, W$k$ = X$1$ + · · · +
X$k$, and where X$1$, X$2$, . . . , X$200$ are independent, each taking
value 1 with probability 1/2 and −1 with probability1/2. In (a) and (b),
perform simulations to obtain good estimates for the following
probabilities. \[Hint: Notice that each Xj can be described in the form
2Bj − 1, where Bj is a binomial (Bernoulli) random variable with
parameters n = 1 and p = .5. Thus the cumulative sum Wk can be built
using the command rbinom.\]

### (a) P\[W$200$ ≥ 10\]

``` r
final_count <- 0
for (i in 1:1000000) {
  sample_generation <- sample(c(-1,1), 200, replace = TRUE)
  pos_count <- sum(sample_generation)
  if (pos_count > 9){
    final_count <- final_count + 1
  }
}
print(final_count/1000000)
```

    ## [1] 0.262814

### (b) P\[max$1≤k≤200$ W$k$ ≥ 10\]

Solution: Lets iterate through all the W$k$’s and find the solution with
max probability

``` r
final_count <- 0
for (i in 1:10000000) {
  
  sample_generation <- sample(c(-1,1), 200, replace = TRUE) #2*rbinom(200, 1, 0.5) - 1
  W <- cumsum(sample_generation)
  #pos_count <- sum(sample_generation)
  if (max(W) > 9){
    final_count <- final_count + 1
  }
}
print(final_count/10000000)
```

    ## [1] 0.4806414

### (c) P\[W$200$ ≥ 10\]

Solution: Step 1 : Convert the problem into standard (0,1) binomial form

Using the conversion X$i$ to 2\*X$i$ - 1

P\[ 2\*(X$1$ - 1) + 2\*(X$2$ - 1) + …. + 2\*(X$200$ - 1)} $>=$ 10\]

= P\[ 2\*(X$1$ + X$2$ + X$3$ + …. + X$200$) $>=$ 210\]

= P\[ (X$1$ + X$2$ + X$3$ + …. + X$200$) $>=$ 105\]

= P\[W$200$ \>= 105\]

= 1 - P\[W$200$ \< 104\]

``` r
{1- pbinom(104, 200, 0.5)}
```

    ## [1] 0.2623112

## Problem (2)

V$1$, V$2$, . . . , V$200$, where for each k, V$k$ = Y$1$ + · · · +
Y$k$, and where Y$1$, Y$2$, . . . , Y$200$ are independent, each taking
value 1 with probability .55 and −1 with probability .45. In (a) and
(b), perform simulations to obtain good estimates for the following
probabilities:

### (a) P\[V$200$ ≥ 10\]

``` r
final_count <- 0
for (i in 1:1000000) {
  sample_generation <- 2*rbinom(200, 1, 0.55) - 1#sample(c(-1,1), 200, replace = TRUE)
  pos_count <- sum(sample_generation)
  if (pos_count > 9){
    final_count <- final_count + 1
  }
}
print(final_count/1000000)
```

    ## [1] 0.782928

### (b) P\[max$1≤k≤200$ V$k$ ≥ 10\]

``` r
final_count <- 0
for (i in 1:10000000) {
  
  sample_generation <- 2*rbinom(200, 1, 0.55) - 1 #sample(c(-1,1), 200, replace = TRUE)
  W <- cumsum(sample_generation)
  #pos_count <- sum(sample_generation)
  if (max(W) > 9){
    final_count <- final_count + 1
  }
}
print(final_count/10000000)
```

    ## [1] 0.8871346

### (c) P\[V$200$ ≥ 10\]

Solution: Step 1 : Convert the problem into standard (0,1) binomial form

Using the conversion X$i$ to 2\*X$i$ - 1

P\[ 2\*(X$1$ - 1) + 2\*(X$2$ - 1) + …. + 2\*(X$200$ - 1)} $>=$ 10\]

= P\[ 2\*(X$1$ + X$2$ + X$3$ + …. + X$200$) $>=$ 210\]

= P\[ (X$1$ + X$2$ + X$3$ + …. + X$200$) $>=$ 105\]

= P\[V$200$ \>= 105\]

= 1 - P\[V$200$ \< 104\]

``` r
{1- pbinom(104, 200, 0.55)}
```

    ## [1] 0.78305

## Problem (3)

U$1$, U$2$, . . . , U$200$, where for each k, U$k$ = Z$1$ + · · · +
Z$k$, and where Z$1$, Z$2$, . . . , Z$200$ are independent, normal
random variables, each having mean 0 and variance 1. Perform simulations
to obtain good estimates for the following probabilities:

### (a) P\[U$200$ ≥ 10\]

``` r
final_count <- 0
for (i in 1:1000000) {
  sample_generation <- rnorm(200, mean = 0, sd = 1)#sample(c(-1,1), 200, replace = TRUE)
  pos_count <- sum(sample_generation)
  if (pos_count > 9){
    final_count <- final_count + 1
  }
}
print(final_count/1000000)
```

    ## [1] 0.261444

### (b) P\[max$1≤k≤200$ U$k$ ≥ 10\]

``` r
final_count <- 0
for (i in 1:1000000) {
  sample_generation <- rnorm(200, mean = 0, sd = 1) #sample(c(-1,1), 200, replace =TRUE)
  W <- cumsum(sample_generation)
  if (max(W) > 9){
    final_count <- final_count + 1
  }
}
print(final_count/1000000)
```

    ## [1] 0.498196

### (c) P\[U$200$ ≥ 10\]

Solution: E\[U$i$\] = 0 and Var\[U$i$\] = 200\*1

P\[ (Z$1$ + Z$2$ + Z$3$ + …. + Z$200$) $>=$ 10\]

= P\[U$200$ \>= 10\]

= 1 - P\[U$200$ \< 9\]

Standardizing P\[U$200$ \< 9\]

= 1 - P\[U$200$/sqrt(200) \< 9/sqrt(200)\] = 1 - P\[z \< 0.6364\]

``` r
1-pnorm(0.6364,mean=0,sd=1)
```

    ## [1] 0.2622579
