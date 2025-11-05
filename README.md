# Statistical-Stock-Analysis
Statistical Stock Analysis for Qualcom and Broadcom (both independently and their dependecy on each other) portfolio in R

1. monte_carlo_random_walk : This stage focused on building a statistical foundation using probability simulations. I modeled random walks and cumulative sums of independent random variables, estimated probabilities through Monte Carlo simulations, and validated them against Binomial and Normal approximations.

2. returns_exploration_qcom_avgo : This project presents a stepwise statistical and financial analysis of Qualcomm (QCOM) and Broadcom (AVGO) stocks. Starting with probability simulations and random walk models, the study progresses into exploratory data analysis, return distributions, hypothesis testing, and advanced statistical methods. Each stage builds toward understanding stock behavior, volatility, and risk patterns through R programming, Monte Carlo methods, and statistical inference.

3. normality_tests_and_density_estimation : Extended the stock analysis by applying normality tests, density estimation, and QQ plots to compare empirical distributions of Qualcomm and Broadcom returns with theoretical ones. Used Shapiro-Wilk, Jarque-Bera, and Kolmogorov-Smirnov tests to assess statistical validity.

4. time_series_arima_analysis : Focused on time series analysis of stock returns, incorporating stationarity checks (ADF test), autocorrelation analysis, and ARIMA modeling. Explored whether Qualcomm and Broadcom returns followed random walk or exhibited predictable patterns.

5. bootstrap_and_hypothesis_testing : Applied hypothesis testing and resampling methods on the two stocks. Conducted bootstrap variance estimation, confidence intervals, and hypothesis testing on mean returns and volatility to evaluate market risk.
   
6. copula_dependence_modeling : Integrated the analysis by applying copula-based dependence modeling between Qualcomm and Broadcom returns. Used Clayton, Gaussian, t, and Joe copulas to capture tail dependencies and joint risk patterns.


![R](https://img.shields.io/badge/R-4.x-lightblue.svg)
![Markdown](https://img.shields.io/badge/Markdown-Documentation-lightgrey.svg)
