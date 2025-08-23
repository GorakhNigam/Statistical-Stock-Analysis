# Statistical-Stock-Analysis
Statistical Stock Analysis for Qualcom and Broadcom portfolio in R

1. monte_carlo_random_walk : This stage focused on building a statistical foundation using probability simulations. I modeled random walks and cumulative sums of independent random variables, estimated probabilities through Monte Carlo simulations, and validated them against Binomial and Normal approximations.
          Workflow:
            Simulate random walks with independent ±1 increments
            Estimate different probabilities
            Compare simulated results with Binomial/Normal approximations
            Interpret findings in the context of stochastic processes
2. returns_exploration_qcom_avgo : This project presents a stepwise statistical and financial analysis of Qualcomm (QCOM) and Broadcom (AVGO) stocks. Starting with probability simulations and random walk models, the study progresses into exploratory data analysis, return distributions, hypothesis testing, and advanced statistical methods. Each stage builds toward understanding stock behavior, volatility, and risk patterns through R programming, Monte Carlo methods, and statistical inference.
           Workflow:
             Import and align daily adjusted closing prices for QCOM and AVGO
             Compute log daily returns for both stocks
             Generate descriptive statistics (mean, variance, skewness, kurtosis)
             Plot time series, histograms, and QQ-plots of returns
             Perform normality checks and visualize distributional differences
3. normality_tests_and_density_estimation : Extended the stock analysis by applying normality tests, density estimation, and QQ plots to compare empirical distributions of Qualcomm and Broadcom returns with theoretical ones. Used Shapiro-Wilk, Jarque-Bera, and Kolmogorov-Smirnov tests to assess statistical validity.
          Workflow:
            Compute log returns of stock data
            Perform normality tests (Shapiro-Wilk, Jarque-Bera, KS)
            Generate density plots and QQ plots
            Compare observed vs. theoretical distributions
4. time_series_arima_analysis : Focused on time series analysis of stock returns, incorporating stationarity checks (ADF test), autocorrelation analysis, and ARIMA modeling. Explored whether Qualcomm and Broadcom returns followed random walk or exhibited predictable patterns.
          Workflow:
            Conduct stationarity tests (ADF, KPSS)
            Plot ACF/PACF for autocorrelation detection
            Fit ARIMA models for forecasting
            Compare model fit and residual diagnostics
5. bootstrap_and_hypothesis_testing : Applied hypothesis testing and resampling methods on the two stocks. Conducted bootstrap variance estimation, confidence intervals, and hypothesis testing on mean returns and volatility to evaluate market risk.
          Workflow:
            Bootstrap resampling of returns
            Estimate variance and confidence intervals
            Hypothesis testing on stock mean differences
            Assess robustness of inference under resampling        
6. copula_dependence_modeling : Integrated the analysis by applying copula-based dependence modeling between Qualcomm and Broadcom returns. Used Clayton, Gaussian, t, and Joe copulas to capture tail dependencies and joint risk patterns.
          Workflow:
            Fit empirical copulas on return data
            Compare with parametric copulas (Clayton, Gaussian, t, Joe)
            Visualize dependence using contour plots
            Interpret joint risk behavior of Qualcomm & Broadcom
