#' Install the sandwich package if necessary
if (!require("sandwich")) install.packages("sandwich")

#' Calculate the nearly exact bias and edf for the HC1 Variance
#' Estimator.
#' @param x design matrix of the regression. A matrix with all the independent
#' variables in the regression.
#' @param w hypothesis test. A vector of numerics that represents the contrast 
#' for which to test a hypothesis. `w = c(0, 1, 0)` represents the test whether
#' the second variable in `x` is statistically significant.
#' @return A list with two elements. `mu` is the estimated mean of the
#' chi-squared distribution in the test statistic for the hypothesis test. `ecf`
#' is the effective degrees of freedom for the test statistic.
nearly_exact <- function(x, w){
  N = nrow(x); k = ncol(x) - 1; c = N / (N - k);
  xx <- solve(crossprod(x))
  M = diag(rep(1, N)) - tcrossprod(x %*% xx, x)
  z = tcrossprod(t(w) %*% xx, x); zz = z %*% t(z); z2 = z^2;
  mu = c/zz * sum(z^2 * diag(M))
  nu = 2 * c^2 / zz^2 * (tcrossprod(z2 %*% M^2, z2))
  edf = 2 * mu^2 / nu
  return(list(mu = as.numeric(mu), edf = as.numeric(edf)))
}

#' Calculate the coefficient table for the nearly exact bias and
#' df correction.
#' @param formula The regression formula as would be used with the standard
#' `stats::lm` function.
#' @param data A dataframe with the dependent and independent variable in the
#' `formula`
#' @param pdigits Number of digits to round the p-value.

lm_nearly <- function(formula, data, pdigits = 4){
  reg <- lm(formula = formula, data = data)
  x <- model.matrix(object = formula, data = data)
  covHC <- sandwich::vcovHC(x = reg, type = "HC1")
  seHC <- sqrt(diag(covHC)); varnames <- names(seHC); 
  coefficients <- data.frame(matrix(nrow = length(varnames), ncol = 6))
  names(coefficients) <- c("variable", "estimate", "se",
                           "t value", "df", "p value")
  coefficients$variable <- varnames
  for (var in varnames){
    hypothesis = I(varnames == var)
    correction <- nearly_exact(x = x, w = hypothesis)
    se = seHC[var] / sqrt(correction$mu)
    stat <- reg$coefficients[var]/se
    coefficients[coefficients$variable == var, 2:6] <- c(
      reg$coefficients[var], se, stat, correction$edf, 
      round(2 * pt(abs(stat), correction$edf, lower.tail = FALSE),
            pdigits)
    )
  }
  return(list(coefficients = coefficients, lm = reg))
}

#' Useage

corrected <- lm_nearly(speed ~ dist, cars)
print(corrected$coefficients, digits = 2)

#' Test with simcompl2

sample <- simcompl2::create_sample(obs = 200, rate = .25,
                                   b2 = c(0, 0, 0),
                                   g1 = c(.33, .33, 0),
                                   sd_eps = c(1, 1, 1))
perf <- lm_nearly(
  y ~ (x1 + x2 + z)^2 + I(x1^2) + I(x2^2), data = sample
)
print(perf$coefficients, digits = 2)
print(summary(perf$lm), digits = 2)

