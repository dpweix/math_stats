library("tidyverse")
library("gnm")

# QAIC and QBIC Functions -------------------------------------------------

# L:   The log-likelihood, calculated by summing over log-scaled output of the 
#      probability density function of the Poisson distribution.
# phi: The overdispersion parameter, extracted from the model summary.
# k:   The number of model parameters, extracted by taking the length of the coefficient
#      vector.
# n:   The number of observations, extracted by taking the number of used 
#      observations when fitting the model.

# Function to compute the QAIC for quasi-Poisson models
QAIC <- function(model) {
  L_hat   <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  phi_hat <- summary(model)$dispersion
  k       <- length(model$coefficients)
  
  -2*L_hat+2*phi_hat*k
}

# Function to compute the QBIC for quasi-Poisson models
QBIC <- function(model) {
  L_hat   <- sum(dpois(model$y, model$fitted.values, log=TRUE))
  phi_hat <- summary(model)$dispersion
  k       <- length(model$coefficients)
  n       <- nrow(model$data)
  
  -2*L_hat+log(n)*phi_hat*k
}

# Example usage -----------------------------------------------------------

# For this example we use the diamonds data set (downloaded with tidyverse)
help(diamonds)
diamonds

# Consider the following models:
# Poisson model
fit0 <- glm(price ~ carat + x,
            family = poisson,
            data = diamonds)

# quasi-Poisson model
fit1 <- glm(price ~ carat + x,
            family = quasipoisson,
            data = diamonds)

# Conditionally fit quasi-Poisson model
fit2 <- gnm(price ~ carat + x,
            family = quasipoisson,
            eliminate = color,
            data = diamonds)

# To get the AIC, normally we can just use the AIC function.
AIC(fit0)
AIC(fit1)
AIC(fit2)

# Unfortunately, this does not work for quasi-families because their
# log-likelihood is not automatically calculated. 
logLik(fit0)
logLik(fit1)
logLik(fit2)

# However, we can still manually calculated them. 
sum(dpois(fit0$y, fit0$fitted.values, log=TRUE))
sum(dpois(fit1$y, fit1$fitted.values, log=TRUE))
sum(dpois(fit2$y, fit2$fitted.values, log=TRUE))

# Additionally, we need the QAIC and QBIC by the overdispersion parameter.
# This value can be extracted from the model summary, or manually calculated
# as the weighted sum of the square squared residuals divided by the residual 
# degrees of freedom.
summary(fit1)$dispersion
sum(fit1$weights * fit1$residuals^2)/fit1$df.residual

# Note that using the Poisson family instead of the quasi-Poisson forces the
# dispersion parameter to 1. 
summary(fit0)$dispersion
sum(fit0$weights * fit0$residuals^2)/fit0$df.residual

# Now we can calculate the QAIC with the formula from Equation 13 of 
# Gasparrini's 2010, DLNM paper (https://doi.org/10.1002%2Fsim.3940)
QAIC(fit0)
QAIC(fit1)
QAIC(fit2)

# Similarly, we calculate the QBIC
QBIC(fit0)
QBIC(fit1)
QBIC(fit2)