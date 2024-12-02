library(tidyverse)
library(gnm)

# QAIC and QBIC functions -------------------------------------------------

# L:   The log-likelihood, extracted by fitting the model with the non-quasi 
#      family.
# phi: The overdispersion parameter, extracted by fitting the quasi-family and 
#      and dividing the deviance by the residual degrees of freedom.
# k:   The number of parameters, extracted by taking the length of the coefficient
#      vector.
# n:   The number of observations, extracted by taking the number of used 
#      observations when fitting the model.
calc_QAIC_QBIC <- function(model = NULL, L_hat, phi_hat, k, n, .type = "QAIC") {
  bool <- stringr::str_detect(model$family$family, "quasi")
  
  if(bool || length(bool) == 0) {
    print("Must supply model fit with regular likelihood function or L_hat, phi_hat, k, and n.")
  }
  
  if(!is.null(model)) {
    L_hat <- as.numeric(logLik(model))
    phi_hat <- model$deviance/model$df.residual
    k <- length(model$coefficients)
    n <- nrow(model$data)
  }
  
  if(stringr::str_detect(tolower(.type), "aic")) {
    -2*L_hat+2*phi_hat*k
  } else if(stringr::str_detect(tolower(.type), "bic")){
    -2*L_hat+log(n)*phi_hat*k
  }
}

# Get quasi-AIC
qaic <- function(model = NULL, L_hat, phi_hat, k) {
  calc_QAIC_QBIC(model = model, L_hat = L_hat, phi_hat = phi_hat, k = k, .type = "QAIC")
}

# Get quasi-BIC
qbic <- function(model = NULL, L, phi, k, n) {
  calc_QAIC_QBIC(model = model, L_hat = L_hat, phi_hat = phi_hat, k = k, n = n, .type = "QBIC")
}

# Example usage -----------------------------------------------------------

# Say we have the following model:
fit1 <- glm(price ~ carat + x,
            family = quasipoisson,
            data = diamonds)

# Because we are using the quasipoisson family, we cannot extract the 
# log-likelihood.
#
# To get the log-likelihood, we must fit the model with the non-quasi version
# of the likelihood argument.
fit0 <- glm(price ~ carat + x,
            family = poisson,
            data = diamonds)

# Now we can extract the values we need to calculated the QAIC and QBIC
L_hat <- as.numeric(logLik(fit0))
phi_hat <- fit0$deviance/fit0$df.residual
k <- length(fit0$coefficients)
n <- nrow(fit0$data)

# QAIC
-2*L_hat+2*phi_hat*k
# QBIC
-2*L_hat+log(n)*phi_hat*k

# Alternatively, we can use the functions defined above. However, we need to 
# give the correct model.
qaic(fit1)
qaic(fit0)

# Test out functions ------------------------------------------------------

# Fit models
fit1 <- glm(price ~ carat, family = poisson, data = diamonds)
fit2 <- update(fit1, . ~ . + x)
fit3 <- update(fit2, . ~ . + depth)

models <- list(fit1 = fit1, 
               fit2 = fit2, 
               fit3 = fit3)

# Check AIC and BIC
map_dfr(models, \(x) {
  tibble(AIC = AIC(x), 
         QAIC = qaic(x),
         BIC = BIC(x),
         QBIC = qbic(x)
  )
}, .id = "Model")

fit1$deviance
fit2$deviance
fit3$deviance

fit1$df.residual
fit2$df.residual
fit3$df.residual