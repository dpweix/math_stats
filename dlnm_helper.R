# Helper function for getting MMT
get_cen <- function(crosspred) {
  if(!is.null(crosspred$fit)) {
    crosspred$predvar[which.min(crosspred$fit)]
  } else if(!is.null(crosspred$allfit)) {
    crosspred$predvar[which.min(crosspred$allfit)]
  } else {
    print("Unable to locate minimum.")
  }
}

# Helper function for extracting reduced curve
get_df_RRs <- function(pred) {
  tibble(Temperature = pred$predvar,
         RR = pred$RRfit,
         RR_low = pred$RRlow,
         RR_high = pred$RRhigh)
}