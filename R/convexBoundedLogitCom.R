#' Ensemble predict function for convex combination of predictions on the logit
#' scale, backtransformed to original scale
#' 
#' This function computes the ensemble prediction for the super learner
#' based on a convex combination of weights on the logit scale and transforms
#' predictions back to the original scale based on the value of \code{lower}
#' and \code{upper}. 
#' 
#' @param p An n x K matrix of predictions from K different online learners
#' @param alpha A K row vector of weights
#'
#' @return prediction An n-length vector of super learner predictions

convexBoundedLogitCom <- function(p, alpha, trimLogit = 1e-5,
                                  lower, upper){
    pB <- (p - lower)/(upper - lower)
    pB[pB < trimLogit] <- trimLogit; 
    pB[pB > 1-trimLogit] <- 1-trimLogit
    plogis(qlogis(pB) %*% alpha)*(upper - lower) + lower
}
