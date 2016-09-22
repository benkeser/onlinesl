#' Ensemble predict function for convex combination of predictions
#' 
#' This function computes the ensemble prediction for the super learner
#' based on a convex combination of weights
#' 
#' @param p An n x K matrix of predictions from K different online learners
#' @param alpha A K row vector of weights
#'
#' @return prediction An n-length vector of super learner predictions

convexLinCom <- function(p, alpha){
    p %*% alpha
}
