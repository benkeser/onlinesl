#' Perform first order stochastic gradient descent update of super learner 
#' weights
#' 
#' This function performs a single step of gradient descent on the weight 
#' vector for the super learner weights and projects the resulting vector onto
#' the L1-simplex via the internal function .projToL1Simp. The function returns
#' the updated weight vector. 
#'
#' @param Y The outcome at iteration t
#' @param slFit.t A named list with a component named alpha.t that contains the
#' 1-column matrix of current estimate of the super learner weights 
#' @param p.t The predictions from the various online algorithms at time t
#' @param tplus1 The iteration of the online algorithm
#' @param stepSize The size of the step to take in the direction of the
#' gradient. If \code{stepSize=NULL} (default) the function uses 
#' \code{1/tplus1}.
#'
#' @return alpha A matrix of updated weights. 
#' 
#' @export

sgdWt_convexLinComBounded <- function(Y,slFit.t,p.t,tplus1,stepSize=NULL,
                                      lower, upper){
    if(is.null(stepSize)){
        stepSize <- 1/tplus1
    }
    ptild <- (p.t - lower)/(upper-lower)
    grad <- - t(ptild) %*% (Y - plogis(qlogis(ptild)%*%slFit.t$alpha))
    cwt <- slFit.t$alpha - stepSize * grad
    wt.tplus1 <- onlinesl:::.projToL1Simp(cwt)
    list(alpha=wt.tplus1)
}