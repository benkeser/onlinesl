#' Bounded negative log-likelihood loss function for ensemble
#' 
#' This function computes the average bounded negative log-likelihood
#' loss for a given set of outcomes Y of length, an n x K matrix of predictions 
#' p, and a row vector of weights alpha. 
#'
#' @param Y The outcome vector, usually of length \code{nl}.
#' @param p An n x K matrix of predictions from K different online learners
#' @param alpha A K row vector of weights
#' @param ensemblePredictFn The function used to compute ensemble predictions.
#' If null, the function assumes there is no ensemble to compute loss for and 
#' instead computes loss for vector Y and vector p. 
#' @param upper The upper bound on \code{Y} 
#' @param lower The lower bound on \code{Y}
#' @return loss The scalar value of the negative log-likelihood loss


.loglikBounded <- function(Y, p, alpha, ensemblePredictFn=NULL, risk=TRUE,
                           upper, lower){
    if(is.null(ensemblePredictFn)){
        if(!risk){
            (Y-lower)/(upper-lower)*log((p - lower)/(upper - lower)) + 
                (1-(Y-lower)/(upper-lower))*log(1-(p - lower)/(upper - lower))
        }else{
            mean((Y-lower)/(upper-lower)*log((p - lower)/(upper - lower)) + 
                     (1-(Y-lower)/(upper-lower))*log(1-(p - lower)/(upper - lower)))
        }
    }else{
        pEnsemble <- do.call(ensemblePredictFn, args=list(p=p,alpha=alpha))
        if(!risk){
            (Y-lower)/(upper-lower)*log((pEnsemble - lower)/(upper - lower)) + 
                (1-(Y-lower)/(upper-lower))*log(1-(pEnsemble - lower)/(upper - lower))
        }else{
            mean((Y-lower)/(upper-lower)*log((pEnsemble - lower)/(upper - lower)) + 
                     (1-(Y-lower)/(upper-lower))*log(1-(pEnsemble - lower)/(upper - lower)))
        }
    }
}