#' Negative log-likelihood loss function for ensemble
#' 
#' This function computes the average squared-error loss for a given set of
#' outcomes Y of length, an n x K matrix of predictions p, and a row vector
#' of weights alpha. 
#'
#' @param Y The outcome vector, usually of length \code{nl}.
#' @param p An n x K matrix of predictions from K different online learners
#' @param alpha A K row vector of weights
#' @param ensemblePredictFn The function used to compute ensemble predictions.
#' If null, the function assumes there is no ensemble to compute loss for and 
#' instead computes loss for vector Y and vector p. 
#'
#' @return loss The scalar value of the negative log-likelihood loss


.loglik <- function(Y, p, alpha, ensemblePredictFn=NULL, risk=TRUE){
    if(is.null(ensemblePredictFn)){
        if(!risk){
            -Y*log(p) - (1-Y)*log(1-p)
        }else{
            mean(-Y*log(p) - (1-Y)*log(1-p))
        }
    }else{
        pEnsemble <- do.call(ensemblePredictFn, args=list(p=p,alpha=alpha))
        if(!risk){
            -Y*log(pEnsemble) - (1-Y)*log(1-pEnsemble)
        }else{
            mean(-Y*log(pEnsemble) - (1-Y)*log(1-pEnsemble))
        }
    }
}