#' Compute initial estimates for online learning algorithms
#' 
#' This function creates a list of otherArgs used by each online algorithm by
#' calling learner.init for each learner in \code{SL.library}. 
#'
#' @param Y The outcome vector, usually of length \code{nl}.
#' @param X The data.frame of predictors, with \code{nl} rows 
#'
#' @return output A list of otherArgs used by each learner in \code{SL.library}


.getInitialLearners <- function(Y, X, SL.library, ...){
    alply(SL.library, 1, function(learner){
        do.call(paste0(learner),args=list(Y=Y,X=X,newX=X,initial=TRUE))
    })
}