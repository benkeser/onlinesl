#' Computes a stochastic gradient step for poisson regression
#' 
#' Wrapper function for using Poisson GLM with online SL. If the function is 
#' called with \code{initial=TRUE} then fits a Poisson regression using the 
#' specified formula and returns predicted values on data frame \code{newX}. Any 
#' input for \code{fit} is ignored. This is called by \code{onlinesl} to obtain
#' starting values for the SGD algorithm. If the function is called with 
#' \code{inital=FALSE}, the function computes a prediction on \code{newX} based 
#' on the current \code{fit} and updates the value of \code{fit} by taking a  
#' step in the direction of the gradient. The function returns a list with named
#' entries \code{fit} and \code{pred}. \code{fit} contains the current regression
#' parameter estimate, while \code{pred} contains the predictions on the 
#' \code{newX} data frame.
#'
#' @param Y The numeric input outcome 
#' @param X The data.frame of predictors (ignored if \code{initial = FALSE})
#' @param newX The data.frame to return predictions for.
#' @param initial Boolean as to whether the function should be called to obtain
#' initial regression parameter estimates or whether it should perform an SGD
#' updating step
#' @param t The numeric step number (ignored if \code{initial = TRUE})
#' @param fit A list of the current fit with named entry theta corresponding 
#' to the regression parameter estimates at the previous iteration and formula
#' corresponding to the regression formula used by the function.
#' @param initial A boolean indicating whether to execute the function for
#' initial parameter estimates or for an sgd update step. 
#' @param formula The regression formula to be used, specified as a character
#' @param stepSize A function evaluating what step size to use in the SGD 
#' algorithm (ignored if \code{initial = TRUE})
#'
#' @return fit An object of class \code{SL.sgd.poisson}. A named list with 
#' entries \code{theta} and \code{formula} consisting of the current regression 
#' parameter estimates and the regression formula.
#' @return pred Predictions on newX either based on an initial call to \code{glm}
#' (if \code{initial = TRUE}) or based on an SGD update (if \code{initial=FALSE})
#'
#' @export
#' 
#' @examples
#' Examples to come
#' 

SL.sgd.poisson <- function(Y, X, newX, t, fit, initial = FALSE,
                           formula="Y ~ .", 
                           stepSize=function(t){1/t}, ...){
    if(initial){
        suppressWarnings(
            fm <- glm(as.formula(formula), data=data.frame(Y,X), family=poisson())
        )
        theta <- fm$coefficients
        # sometimes fit is non-unique, so replace NAs with 0 
        naCoef <- is.na(theta)
        if(sum(naCoef)>0){
            warning("Some NA coefficients in initial fit. Setting = 0")
        }
        theta[naCoef] <- 0
        suppressWarnings(
            pred <- predict(fm, newdata=newX, type="response")
        )
    }else{
        # model matrix
        Xmat <- model.matrix(as.formula(formula), data=data.frame(Y,newX))
        # prediction
        pred <- exp(Xmat%*%fit$theta)
        # gradient
        grad <- - t(Xmat) %*% (Y - pred)
        # learning rate
        g <- do.call(stepSize, args=list(t=t))
        # update step
        theta <- fit$theta - g * grad
    }
    out <- list(fit=list(theta=theta, formula=formula), pred=pred)
    class(out) <- "SL.sgd.poisson"
    return(out)
}