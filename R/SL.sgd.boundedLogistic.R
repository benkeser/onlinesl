#' Computes a stochastic gradient step for bounded logistic regression
#' 
#' Wrapper function for using Logistic GLM with online SL when outcome is 
#' bounded between \code{lower} and \code{upper}. If the function is 
#' called with \code{initial=TRUE} then fits a bounded logistic regression using 
#' the  specified formula and returns predicted values on data frame 
#' \code{newX}. Any input for \code{fit} is ignored. 
#' This is called by \code{onlinesl} to obtain
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
#' @param lower The lower bound on the outcome \code{Y}
#' @param upper The upper bound on the outcome \code{Y}
#' @param stepSize A function evaluating what step size to use in the SGD 
#' algorithm (ignored if \code{initial = TRUE})
#'
#' @return fit An object of class \code{SL.sgd.boundedLogistic}. A named list 
#' with entries \code{theta} and \code{formula} consisting of the current 
#' regression parameter estimates and the regression formula. Also includes the
#' specified \code{upper} and \code{lower} bounds. 
#' @return pred Predictions on newX either based on an initial call to \code{glm}
#' (if \code{initial = TRUE}) or based on an SGD update (if \code{initial=FALSE})
#'
#' @export
#' 
#' @examples
#' Examples to come
#' 

SL.sgd.boundedLogistic <- function(Y, X, newX, t, fit, initial = FALSE,
                           formula="Y ~ .", lower, upper,
                           stepSize=function(t){1/t}, ...){
    if(initial){
        Ytild <- (Y - lower)/(upper - lower)
        suppressWarnings(
            fm <- glm(as.formula(gsub(x=formula, pattern="Y", 
                                      replacement="Ytild")), 
                      data=data.frame(Ytild,X), 
                      family=binomial())
        )
        theta <- fm$coefficients
        # sometimes fit is non-unique, so replace NAs with 0 
        naCoef <- is.na(theta)
        if(sum(naCoef)>0){
            warning("Some NA coefficients in initial fit. Setting = 0")
        }
        theta[naCoef] <- 0
        suppressWarnings(
            ptild <- predict(fm, newdata=newX, type="response")
        )
        pred <- ptild*(upper - lower) + lower
    }else{
        # model matrix
        Xmat <- model.matrix(as.formula(formula), data=data.frame(Y,newX))
        # prediction
        ptild <- plogis(Xmat%*%fit$theta)
        pred <- ptild*(upper - lower) + lower
        # gradient
        grad <- - t(Xmat) %*% ((Y-lower)/(upper - lower) - ptild)
        # learning rate
        g <- do.call(stepSize, args=list(t=t))
        # update step
        theta <- fit$theta - g * grad
    }
    out <- list(fit=list(theta=theta, formula=formula, upper=upper, lower=lower), 
                pred=pred)
    class(out) <- "SL.sgd.boundedLogistic"
    return(out)
}