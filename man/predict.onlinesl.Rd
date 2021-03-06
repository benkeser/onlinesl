% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/onlinesl-predict-method.R
\name{predict.onlinesl}
\alias{predict.onlinesl}
\title{Predict method for onlinesl objects}
\usage{
\method{predict}{onlinesl}(object, newdata, X = NULL, Y = NULL,
  onlySL = FALSE, ...)
}
\arguments{
\item{object}{An object of class \code{onlinesl}}

\item{newdata}{A \code{data.frame} object on which to obtain new predictions.}

\item{X}{Not sure yet}

\item{Y}{Not sure yet}

\item{onlySL}{Return predictions only for algorithms with non-zero weight in the online 
super learner?}
}
\value{
pred The continuous online super learner predictions

library.predict The predictions from each candidate online algorithm
}
\description{
This function computes the ensemble prediction for the online super learner.
}

