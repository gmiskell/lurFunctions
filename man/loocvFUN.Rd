% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/loocvFUN.R
\name{loocvFUN}
\alias{loocvFUN}
\title{A function for performing leave-one-out cross-validation}
\usage{
loocvFUN(x, sites, response, variables)
}
\arguments{
\item{sites}{The variable that identifies the relational element.}

\item{response}{The response variable.}

\item{variables}{A string of explanatory covariates in the model.}
}
\description{
This function removes one observation at a time and reruns the model in order to test model parameter stability.
}
\examples{
data(iris)
loocvFUN(iris, sites = 'Species', response = 'Petal.Width', variables = 'Sepal.Length+Sepal.Width')
}
