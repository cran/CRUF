% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logistic_regression.r
\name{logistic_cluster_multivariate}
\alias{logistic_cluster_multivariate}
\title{Multivariate Logistic Regression with cluster}
\usage{
logistic_cluster_multivariate(fit)
}
\arguments{
\item{fit}{Class glm.cluster. Multivariate model to format}
}
\value{
A dataframe of the multivariate parameters formatted
}
\description{
A function used to generate result table for multivariate logistic regression
model using a cluster variable. Compute robust variance using sandwich
}
