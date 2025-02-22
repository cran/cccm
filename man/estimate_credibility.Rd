% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/estimate_credibility.R
\name{estimate_credibility}
\alias{estimate_credibility}
\title{The Credibility Premium Estimates}
\usage{
estimate_credibility(
  raw_data,
  categorical_columns,
  weights_column,
  debt_column
)
}
\arguments{
\item{raw_data}{a data set of credibility.}

\item{categorical_columns}{categorical column of data set.}

\item{weights_column}{weights column of data set.}

\item{debt_column}{credit dept column of data set.}
}
\value{
returns premium estimation of credibility.
}
\description{
The Credibility Premium Estimates
}
\examples{
raw_data <- debt

categorical_columns <- c(1,2)

weights_column <- 3

debt_column <- 4

estimate_credibility(raw_data, categorical_columns, weights_column, debt_column)
}
