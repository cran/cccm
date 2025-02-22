% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_varianceComponents.R
\name{calculate_varianceComponents}
\alias{calculate_varianceComponents}
\title{Variance Components}
\usage{
calculate_varianceComponents(
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
variance components
}
\description{
Variance Components
}
\examples{
raw_data <- debt

categorical_columns <- c(1,2)

weights_column <- 3

debt_column <- 4

calculate_varianceComponents(raw_data, categorical_columns, weights_column, debt_column)
}
