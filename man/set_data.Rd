% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/set_data.R
\name{set_data}
\alias{set_data}
\title{Data prep}
\usage{
set_data(raw_data, categorical_columns, weights_column, debt_column)
}
\arguments{
\item{raw_data}{a data set of credibility.}

\item{categorical_columns}{categorical column of data set.}

\item{weights_column}{weights column of data set.}

\item{debt_column}{credit debt column of data set.}
}
\value{
This function returns a tibble as prepared_data by using raw_data. Adds new columns to raw data as weighted_obs, group_average_weights, variance_column.
}
\description{
Data prep
}
\examples{
raw_data <- debt

categorical_columns <- c(1,2)

weights_column <- 3

debt_column <- 4

prepared_data <- set_data(raw_data, categorical_columns, weights_column, debt_column)
}
