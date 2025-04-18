% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portfolio_objects.R
\name{summary.performance2}
\alias{summary.performance2}
\title{Summarize Advanced Portfolio Performance Metrics and Statistical Tests}
\usage{
\method{summary}{performance2}(
  portfolio_object,
  transaction_cost = 0.001,
  gamma = 3,
  test = FALSE,
  print = FALSE
)
}
\arguments{
\item{portfolio_object}{A \code{portfolioReturns} object.}

\item{transaction_cost}{Numeric. Assumed transaction costs for turnover adjustment.}

\item{gamma}{Numeric. Risk aversion parameter for Certainty Equivalent Return.}

\item{test}{Logical. If \code{TRUE}, performs statistical tests for significance.}

\item{print}{Logical. If \code{TRUE}, prints a formatted table of results.}
}
\value{
A data frame summarizing portfolio performance metrics and test results.
}
\description{
Provides a detailed summary of advanced portfolio performance metrics,
including statistical tests and transaction cost adjustments.
}
