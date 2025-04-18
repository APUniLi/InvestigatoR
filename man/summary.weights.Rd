% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/portfolio_objects.R
\name{summary.weights}
\alias{summary.weights}
\title{Summarize Portfolio Weights with Advanced Options}
\usage{
\method{summary}{weights}(portfolio_object, use_delta = FALSE, print = FALSE)
}
\arguments{
\item{portfolio_object}{A \code{portfolioReturns} object.}

\item{use_delta}{Logical. If \code{TRUE} and the portfolio object has a benchmark, computes statistics for delta_weights. Defaults to \code{FALSE}.}

\item{print}{Logical. If \code{TRUE}, prints the summary in a nicely formatted table. Defaults to \code{FALSE}.}
}
\value{
A tibble with advanced portfolio weight statistics.
}
\description{
Computes summary statistics for portfolio weights, including deviations, turnover, L1/L2 norms, and weight stability.
}
