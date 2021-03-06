% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fns.R
\name{corner}
\alias{corner}
\title{Show the top-left corner}
\usage{
corner(x, nrow = 5, ncol = 5)
}
\arguments{
\item{x}{the input data frame or matrix}

\item{nrow}{number of rows to pull out}

\item{ncol}{number of columns to pull out}
}
\description{
This function pulls out the top-left corner of a data frame or matrix. This
is useful if the input is very tall and/or wide, such that printing the
entire object fills up the entire console, even with the nice printing
features of tibbles.
}
