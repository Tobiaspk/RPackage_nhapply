% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nhapply_functions.R
\name{mat_apply}
\alias{mat_apply}
\title{Apply Function with Optimised Functions}
\usage{
mat_apply(mat_open, FUN, weights = 1)
}
\arguments{
\item{mat_open}{is the input matrix. Every column should represent a
pixel-neighbourhood and every row one element of that neighbourhood.}

\item{FUN}{is a function, that is applied on the neighbourhood of each point
of a matrix. Optimised functions are: "mean", "sum", "max", "min",
"maxCount", "maxCountValue" (which value occurs most often). It is 
possible to add a customised function like function(x) {...} which must
return a single value.}

\item{weights}{is a vector of weights that is used when summing up a 
neighbourhood. Vector length must be the same as points in neighbourhood.}
}
\description{
Applies a function to each column of an open matrix, just like
    apply, with most common functions optimised. Output is a vector with the
     length equal to the number of columns of the input matrix.
}
\examples{
mat_inp <- matrix(sample(1:5, 10^6, rep = TRUE), nrow=10)
invisible(mat_apply(mat_inp, FUN = "maxcountvalue"))
}
