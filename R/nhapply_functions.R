#' Apply Function with Optimised Functions
#'
#' @description Applies a function to each column of an open matrix, just like
#'     apply, with most common functions optimised. Output is a vector with the
#'      length equal to the number of columns of the input matrix.
#' @param mat_open is the input matrix. Every column should represent a
#'     pixel-neighbourhood and every row one element of that neighbourhood.
#' @inheritParams nhapply     
#' @export
#' @examples
#' mat_inp <- matrix(sample(1:5, 10^6, rep = TRUE), nrow=10)
#' invisible(mat_apply(mat_inp, FUN = "maxcountvalue"))


mat_apply <- function(mat_open, FUN, weights = 1) {
  
  # list of optimised functions
  optimised_functions <- c("max", "min", "which.max", "which.min",
                           "mean", "sum", "count", "maxcount", 
                           "maxcountvalue", "mincount", "mincountvalue")

  if (is.character(FUN)) fun_call <- FUN
  else if (is.name(FUN)) fun_call <- as.character(FUN)
  else if (is.name(match.call()$FUN)) fun_call <- as.character(match.call()$FUN)
  else fun_call <- "mustBeFunction"
  
  # optimised function
  if (is.character(fun_call) && fun_call %in% optimised_functions) {
    # cat("Optimised Procedure!\n")
    # reassign input to optimised function
    switch(fun_call,
           # extremes
           max = extreme(mat_open, sub.FUN = "max"),
           min = extreme(mat_open, sub.FUN = "min"),
           which.max = return(max.col(t(mat_open))),
           which.min = return(max.col(t(-mat_open))),
           
           # sums
           sum = w_sums(mat_open, weights = weights),
           mean = colMeans(mat_open, na.rm = TRUE),
           
           # counts
           count = count_(mat_open, sub.FUN = "max", count.output = "count"),
           maxcount = count_(mat_open, sub.FUN = "max", count.output = "count"),
           maxcountvalue = count_(mat_open, sub.FUN = "max", count.output = "value"),
           mincount = count_(mat_open, sub.FUN = "min", count.output = "count"),
           mincountvalue = count_(mat_open, sub.FUN = "min", count.output = "value")
           )
    
    # casual function with apply
    } else {
      if (is.call(FUN)) {
        test_matrix3(mat_open[, 1:5], eval(FUN))
        apply(mat_open, 2, FUN = eval(FUN))
      } else {
        test_matrix3(mat_open[, 1:5], FUN)
        apply(mat_open, 2, FUN = FUN)
        
      }
    }
}


#' Maximum of Columns
#'
#' @description The function outputs a vector of the max/min values of each column
#' @param mat_open is a 2 dimensional input matrix
#' @param sub.FUN describes, if the max or min value should be calculated
#' @export

extreme <- function(mat_open, sub.FUN = "max") {

  # check if input is numeric or can be transformed to numeric
  if (!is.numeric(mat_open) & !is.factor(mat_open)) {
    tryCatch(mat_open <- as.numeric(mat_open),
             warning = function(w) stop(stop5))
  } else if (is.factor(mat_open)) {
    stop(stop6)
  }
  
  nr <- nrow(mat_open)
  out <- mat_open[1, ]

  if (sub.FUN=="max") for (i in 2:nr) out <-pmax(out, mat_open[i, ], na.rm=TRUE)
  if (sub.FUN=="min") for (i in 2:nr) out <-pmin(out, mat_open[i, ], na.rm=TRUE)

  return(out)
}


#' Count of Most Common Neighbour
#'
#' @description The function outputs a vector of the number of times, the most
#'     common neighbour occurs
#' @param mat_open is a 2 dimensional input matrix
#' @param sub.FUN describes, if the max or min count should be calculated
#' @param count.output specifies if the output should be the value of the
#'     max or min count, or the count itself (number of occurrences)
#' @export

count_ <- function(mat_open, sub.FUN = "max", count.output = "value") {
  
  # check if inputs are correct
  if(!count.output %in% c("value", "count")) warning(warning5)
  if(!sub.FUN %in% c("max", "min")) warning(warning6)
  
  # colSum of values equal i
  sum_i <- function(i) colSums(mat_open == values[i], na.rm = TRUE)
  # pmin, pmax functions
  if (sub.FUN=="max") f_min_max <- function(i) pmax(cSum, sum_i(i), na.rm=TRUE)
  if (sub.FUN=="min") f_min_max <- function(i) pmax(cSum, sum_i(i), na.rm=TRUE)
  
  
  # find set of unique values, na excluded
  values <- unique(c(mat_open))
  values <- values[!is.na(values)]
  
  cSum <- sum_i(1)
  for (i in 2:(length(values))) cSum <- f_min_max(i)

  # return value or count
  if (count.output == "count") return(cSum)
  if (count.output == "value") {
    val <- rep(NA, ncol(mat_open))
    for (i in 1:(length(values))) val[sum_i(i) == cSum] <- values[i]
    return(val)
  }
}


#' Sum of Neighbourhood
#'
#' @description The function outputs a the weighted col-sums as a vector.
#' @param mat_open is a 2 dimensional input matrix
#' @inheritParams mat_apply
#' @export


w_sums <- function(mat_open, weights = 1) {
  # check if weights-input has correct class
  mat_open[is.na(mat_open)] <- 0
  
  if (!is.vector(weights)) stop(stop8)
  
  # simple colSum
  if (length(weights) == 1) return(colSums(mat_open, na.rm = TRUE))
  
  # weighted sum
  if (length(weights) == nrow(mat_open)) return(c(weights %*% mat_open))
  
  # stop if length of weights is not correct
  stop(gettextf(stop9, nrow(mat_open)))
}






