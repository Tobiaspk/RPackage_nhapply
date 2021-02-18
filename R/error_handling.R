### Stop ----------------------------------------------------

stop1 <- "Advanced Functions should (must) be applied column-wise! Each column 
is a pixel-neighbourhood"

stop2 <- "Columns have different classes, which may cause mistakes. 
Use matrix as input to avoid mistakes"

stop3 <- "Use matrix as input to avoid mistakes!"

stop4 <- "An error occured. Make sure your function works on matrices."

stop5 <- "Cannot evaluate maximum/minimum of non-integers and changing class
to numeric failed Make sure that the input is numeric if you want to find the maximum/minimum of the neigbourhood."

stop6 <- "Input is of type numeric. To find minimum/maximum make sure to have
a matrix of class numeric."

stop7 <- "The inputed default neighbourhood is not known. Please choose one
of the following 3: 
1 - 'Moore'
2 - 'VonNeumann'
3 - 'Diamond'"

stop8 <- "Input weights must be of type vector."

stop9 <- "Weights-vector is not same size as neighbourhood size.
\nRequired length: %s"

stop10 <- "Input matrix must be numeric and and have odd number of rows and 
columns"

stop11 <- "Input of parameter 'neighb' must be a numeric 2-dim matrix with two
columns, representing x and y coordinates respectively."

stop12 <- "Input 'neighb' can not have NA values!"

stop13 <- "An error occured! Make sure your function returns only one value,
when input is a vector!"

stop14 <- "Densities must bigger or equal to 0. Results will not be
representative!"

### Warnings ----------------------------------------------------

warning1 <- "Use matrix as input to avoid mistakes!"

warning2 <- "Both FUN_advanced and FUN was specified. Only FUN_advanced will
be used."

warning3 <- "" # still free

warning4 <- "A warning occured. Make sure your function works without problems 
on matrices."

warning5 <- "For parameter count.output choose between 'count' and 'value'
to either receive the most common neighbour-value or count of
most common neighbour"

warning6 <- "For parameter sub.FUN choose between 'max'/'pmax' and 'min'/'pmin'
to receive count or value of least common neighbour"

warning7 <- "Input param 'neighb' should be type matrix to avoid possible 
errors"

### Test procedures ----------------------------------------------------

# Test if advanced function works on open matrices and outputs right format
test_matrix1 <- function(FUN_test, ...) {

  test_matrix_temp <- matrix(sample(1:60), ncol = 10)
  res <- tryCatch(FUN_test(test_matrix_temp, ...),
                  warning = function(w) warning(paste(warning4, "\n", w)),
                  error = function(e) stop(paste(stop4, "\n", e)))
  
  if (is.vector(res) && length(res) != ncol(test_matrix_temp)) {
    stop(stop1)
  } 
}

# Test if matrix has correct type
test_matrix2 <- function(mat) {
  if (is.data.frame(mat)) {
    if (length(unique(sapply(mat, class))) != 1) {
      stop(stop2)
    }
    warning(warning1)
    mat <- as.matrix(mat)
  } else if (length(dim(mat)) != 2) {
    stop(stop3)
  }
}

# Test if custom function works on columns of open matrix
test_matrix3 <- function(mat_test, FUN_test) {

  res <- tryCatch(apply(mat_test, 2, FUN_test),
           warning = function(w) warning(paste(stop13, "\n", w)),
           error = function(e) stop(paste(stop13, "\n", e)))

  if (is.list(res) | !is.vector(res)) {
    stop(stop13)
  } 
}

