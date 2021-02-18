#' Main Neighbour Apply Function
#' 
#' @description Apply functions on neighbourhood of pixels
#' @param mat is an input matrix to apply a function on
#' @param FUN is a function, that is applied on the neighbourhood of each point
#'     of a matrix. Optimised functions are: "mean", "sum", "max", "min",
#'     "maxCount", "maxCountValue" (which value occurs most often). It is 
#'     possible to add a customised function like function(x) {...} which must
#'     return a single value.
#' @param restriction restricts the input matrix according to some condition as
#'     string. For example nhapply(mat, sum, restriction = "mat > 9").
#' @param weights is a vector of weights that is used when summing up a 
#'     neighbourhood. Vector length must be the same as points in neighbourhood.
#' @param density if True, weights vector is scale to 0 - 1
#' @param neighb_type specifies the type of surrounding:
#'     Type 1 is square-shaped ("Moore-Neighbourhood")
#'     Type 2 is cross-shaped ("Von-Neumann-Neighbourhood")
#'     Type 3 is diamond-shape
#' @param width specifies how many neighbour pixels horizontally and vertically
#'     from center pixel should be included
#' @param include.own tells, if the value of the center pixel should be included
#' @param custom_neighb can be a matrix with 2 columns describing the x- and 
#'     y-coordinates of the pixels to use or it can be a square matrix with odd
#'     number of rows and columns describing the neighbourhood and weights of
#'     each neighbour-pixel (0 means not using!)
#' @param FUN_advanced can be used for more custom use-cases. The advanced 
#'     function is applied on an open matrix where each column represents a
#'     point in a matrix and the rows its neighbourhood. Output should be a
#'     vector of same length as the number of pixels in the input matrix.
#' @param transition if FALSE, matrix end at the sides, if TRUE, neighbourhood
#'     includes pixel from other side.
#' @param force if your custom function outputs a different number of values
#'     than the number of points of the input matrix, force will force the 
#'     program to apply the function. Do not use it, seriously, don't!
#' @export

nhapply <- function(mat,
                          FUN, restriction = "TRUE",
                          weights = 1, density = FALSE,
                          neighb_type = 1, width = 1, include.own = FALSE,
                          custom_neighb, FUN_advanced, transition = TRUE,
                          force = FALSE) {
  
  ## 0. organisation
  
  # check input matrix
  test_matrix2(mat)
  
  # check function input doppelte Funktion angegeben
  if (!missing(FUN) &&
      !missing(FUN_advanced)) {
    warning(warning2)
  }
  
  # restrict input matrix
  expr <- parse(text = paste0("!(", restriction, ")"))
  mat[eval(expr)] <- NA

  ## 1. generate neighb-coordinates -----------------------------------
  # generate default if no custom input
  if (missing(custom_neighb)) {
    neighb <- generate_coords_default(neighb_type = neighb_type,
                                      width = width,
                                      include.own = include.own)
    
  # generate coords and weights from matrix of neighbourhood
  } else if (ncol(custom_neighb) != 2) {
    temp <- generate_coords(custom_neighb)
    neighb <- temp$coords

    # use weights from matrix if not additional weights given
    if (missing(weights)) weights <- temp$weights
    else weights <- weights

  # use coordinates if input are x-y values of neighbourhood
  } else {
    neighb <- custom_neighb
  }
  
  # re-adjust weights
  if (density == TRUE) {
    if (any(weights < 0 || sum(weights) == 0)) {
      stop(stop14)
    }
    weights <- weights/sum(weights) 
  } 
  
  ## 2. generate matrix lags -----------------------------------
  lags <- generate_lags(neighb = neighb,
                        rows = nrow(mat),
                        cols = ncol(mat),
                        transition = transition)
  
  ## 3. open matrix to a 2 dimensional matrix with neighbours as rows -----
  mat_open <- open_matrix(mat = mat,
                          lags = lags)
  
  ## 4. apply function on columns (columns: pixels, rows: pixel-neighbourhood) -
  if (!missing(FUN_advanced)) {
    
    # check if function gives valid output
    res <- FUN_advanced(mat_open)

  } else if (is.character(FUN)) {
    res <- mat_apply(mat_open = mat_open, FUN = FUN, 
                     weights = weights)
  } else { 
    res <- mat_apply(mat_open = mat_open, FUN = match.call()$FUN, 
                     weights = weights)
  }
  
  ## 5. transform back to matrix
  out_mat <- matrix(res, ncol = ncol(mat))
  
  return(out_mat)
}
