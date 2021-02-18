#' Matrix of Neighbourhood to Coordinates
#'
#' @description The function takes an odd 2D matrix as input. Non zero fields
#'     are used and value saved. Output is a list of a coordinates matrix and a 
#'     weights matrix.
#' @param mat input is type matrix with odd no. of rows/columns, element in 
#'     middle is pixel.
#' @export
#' @examples 
#' co <- generate_coords(matrix(sample(0:1, 9, rep = TRUE), ncol = 3))

generate_coords <- function(mat) {
  if ((!is.numeric(mat) | 
       ncol(mat)%%2 != 1 |
       nrow(mat)%%2 != 1)) stop(stop10)
  
  # generate coordinates from col and row information
  neighb <- t(matrix(c(col(mat), row(mat))[mat!=0], ncol = 2))
  # center
  neighb <- t(neighb - c(nrow(mat)/2 + .5, ncol(mat)/2 + .5))
  # correct y coordinates
  neighb[, 2] <- - neighb[, 2]
  weights <- c(mat)[mat!=0]
  
  return(list(coords = neighb, weights = weights))
}

#' Coordinates of Default Pixel-Neighbourhoods
#'
#' A Function to generate built-in Pixel-Neighbourhood of custom size
#' @inheritParams nhapply
#' @export
#' @examples
#' co <- generate_coords_default(neighb_type = "moore", width = 2)

generate_coords_default <- function(neighb_type = 1, width = 1,
                                    include.own = FALSE) {
  
  # dimension of pixel neighbourhood
  total_width = 2 * width + 1
  inputs <- c("1", "2", "3",
              "Moore", "moore", "Moore-Neighbourhood",
              "VonNeumann", "neumann", "vonneumann",
              "Von-Neumann-Neighbourhood",
              "Diamond", "diamond")
  
  # create type 1 Moore neighbourhood
  if (!as.character(neighb_type) %in% inputs) {
    stop(stop7)
    
  } else {
    # initialise neighbourhood
    neighb <- matrix(c(rep(-width : width, each = total_width),
                       rep(width : -width, total_width)),
                     ncol = 2, byrow = FALSE)
  }
  
  # Von Neumann: eliminate fields not used for Von Neumann neighbourhood
  if (any(grepl("eumann", neighb_type)) | neighb_type == 2) {
    neighb <- neighb[!(neighb[, 1] != 0 & neighb[, 2] != 0),]
  }
  
  # Diamond: eliminate fields not used for diamond neighbourhood
  if (any(grepl("iamond", neighb_type)) | neighb_type == 3) {
    neighb <- neighb[rowSums(abs(neighb)) <= width, ]
  }
  
  # eliminate own coordinate
  if (!include.own) {
    neighb <- neighb[rowSums(abs(neighb)) != 0,, drop = FALSE]
  }
  
  return (neighb)
}


#' Generate Matrix Lags
#'
#' @description This function takes coordinates (2-col matrices) as inputs and 
#'     generates a list with the dimensions "rows" and "cols" being indices
#'     on how to lag a matrix to achieve desired output.
#' @param neighb is a matrix with two columns each representing the x-axis and
#'     y-axis of a pixel in the pixel neighbourhood.
#' @param rows,cols describes how many columns/rows the matrix has.
#' @param transition this parameter describes, if the matrix should continue
#'     at the other side of the screen if ending at one.
#' @export
#' @examples 
#' l <- generate_lags(generate_coords_default(2, 2), 10, 10, FALSE)

generate_lags <- function(neighb, rows, cols, transition = TRUE) {
  
  # check and correct input
  if (ncol(neighb) != 2) stop(stop11)
  if (sum(is.na(neighb)) != 0) stop(stop12)
  if (!is.matrix(neighb)) {
    
    if (is.data.frame(neighb) && 
        ncol (neighb) == 2 &&
        all(sapply(neighb, class) == "numeric")) {
    warning(warning7)
      neighb <- as.matrix(neighb)
      
    } else {
      stop(stop11)
    }
  }
  
  # create list for output
  lags <- list()
  
  if (transition == TRUE) {
    row_default <- c(rep(1:rows, 3))
    col_default <- c(rep(1:cols, 3))
  } else {
    row_default <- c(rep(NA, rows), 1:rows, rep(NA, rows))
    col_default <- c(rep(NA, cols), 1:cols, rep(NA, cols))
  }
  
  for (i in 1:nrow(neighb)) {
    x <- neighb[i, 1]
    y <- - neighb[i, 2]

    lags$rows[[i]] <- row_default[(y + rows + 1):(y + 2 * rows)]
    lags$cols[[i]] <- col_default[(x + cols + 1):(x + 2 * cols)]
  }
  
  return(lags)
}

#' Open matrix
#'
#' @description This function takes a matrix and a list of lagged rows/columns 
#'     as inputs and creates a 2-dimensional matrix with as many rows as objects
#'      in the neighbourhood.
#' @param mat is the matrix to open and must be 2-dimensional.
#' @export
#' @param lags is a list of coordinates describing the pixel neighbourhoods.


open_matrix <- function(mat, lags) {
  
  n <- length(lags$rows)
  mat_temp <- NULL
  
  for (i in 1:n) {
    mat_temp <- c(mat_temp, mat[lags$rows[[i]], lags$cols[[i]]])
  }

  return(matrix(mat_temp, nrow = n, byrow = TRUE))
  
}
