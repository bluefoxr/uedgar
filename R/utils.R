#' Shrink process codes
#'
#' Shortens EDGAR process codes to a specified depth
#'
#' @param codes A vector of process codes, like "ENE.AEL.CRU.GT0.000"
#' @param depth Depth to shrink to (value between 1 and 5)
#'
#' @return Vector of length `codes` but shrunk
#' @export
shrink_process_codes <- function(codes, depth = 3){

  stopifnot(depth %in% 1:5)

  current_depth <- proc_code_depth(codes)

  if(depth > current_depth){
    stop("Current process code depth is less than required depth - no can do.", call. = FALSE)
  }

  # shrink
  n_remove <- (current_depth - depth)*4

  # at this point, all codes should have same length
  shrunk_codes <- substr(codes, 1, nchar(codes[1]) - n_remove)

  # check
  if(proc_code_depth(shrunk_codes) != depth){
    stop("Something went wrong when shrinking process codes...", call. = FALSE)
  }

  shrunk_codes

}

#' Get depth of process code
#'
#' Checks the format of a vector of EDGAR process codes, ensures they are valid,
#' and returns the depth (value between 1 and 5), or an error if codes are not
#' in the expected format.
#'
#' @param codes Vector of codes to test.
#'
#' @return Value between 1 and 5
#' @export
proc_code_depth <- function(codes){

  stopifnot(is.character(codes))

  valid_lengths <- c(3,7,11,15,19)

  # Check lengths -----------------------------------------------------------

  code_lengths <- nchar(codes)
  bad_lengths <- codes[code_lengths %nin% valid_lengths]

  if(length(bad_lengths) > 0){
    stop("One or more codes with unexpected lengths: ", toString(head(bad_lengths)), call. = FALSE)
  }

  # Check depth ------------------------------------------------------------

  # these match patterns like XXX.XXX etc, where X can be any character
  depth_regs <- c(
    "^...$",
    "^...\\....$",
    "^...\\....\\....$",
    "^...\\....\\....\\....$",
    "^...\\....\\....\\....\\....$"
  )

  # checks whether all strings in vector are at specified depth
  is_at_depth <- function(codes, depth) all(grepl(depth_regs[depth], codes))

  for(depth in 5:1){
    if(is_at_depth(codes, depth)){
      return(depth)
    }
  }

  stop("Codes do not follow expected format.", call. = FALSE)

}

# Not in operator
#
# For convenience, rather than always `!(x, %in% y)`
#
# @param x A scalar or vector
# @param y A scalar or vector
#
# @return TRUE if x is not in y, FALSE otherwise
'%nin%' <- function(x,y){
  !('%in%'(x,y))
}
