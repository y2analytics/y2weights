### create_weights_y2

#' Create a weights vector
#'
#' Use create_weights_y() to create a vector of weights by taking the survey design output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param svy_design The complex raked survey design object output from the rake_y2() function.
#' @export
#' @return A vector of weights constructed using your survey design object.
#' @examples
#' \dontrun{
#' municipal_data %>% 
#' mutate(
#' weights = create_weights_y2(svy_design)
#' )
#' }

create_weights_y2 <- function(
    svy_design
){
  
  # Create and return weights
  
  return(stats::weights(svy_design))

}