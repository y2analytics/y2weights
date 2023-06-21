### create_trimmed_weights_y2

#' Create a trimmed weights vector
#'
#' Use create_trimmed_weights_y() to create a vector of trimmed weights by taking the survey design output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param svy_design A complex raked survey design object (can be created using the rake_y2() function).
#' #' @param limit_type (default = 'percentiles') The type of limits that will be used in creating limits. Currently accepts 'percentiles', 'standard deviations', or 'manual'.
#' @param upper_limit (default = 0.95) Set an upper trim limit for your weights (set to the 95th percentile of the untrimmed weights by default). If limit_type is set to 'standard deviations', limit will be the number of standard deviations above the mean to which the weight will be trimmed. If limit_type is set to 'manual', limit will be the upper numerical limit of the weight.
#' @param lower_limit (default = 0.05) set a lower trim limit for your weights (set to the 5th percentile of the untrimmed weights by default). If limit_type is set to 'standard deviations', limit will be the number of standard deviations below the mean to which the weight will be trimmed. If limit_type is set to 'manual', limit will be the lower numerical limit of the weight.
#' @param strict (default = TRUE) The reapportionment of the ‘trimmings’ from the weights can push other weights over the limits. If strict=TRUE the function repeats the trimming iteratively to prevent this.
#' @export
#' @return A vector of trimmed weights constructed using your survey design object.
#' @examples
#' \dontrun{
#' 
#' # Default method (percentiles)
#' municipal_data %>% 
#' mutate(
#' trimmed_weights = create_trimmed_weights_y2(svy_design)
#' )
#' }
#' 
#' # Standard deviations method
#' municipal_data %>% 
#' mutate(
#' trimmed_weights = create_trimmed_weights_y2(
#' svy_design,
#' limit_type = 'standard deviations,
#' upper_limit = 3,
#' lower_limit = -3
#' )
#' )
#' }
#' 
#' # Standard deviations method
#' municipal_data %>% 
#' mutate(
#' trimmed_weights = create_trimmed_weights_y2(
#' svy_design,
#' limit_type = 'manual,
#' upper_limit = 5,
#' lower_limit = 0.5
#' )
#' )
#' }

create_trimmed_weights_y2 <- function(
    svy_design,
    limit_type = c('percentiles', 'standard deviations', 'manual'),
    upper_limit = 0.95,
    lower_limit = 0.05,
    strict = TRUE
){
  
  ## Get limit_type
  limit_type <- rlang::arg_match(limit_type)
  
  ## Create normal weights for limit references
  
  weights <- stats::weights(svy_design)
  
  ## Set limits
  
  if (limit_type == 'percentiles') {
    
    # Percentile limits
    
    upper <- stats::quantile(weights, upper_limit)
    
    lower <- stats::quantile(weights, lower_limit)
    
  } 
  
  if (limit_type == 'standard deviations') {
    
    # Num. SDs limits
    
    upper <- stats::sd(weights) * upper_limit + 1
    
    lower <- stats::sd(weights) * lower_limit + 1
    
  } 
  
  if (limit_type == 'manual') {
    
    # Manual limits
    
    upper <- upper_limit
    
    lower <- lower_limit
    
  }
  
  ## Create trimmed weights
  
  trimmed_svy_design <-
    survey::trimWeights(
      svy_design,
      upper = upper,
      lower = lower,
      strict = strict
    )

  ## Return
  
  return(stats::weights(trimmed_svy_design))

}