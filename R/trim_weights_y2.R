### trim_weights_y2

#' Create a trimmed weights vector
#'
#' Use trim_weights_y2() to create a vector of trimmed weights by taking the survey design output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param svy_design A complex raked survey design object (can be created using the rake_y2() function).
#' @param limit_type (default = 'percentile') The type of limits that will be used for the upper_limit/lower_limit arguments. Currently accepts 'percentile', 'standard deviations', or 'manual'.
#' @param upper_limit (default = 0.95) Set an upper trim limit for your weights (set to the 95th percentile of the untrimmed weights by default). If limit_type is set to 'standard deviations', limit will be the number of standard deviations above the mean to which the weight will be trimmed. If limit_type is set to 'manual', limit will be the upper numerical limit of the weight.
#' @param lower_limit (default = 0.05) Set a lower trim limit for your weights (set to the 5th percentile of the untrimmed weights by default). If limit_type is set to 'standard deviations', limit will be the number of standard deviations below the mean to which the weight will be trimmed. If limit_type is set to 'manual', limit will be the lower numerical limit of the weight.
#' @param strict (default = TRUE) The reapportionment of the ‘trimmings’ from the weights may sometimes push other weights over the limits. If strict = TRUE the function repeats the trimming iteratively to prevent this.
#' @export
#' @return A vector of trimmed weights constructed using the svy_design object
#' @examples
#' municipal_data %>%
#'   define_target_y2(
#'     s_sex,
#'     c(
#'       '1' = .49,
#'       '2' = .5,
#'       '3' = .01
#'     )
#'   )
#' svy_design <- municipal_data %>% 
#'   rake_y2(
#'     s_sex
#'   )
#'   
#' #' # Default method (percentile)
#' municipal_data %>% 
#'   dplyr::mutate(
#'     trimmed_weights = trim_weights_y2(svy_design)
#'   )
#' 
#' # Standard deviations method
#' municipal_data %>% 
#'   dplyr::mutate(
#'     trimmed_weights = trim_weights_y2(
#'       svy_design,
#'       limit_type = 'standard deviations',
#'       upper_limit = 3,
#'       lower_limit = -3
#'     )
#'   )
#' 
#' # Standard deviations method
#' municipal_data %>% 
#'   dplyr::mutate(
#'     trimmed_weights = trim_weights_y2(
#'       svy_design,
#'       limit_type = 'manual',
#'       upper_limit = 5,
#'       lower_limit = 0.5
#'     )
#'   )

trim_weights_y2 <- function(
    svy_design,
    limit_type = c('percentile', 'standard deviations', 'manual'),
    upper_limit = 0.95,
    lower_limit = 0.05,
    strict = TRUE
) {
  
  limit_type <- rlang::arg_match(limit_type)
  
  ## Create untrimmed weights for limit references
  weights <- stats::weights(svy_design)
  
  ## Set limits
  if (limit_type == 'percentile') {
    
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

  return(stats::weights(trimmed_svy_design))
}

