### trim_weights_y2

#' Create a trimmed weights vector
#'
#' Use trim_weights_y2() to create a vector of trimmed weights by taking the survey design output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param weights_schema A complex, raked, survey.design object (can be created using the rake_y2() function).
#' @param limit_method (default = 'standard deviations') The type of limits that will be used for the upper_limit/lower_limit arguments. Currently accepts 'standard deviations', 'percentile', or 'decimal'.
#' @param upper_limit (default = 3) Set an upper trim limit for your weights (the default of 3 SDs would trim only .3\% of the weights). If limit_method is set to 'percentile', limit will be the upper percentile at which the weight will be trimmed. If limit_method is set to 'decimal', limit will be the upper numerical limit of the weight.
#' @param lower_limit (default = -3) Set a lower trim limit for your weights (the default of 3 SDs would trim only .3\% of the weights). If limit_method is set to 'percentile', limit will be the lower percentile at which the weight will be trimmed. If limit_method is set to 'decimal', limit will be the lower numerical limit of the weight.
#' @param strict (default = TRUE) The reapportionment of the ‘trimmings’ from the weights may sometimes push other weights over the limits. If strict = TRUE the function repeats the trimming iteratively to prevent this.
#' @export
#' @return A vector of trimmed weights constructed using the weights_schema, a survey.design object
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
#' weights_schema <- municipal_data %>% 
#'   rake_y2(
#'     s_sex
#'   )
#'   
#' # Default method (Standard deviations)
#' municipal_data$trimmed_weights <- trim_weights_y2(weights_schema)
#' # OR
#' municipal_data %>% 
#'   dplyr::mutate(
#'     trimmed_weights = trim_weights_y2(
#'       weights_schema
#'     )
#'   )
#'   
#' # Percentile method
#' municipal_data %>% 
#'   dplyr::mutate(
#'     trimmed_weights = trim_weights_y2(
#'       weights_schema,
#'       limit_method = 'percentile',
#'       upper_limit = .975,
#'       lower_limit = .025
#'     )
#'   )
#'   
#' # Decimal method
#' municipal_data %>% 
#'   dplyr::mutate(
#'     trimmed_weights = trim_weights_y2(
#'       weights_schema,
#'       limit_method = 'decimal',
#'       upper_limit = 5,
#'       lower_limit = 0.5
#'     )
#'   )

trim_weights_y2 <- function(
    weights_schema,
    limit_method = c('standard deviations', 'percentile', 'decimal'),
    upper_limit = 3,
    lower_limit = -3,
    strict = TRUE
) {
  
  limit_method <- rlang::arg_match(limit_method)
  
  ## Create untrimmed weights for limit references
  weights <- stats::weights(weights_schema)
  
  ## Set limits
  if (limit_method == 'percentile') {
    upper <- stats::quantile(weights, upper_limit)
    lower <- stats::quantile(weights, lower_limit)
  } 
  
  if (limit_method == 'standard deviations') {
    upper <- stats::sd(weights) * upper_limit + 1
    lower <- stats::sd(weights) * lower_limit + 1
  } 
  
  if (limit_method == 'decimal') {
    upper <- upper_limit
    lower <- lower_limit
  }
  
  ## Create trimmed weights
  trimmed_weights_schema <-
    survey::trimWeights(
      weights_schema,
      upper = upper,
      lower = lower,
      strict = strict
    )

  return(stats::weights(trimmed_weights_schema))
}

