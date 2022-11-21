### add_trimmed_weights_y2

#' Add weights to your data
#'
#' Use add_weights_y() to add a weight column to your data by taking the raked weights output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be weighted
#' @param raked_weights Raked weights output
#' @param upper_limit DEFAULT = 5, set an upper trim limit for your weights
#' @param lower_limit DEFAULT = .05, set a lower trim limit for your weights
#' @param trimmed_weights_name An unquoted name to give to your new trimmed weights variable
#' @export
#' @return A tibble of with weights
#' @examples
#' \dontrun{
#' municipal_data %>% 
#' add_trimmed_weights_y2(
#' raked_weights
#' )
#' }


add_trimmed_weights_y2 <- function(
  dataset,
  raked_weights,
  upper_limit = 5,
  lower_limit = .05,
  trimmed_weights_name
){


  variable_string <-
    deparse(
      substitute(
        trimmed_weights_name
        )
      )

  trimmedweights <-
    survey::trimWeights(
      raked_weights,
      upper = upper_limit,
      lower = lower_limit
    )

  dataset %>%
    dplyr::select(
      -tidyselect::all_of(variable_string)
    ) %>%
    dplyr::bind_cols(
      {{ trimmed_weights_name }} := stats::weights(
        trimmedweights
      )
    )

}
