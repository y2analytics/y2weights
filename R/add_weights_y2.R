### add_weights_y2

#' Add weights to your data
#'
#' Use add_weights_y() to add a weight column to your data by taking the raked weights output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be weighted
#' @param raked_weights Raked weights output
#' @param weights_name An unquoted name to give to your new weights variable
#' @export
#' @return A tibble of with weights
#' @examples
#' \dontrun{
#' municipal_data %>% 
#' add_weights_y2(
#' raked_weights
#' )
#' }

add_weights_y2 <- function(
  dataset,
  raked_weights,
  weights_name
){

  variable_string <-
    deparse(
      substitute(
        weights_name
        )
      )

  dataset %>%
    dplyr::select(
      -tidyselect::all_of(
        variable_string
        )
    ) %>%
    dplyr::bind_cols(
      {{ weights_name }} := stats::weights(
        raked_weights
      )
    )

}
