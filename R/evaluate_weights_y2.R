### evaluate_weights

#' Evaluate the weights created for your data
#'
#' Use add_weights_y() to add a weight column to your data by taking the raked weights output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be weighted
#' @param ... List of variables to check
#' @param weight_var Weight variable
#' @param nas DEFAULT = TRUE, remove NA's from calculations
#' @param large_miss_limit DEFAULT = .05, set to different level to quickly see large discrepancies between weighted frequencies and target frequencies
#' @param large_movement_limit DEFAULT = .1, set to different level to quickly see where weighting caused large shifts from the unweighted data 
#' @export
#' @return A tibble of compared weighting schema
#' @examples
#' \dontrun{
#' municipal_data %>% 
#' evaluate_weights(
#' w_sex,
#' w_race,
#' weight_var = weights
#' )
#' }

evaluate_weights <- function(
  dataset,
  ...,
  weight_var,
  nas = TRUE,
  large_miss_limit = .05,
  large_movement_limit = .1 # add new argument to change MISSING to NA
){

  variables <-
    dataset %>%
    dplyr::select(
      ...
    ) %>%
    names

  purrr::map_dfr(
    variables,
    ~combine_data(
      w_variable = .x,
      keep_nas = nas,
      dataset = dataset,
      large_miss_limit = large_miss_limit,
      large_movement_limit = large_movement_limit,
      weight = {{ weight_var }}
    )
  )

}


combine_data <- function(
    w_variable,
    keep_nas,
    dataset,
    large_miss_limit,
    large_movement_limit,
    weight
){

  unweighted <-
    dataset %>%
    y2clerk::freq(
      .data[[w_variable]],
      nas = keep_nas
    ) %>%
    dplyr::select(
      .data$variable,
      .data$label,
      unweighted_result = .data$result
    )

  weighted <-
    dataset %>%
    y2clerk::freq(
      .data[[w_variable]],
      wt = {{ weight }},
      nas = keep_nas
    ) %>%
    dplyr::select(
      .data$variable,
      .data$label,
      weighted_result = .data$result
    )

  n <-
    dataset %>%
    dplyr::count() %>%
    dplyr::pull(
      .data$n
      )

  expected <-
    eval(
      as.symbol(
        stringr::str_c(
          w_variable,
          '_pps'
        )
      )
    ) %>%
    dplyr::mutate(
      variable = w_variable,
      target_result = round(.data$prop / .data$n, 2)
    ) %>%
    dplyr::select(
      .data$variable,
      label = .data[[w_variable]],
      .data$target_result
    )

  unweighted %>%
    dplyr::left_join(
      weighted,
      by = c("variable", "label")
    ) %>%
    dplyr::left_join(
      expected,
      by = c("variable", "label")
    ) %>%
    dplyr:: mutate(
      miss_amount = round(.data$weighted_result - .data$target_result, 2),
      large_miss = dplyr::case_when(
        abs(.data$miss_amount) >= .data$large_miss_limit ~ '*',
        TRUE ~ NA_character_
        ),
      movement_amount = round(
        .data$weighted_result - .data$unweighted_result,
        2
      ),
      large_movement = dplyr::case_when(
        abs(.data$movement_amount) >= .data$large_movement_limit ~ '*',
        TRUE ~ NA_character_
      ),
    )

}