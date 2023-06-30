### evaluate_weights_y2

#' Compare weighted and unweighted data vs the ideal
#'
#' Use add_weights_y() to add a weight column to your data by taking the raked weights output from rake_y2() and passing it to this function.
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be weighted
#' @param ... List of variables to check
#' @param weight_var Weight variable
#' @param nas DEFAULT = TRUE, Remove NA's from calculations
#' @param remove_missing DEFAULT = FALSE, Before running frequencies, filter out responses that are "MISSING"
#' @export
#' @return A tibble of compared weighting schema
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
#' municipal_data %>% 
#'   evaluate_weights_y2(
#'     s_sex,
#'     weight_var = weights
#'   )

evaluate_weights_y2 <- function(
    dataset,
    ...,
    weight_var,
    nas = TRUE,
    remove_missing = FALSE
) {
  
  if (missing(weight_var)) {
    stop('Argument "weight_var" is missing')
  }
  
  variables <-
    dataset %>%
    dplyr::select(
      ...
    ) %>%
    names()
  
  purrr::map(
    variables,
    ~combine_data(
      w_variable = .x,
      nas = nas,
      dataset = dataset,
      weight = {{ weight_var }},
      remove_missing = remove_missing
    )
  ) %>% 
    purrr::list_rbind()
  
}


# Private functions -------------------------------------------------------

combine_data <- function(
    w_variable,
    nas,
    dataset,
    weight,
    remove_missing
) {
  
  ## Unweighted freqs
  if (remove_missing == TRUE) {
    # Filter out "MISSING" values
    unweighted <-
      dataset %>%
      dplyr::filter(
        .data[[w_variable]] != 'MISSING'
      ) %>% 
      y2clerk::freq(
        .data[[w_variable]],
        nas = nas
      ) %>%
      dplyr::select(
        .data$variable,
        .data$label,
        unweighted_result = .data$result
      )
  } else {
    # Keep "MISSING" values
    unweighted <-
      dataset %>%
      y2clerk::freq(
        .data[[w_variable]],
        nas = nas
      ) %>%
      dplyr::select(
        .data$variable,
        .data$label,
        unweighted_result = .data$result
      )
  }
  
  ## Weighted freqs
  if (remove_missing == TRUE) {
    # Filter out "MISSING" values
    weighted <-
      dataset %>%
      dplyr::filter(
        .data[[w_variable]] != 'MISSING'
      ) %>% 
      y2clerk::freq(
        .data[[w_variable]],
        wt = {{ weight }},
        nas = nas
      ) %>%
      dplyr::select(
        .data$variable,
        .data$label,
        weighted_result = .data$result
      )
  } else {
    # Keep "MISSING" values
    weighted <-
      dataset %>%
      y2clerk::freq(
        .data[[w_variable]],
        wt = {{ weight }},
        nas = nas
      ) %>%
      dplyr::select(
        .data$variable,
        .data$label,
        weighted_result = .data$result
      )
  }
  
  ## Expected results
  n <- nrow(dataset)
  expected <-
    eval(
      as.symbol(
        stringr::str_c(
          'target_',
          w_variable
        )
      )
    ) %>%
    dplyr::mutate(
      variable = w_variable,
      target_result = round(.data$prop / n, 2)
    ) %>%
    dplyr::select(
      .data$variable,
      label = 1,
      .data$target_result
    )
  
  ## Final combined output
  unweighted %>%
    dplyr::left_join(
      expected,
      by = c('variable', 'label')
    ) %>%
    dplyr::left_join(
      weighted,
      by = c('variable', 'label')
    ) %>%
    dplyr:: mutate(
      movement_amount = round(
        .data$weighted_result - .data$unweighted_result,
        2
      ),
      miss_amount = round(
        .data$weighted_result - .data$target_result, 
        2
      )
    )
}