### evaluate_weights_y2

#' Compare weighted and unweighted data vs the ideal
#'
#' Use evaluate_weights_y2() to compare given variables on: 1) unweighted data, 2) weighted data, 3) target weighting goals, 4) movement after weighting, and 5) difference from the target.
#'
#' @keywords freqs weights target parameters
#' @param dataset A dataframe to be weighted
#' @param ... List of variables to check
#' @param weight_var Weight variable
#' @param nas DEFAULT = TRUE, Remove NAs from calculations
#' @param remove_missing DEFAULT = FALSE, Before running frequencies, filter out responses that are "MISSING"
#' @export
#' @return A tibble with the following columns: variable, label, result_unweighted, target, result_weighted, movement, diff_from_target
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
#'   
#' weights_schema <- municipal_data %>% 
#'   rake_y2(
#'     s_sex
#'   )
#'   
#' municipal_data$trimmed_weights <- trim_weights_y2(weights_schema)
#'   
#' municipal_data %>% 
#'   evaluate_weights_y2(
#'     s_sex,
#'     weight_var = trimmed_weights
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
  
  results <- purrr::map(
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
  
  ## Final re-ordering if results contain weighting and non-weighting vars and non-weighting vars are first
  if ('target' %in% colnames(results)) {
    
    results <- results %>% 
      dplyr::select(
        'variable',
        'label',
        'result_unweighted',
        'target',
        'result_weighted',
        'movement',
        'diff_from_target'
      )
    
  }
  
  return(results)
  
}


# Private functions -------------------------------------------------------

combine_data <- function(
    w_variable,
    nas,
    dataset,
    weight,
    remove_missing
) {
  
  w_var_quoed <- rlang::sym(w_variable)
  ## Unweighted freqs
  if (remove_missing == TRUE) {
    # Filter out "MISSING" values
    unweighted <-
      dataset %>%
      dplyr::filter(
        .data[[w_variable]] != 'MISSING'
      ) %>% 
      y2clerk::freq(
        !!w_var_quoed,
        nas = nas
      ) %>%
      dplyr::select(
        'variable',
        'label',
        result_unweighted = 'result'
      )
  } else {
    # Keep "MISSING" values
    unweighted <-
      dataset %>%
      y2clerk::freq(
        !!w_var_quoed,
        nas = nas
      ) %>%
      dplyr::select(
        'variable',
        'label',
        result_unweighted = 'result'
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
        !!w_var_quoed,
        wt = {{ weight }},
        nas = nas
      ) %>%
      dplyr::select(
        'variable',
        'label',
        result_weighted = 'result'
      )
  } else {
    # Keep "MISSING" values
    weighted <-
      dataset %>%
      y2clerk::freq(
        !!w_var_quoed,
        wt = {{ weight }},
        nas = nas
      ) %>%
      dplyr::select(
        'variable',
        'label',
        result_weighted = 'result'
      )
  }
  
  ## Creates expected results only for weighting vars
  if (exists(stringr::str_c('target_', w_variable))) {
    
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
        target = round(.data$prop / n, 2)
      ) %>%
      dplyr::select(
        'variable',
        label = 1,
        'target'
      )
    
  }
  
  ## Final combined output
  if (exists(stringr::str_c('target_', w_variable))) {
    
    # For weighting vars
    results <- unweighted %>%
      dplyr::left_join(
        expected,
        by = c('variable', 'label')
      ) %>%
      dplyr::left_join(
        weighted,
        by = c('variable', 'label')
      ) %>%
      dplyr:: mutate(
        movement = round(
          .data$result_weighted - .data$result_unweighted,
          2
        ),
        diff_from_target = round(
          .data$result_weighted - .data$target, 
          2
        )
      )
    
  } else {
    
    # For non-weighting vars
    results <- unweighted %>%
      dplyr::left_join(
        weighted,
        by = c('variable', 'label')
      ) %>%
      dplyr:: mutate(
        movement = round(
          .data$result_weighted - .data$result_unweighted,
          2
        )
      )
    
  }
  
  return(results)
  
}