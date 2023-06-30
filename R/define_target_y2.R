### define_target_y2

#' Create a table of target population parameters to be used for weighting data
#'
#' Use define_target_y2() to create a table of target population parameters to be used for weighting data. To avoid typos, define_target_y2() will automatically create an object in your R environment using the provided variable to create the name (e.g., "target_varname").
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe with variables to be used in weighting.
#' @param variable The variable for which you will be supplying population parameters.
#' @param target_params A named vector containing target parameters you wish your data to be weighted to. If the variable has a "MISSING" level, you do not need to include it in the target_params, it will be calculated automatically (essentially set to weight those respondents as 1, the mean). 
#' @export
#' @importFrom rlang .data
#' @importFrom data.table :=
#' @return A tibble with population parameters to the environment
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

define_target_y2 <- function(
    dataset,
    variable,
    target_params
) {
  
  variable_string <- deparse(substitute(variable))
  dataset_string <- deparse(substitute(dataset))
  
  if (!rlang::is_named(target_params)) {
    stop('target_params vector is unnamed; please add levels and proportions')
  }
  
  if (sum(stringr::str_detect(names(target_params), 'MISSING')) == 1) {
    stop('Do not supply target_params for "MISSING" values. These are calculated automatically and weighted to 1 on this parameter')
  }
  
  var_levels <- dataset %>% 
    dplyr::count({{ variable }}) %>% 
    dplyr::pull({{ variable }})
  
  ## Make the prop table when there are missing levels
  if ('MISSING' %in% var_levels) {
    
    if (NA %in% var_levels) {
      stop(
        stringr::str_c(
          'variable "',
          variable_string,
          '" in dataset "',
          dataset_string,
          '" contains NA values; please set NAs to "MISSING"'
        )
      )
    }
    
    target_table <- 
      tibble::tibble(
        {{ variable }} := 
          c(
            target_params %>% names(),
            'MISSING'
          ),
        prop = 
          c(
            target_params %>% unname() * nrow(dataset),
            (dataset %>% dplyr::filter({{ variable }} == 'MISSING') %>% nrow())
          ) 
      )
    
    ## Make prop table without the row for MISSING levels
  } else {
    
    if (NA %in% var_levels) {
      stop(
        stringr::str_c(
          'variable "',
          variable_string,
          '" in dataset "',
          dataset_string,
          '" contains NA values; please set NAs to "MISSING"'
        )
      )
    }
    
    target_table <- 
      tibble::tibble(
        {{ variable }} := 
          target_params %>% names(),
        prop = 
          target_params %>% unname() * nrow(dataset)
      ) 
    
  }
  
  ## Ensure that weight groups strings are the same as the data
  if (haven::is.labelled(dataset[[variable_string]])) {
    
    expected_inputs <- 
      dataset %>% 
      y2clerk::freqs(
        {{ variable }}
      ) %>% 
      dplyr::arrange(
        .data$value
      ) %>% 
      dplyr::pull(
        .data$value
      )
    
  } else {
    
    expected_inputs <- 
      dataset %>% 
      y2clerk::freqs(
        {{ variable }}
      ) %>% 
      dplyr::arrange(
        .data$label
      ) %>% 
      dplyr::pull(
        .data$label
      )
    
  }
  
  ## Ensure inputs are correct
  supplied_inputs <- 
    target_table %>% 
    dplyr::arrange(
      {{ variable }}
    ) %>% 
    dplyr::pull(
      {{ variable }}
    )
  
  only_in_supplied <- 
    supplied_inputs %>% 
    setdiff(
      expected_inputs
    )
  
  only_in_expected <- 
    expected_inputs %>% 
    setdiff(
      supplied_inputs
    )
  
  if (length(only_in_expected) > 0) {
    stop(
      stringr::str_c(
        'dataset levels of [',
        stringr::str_flatten(
          only_in_expected, 
          collapse = ', '
        ),
        '] are not found in target_params'
      )
    )
  }
  
  if (length(only_in_supplied) > 0) {
    stop(
      stringr::str_c(
        'Supplied target_params [',
        stringr::str_flatten(
          only_in_supplied, 
          collapse = ', '
        ),
        '] are not found in the dataset'
      )
    )
  }
  
  ## Check if the target_params add up to 100%
  if (round(sum(target_params), 2) != 1.00) {
    warning(
      stringr::str_c(
        'Provided total weighting proportions is not 1. ',
        'The sum of the supplied proportions is ',
        sum(target_params)
      )
    )
  }
  
  ## Send to environment
  target_list <- list(target_table)
  names(target_list) <- stringr::str_c(
    'target_',
    variable_string
  )
  list2env(
    x = target_list,
    envir = as.environment(1)
  )
}

