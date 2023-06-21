### create_prop_table_y2

#' Create a table of population parameters to be used for weighting data
#'
#' Use create_prop_table_y2() to create nicely formatted population parameter tables to be used for weighting data. To avoid typos, create_prop_table_y2() will automatically create an object in your R environment using the provided variable to create the name. This function is designed to work within the y2weights framework.
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe with variables to be used in weighting.
#' @param variable The variable for which you will be supplying population parameters.
#' @param population_params A named vector containing population parameters.
#' @export
#' @importFrom rlang .data
#' @importFrom data.table :=
#' @return A tibble of with population parameters to the environment
#' @examples
#' municipal_data %>% 
#' create_prop_table_y2(
#' s_sex,
#' c(
#' 'Male' = .49,
#' 'Female' = .5,
#' 'In another way, specify if you wish' = .01
#' )
#' )

create_prop_table_y2 <- function(
    dataset,
    variable,
    population_params
){
  
  ## Get the variable as a string
  variable_string <- 
    deparse(
      substitute(
        variable
      )
    )
  
  ## Error if population_params is unnamed
  
  if (!rlang::is_named(population_params)){
    
    stop('Population params vector is unnamed; please add levels and proportions')
    
  }
  
  ## Get var_levels for control flow
  var_levels <- dataset %>% 
    dplyr::count({{ variable }}) %>% 
    dplyr::pull({{ variable }})
  
  ## Make the prop table when there are missing levels
  if ('MISSING' %in% var_levels) {
    
    # Throws error if variable still has missing values
    if (NA %in% var_levels) {
      
      stop(
        stringr::str_c(
          'variable ',
          variable_string,
          ' contains NA values; please set to "MISSING"'
        )
      )
      
    }
    
    # Table
    prop_table <- 
      tibble::tibble(
        {{ variable }} := 
          c(
            population_params %>% names(),
            'MISSING'
          ),
        prop = 
          c(
            population_params %>% unname() * nrow(dataset),
            (dataset %>% dplyr::filter({{ variable }} == 'MISSING') %>% nrow())
          ) 
      )
    
    ## Make prop table without the row for MISSING levels
  } else {
    
    # Throws error if variable has missing values
    if (NA %in% var_levels) {
      
      stop(
        stringr::str_c(
          'variable ',
          variable_string,
          ' contains NA values; please set to "MISSING"'
        )
      )
      
    }
    
    # Table
    prop_table <- 
      tibble::tibble(
        {{ variable }} := 
          population_params %>% names(),
        prop = 
          population_params %>% unname() * nrow(dataset)
      ) 
    
  }
  
  ## Ensure that weight groups strings are the same as the data
  
  if (haven::is.labelled(dataset[[variable_string]])) {
    
    # For haven labelled variables
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
    
    # All else
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
    prop_table %>% 
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
        'Expected inputs [',
        stringr::str_flatten(
          only_in_expected, 
          collapse = ', '
        ),
        '] are not found in supplied inputs.'
      )
    )
    
  }
  
  if (length(only_in_supplied) > 0) {
    
    stop(
      stringr::str_c(
        'Supplied inputs [',
        stringr::str_flatten(
          only_in_supplied, 
          collapse = ', '
        ),
        '] are not found in expected inputs.'
      )
    )
    
  }
  
  ## Check if the population_params add up to 100%
  if (sum(population_params) != 1) {
    
    warning(
      stringr::str_c(
        'Provided total weighting proportions is not 1. ',
        'The sum of the supplied proportions is ',
        sum(population_params)
      )
    )
    
  }
  
  ## Send to environment
  
  prop_table_name <- 
    stringr::str_c(
      variable_string,
      '_prop_table'
    )
  
  assign(
    x = prop_table_name,
    value = prop_table,
    envir = .GlobalEnv
  )
  
}