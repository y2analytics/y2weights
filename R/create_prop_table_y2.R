### create_prop_table_y2

#' Create a table of population parameters to be used for weighting data
#'
#' Use create_prop_table_y2() to create nicely formatted population parameter tables to be used for weighting data. To avoid typos, create_prop_table_y2() will automatically create an object in your R environment using the provided variable to create the name. This function is designed to work within the y2weights framework.
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be weighted
#' @param variable The variable for which you will be supplying population parameters
#' @param population_params A named vector containing population parameters
#' @param add_missing_row DEFAULT = FALSE, Set to TRUE to add a row to the prop table that takes into account "MISSING" values in the variable for which you are supplying population parameters
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
    population_params, 
    add_missing_row = FALSE
){
  
  ### Get the variable as a string
  variable_string <- 
    deparse(
      substitute(
        variable
        )
      )
  
  ### Make pps name as string
  pps_string <- 
    stringr::str_c(
      variable_string,
      '_prop_table'
    )
  
  ### make prop table without the row for MISSING levels
  if(add_missing_row == TRUE){
    
    prop_table <- 
      tibble::tibble(
        {{ variable }} := 
          c(
            population_params %>% names,
            'MISSING'
          ),
        prop = 
          c(
            population_params %>% unname() * nrow(dataset),
            (dataset %>% dplyr::filter({{ variable }} == 'MISSING')) %>% nrow()
          ) 
      )
    
  } else {
    
    ### Make the prop table when there are missing levels
    prop_table <- 
      tibble::tibble(
        {{ variable }} := 
          population_params %>% names,
        prop = 
          population_params %>% unname() * nrow(dataset)
      ) 


}
  
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
  
  ### Ensure that weight groups strings are the same as the data
  if(  
    
    only_in_supplied %>% 
    length() > 0 |
      only_in_expected %>% 
      length() > 0
    
  ){
    
    message(
        'Warning: Expected inputs do not match the supplied inputs.'
        )
    
    if(only_in_expected %>% 
       length() > 0){
      
      message(
        stringr::str_c(
          'Expected inputs [ ',
          stringr::str_flatten(
            only_in_expected, 
            collapse = ', '
            ),
          ' ] are not found in supplied inputs.'
        )
      )
      
    }
    
    if(only_in_supplied %>% 
       length() > 0){
      
      message(
        stringr::str_c(
          'Supplied inputs [ ',
          stringr::str_flatten(
            only_in_supplied, 
            collapse = ', '
            ),
          ' ] are not found in expected inputs.'
        )
      )
      
    }
    
  }
  
  ### Check if the population_params add up to 100%
  if(  
    
    sum(population_params) %>% round(2) != 1
    
  ){
    
    message(
      stringr::str_c(
        'Warning: Provided weighting proportions != 1. ',
        'The sum of the supplied proportions == ',
        sum(population_params)
        ) 
      )
    
  }
  
  
  ### Check to see if add_missing_row should be set to TRUE
  n <- 
    dataset %>% 
    y2clerk::freqs(
      {{ variable }}
    ) %>% 
    dplyr::filter(
      .data$label == 'MISSING'
    ) %>% 
    dplyr::count() %>% 
    dplyr::pull(
      n
    )
  
  if(  
    
    n > 0 & add_missing_row == FALSE
    
  ){
    
    message(
      stringr::str_c(
        'Warning: add_missing_row = FALSE, but "MISSING" levels detected in data column ',
        variable_string
      )
    )
    
  }
  
  
  ### Check to see if there are NA's in the column
  n <- 
    dataset %>% 
    y2clerk::freqs(
      {{ variable }}
    ) %>% 
    dplyr::filter(
      is.na(.data$label)
    ) %>% 
    dplyr::count() %>% 
    dplyr::pull(.data$n)
  
  if(  
    
    n > 0 
    
  ){
    
    message(
      stringr::str_c(
        'Warning: NAs detected in data column ',
        variable_string,
        '. Please fix this before continuing.'
      )
    )
    
  }
  
  
  ### Send the prop table to the environment
  prop_table %>%
    tibble::tibble() %>% 
    list() %>%
    purrr::set_names(
      pps_string
    ) %>% 
    list2env(envir = .GlobalEnv) %>%
    invisible()
  
}

