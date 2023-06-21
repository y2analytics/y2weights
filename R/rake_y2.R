### rake_y2

#' Rake weights using population parameter tables
#'
#' Use rake_y2() in conjunction with create_prop_table_y2() to rake weights cleanly and with easier to understand errors. rake_y2() will automatically pull population parameter tables
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be used in creating the survey design.
#' @param ... List of variables 
#' @export
#' @return A complex raked survey design object.
#' @examples
#' \dontrun{
#' survey::svydesign(
#' ids = ~1, 
#' data = municipal_data
#' )
#'}

rake_y2 <- function(
    dataset,
    ...
){
  
  # Get variable string
  
  variable_string <-
    dataset %>%
    dplyr::select(
      ...
    ) %>%
    names()
  
  # Prep variable names for function
  
  variable_list <-
    stringr::str_c(
      '~',
      variable_string
    ) %>%
    purrr::map(
      ~as.formula(.x)
    )
  
  # Ensure every prop_table exists
  
  props_list <-
    stringr::str_c(
      variable_string,
      '_prop_table'
    )
  
  nonexistent <- c()
  
  for (prop in props_list) {
    
    if (!exists(prop)) {
      
      nonexistent <- append(nonexistent, prop)
      
    }
    
  }
  
  if (length(nonexistent > 0)) {
    
    stop(
      stringr::str_c(
        'Expected prop_table(s) "',
        paste(
          nonexistent,
          collapse = ', '
        ),
        '" not found'
      )
    )
    
  }
  
  # Prep prop table list for function
  
  props_list <-
    stringr::str_c(
      variable_string,
      '_prop_table'
    ) %>%
    purrr::map(
      ~eval(
        as.symbol(
          .x
        )
      )
    )
  
  # Initialize
  
  svy_start_weight <-
    survey::svydesign(
      ids = ~1,
      weights = NULL,
      data = dataset
    )
  
  # Create survey design
  
  svy_design <- 
    survey::rake(
      design =
        svy_start_weight,
      sample.margins =
        variable_list,
      population.margins =
        props_list
    ) %>% 
    suppressWarnings()
  
  # Return
  
  return(svy_design)
  
}