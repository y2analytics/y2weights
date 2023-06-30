### rake_y2

#' Rake weights using population parameter tables
#'
#' Use rake_y2() in conjunction with define_target_y2() to rake weights cleanly and with easier to understand errors. rake_y2() will automatically pull target population parameter tables from your environment that have matching names to the variables provided.
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be used in creating the survey design.
#' @param ... List of variables 
#' @export
#' @return A complex raked survey design object.
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
#' svy_design <- municipal_data %>% 
#'   rake_y2(
#'     s_sex
#'   )

rake_y2 <- function(
    dataset,
    ...
) {
  
  variable_string <-
    dataset %>%
    dplyr::select(
      ...
    ) %>%
    names()
  
  variable_list <-
    stringr::str_c(
      '~',
      variable_string
    ) %>%
    purrr::map(
      ~as.formula(.x)
    )
  
  # Ensure every target exists
  props_list <-
    stringr::str_c(
      'target_',
      variable_string
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
        'Expected target(s) "',
        paste(
          nonexistent,
          collapse = ', '
        ),
        '" not found'
      )
    )
  }
  
  # Prep target table list for function
  props_list <-
    stringr::str_c(
      'target_',
      variable_string
    ) %>%
    purrr::map(
      ~eval(
        as.symbol(
          .x
        )
      )
    )
  
  svy_start_weight <-
    survey::svydesign(
      ids = ~1,
      weights = NULL,
      data = dataset
    )
  
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
  
  return(svy_design)
}

