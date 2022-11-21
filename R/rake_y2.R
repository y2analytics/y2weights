### rake_y2

#' Rake weights using population parameter tables
#'
#' Use rake_y2() in conjunction with create_prop_table_y2() to rake weights cleanly and with easier to understand errors. rake_y2() will automatically pull population parameter tables
#'
#' @keywords freqs weights population parameters
#' @param dataset A dataframe to be weighted
#' @param ... List of variables 
#' @export
#' @return A complex survey object
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

  svy_start_weight <-
    survey::svydesign(
      ids = ~1, 
      data = dataset
    )

  variable_string <-
    dataset %>%
    dplyr::select(
      ...
    ) %>%
    names

  variable_list <-
    stringr::str_c(
      '~',
      variable_string
    ) %>%
    purrr::map(
      ~as.formula(.x)
    )

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

  ss_rake <- 
    survey::rake(
    design =
      svy_start_weight,
    sample.margins =
      variable_list,
    population.margins =
      props_list
  )

  ss_rake

}
