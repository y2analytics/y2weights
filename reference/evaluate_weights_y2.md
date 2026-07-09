# Compare weighted and unweighted data vs the ideal

Use evaluate_weights_y2() to evaluate given variables given weighting.
For variables used in weighting, compare 1) unweighted data, 2) weighted
data, 3) target weighting goals, 4) movement after weighting, and 5)
difference from the target. For variables not used in weighting, can
only compare 1) unweighted data, 2) weighted data, and 3) movement.

## Usage

``` r
evaluate_weights_y2(
  dataset,
  ...,
  weight_var,
  nas = TRUE,
  remove_missing = FALSE
)
```

## Arguments

- dataset:

  A dataframe to be weighted

- ...:

  List of variables to check

- weight_var:

  Weight variable

- nas:

  DEFAULT = TRUE, Remove NAs from calculations

- remove_missing:

  DEFAULT = FALSE, Before running frequencies, filter out responses that
  are "MISSING"

## Value

A tibble with the following columns: variable, label, result_unweighted,
target, result_weighted, movement, diff_from_target

## Examples

``` r
municipal_data %>%
  define_target_y2(
    s_sex,
    c(
      '1' = .49,
      '2' = .5,
      '3' = .01
    )
  )
#> Error in as.character(x): Can't convert `x` <haven_labelled> to <character>.

weights_schema <- municipal_data %>%
  rake_y2(
    s_sex
  )
#> Error in rake_y2(., s_sex): Expected target(s) "target_s_sex" not found

municipal_data$trimmed_weights <- trim_weights_y2(weights_schema)
#> Error: object 'weights_schema' not found

municipal_data %>%
  evaluate_weights_y2(
    s_sex,
    weight_var = trimmed_weights
  )
#> Error in purrr::map(variables, ~combine_data(w_variable = .x, nas = nas,     dataset = dataset, weight = {        {            weight_var        }    }, remove_missing = remove_missing)): ℹ In index: 1.
#> Caused by error in `map()`:
#> ℹ In index: 1.
#> Caused by error in `dplyr::count()`:
#> ℹ In argument: `n = base::sum(trimmed_weights, na.rm = TRUE)`.
#> ℹ In group 1: `s_sex = 1`.
#> Caused by error:
#> ! object 'trimmed_weights' not found
```
