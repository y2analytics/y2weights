# Create a table of target population parameters to be used for weighting data

Use define_target_y2() to create a table of target population parameters
to be used for weighting data. To avoid typos, define_target_y2() will
automatically create an object in your R environment using the provided
variable to create the name (e.g., "target_varname").

## Usage

``` r
define_target_y2(dataset, variable, target_params)
```

## Arguments

- dataset:

  A dataframe with variables to be used in weighting.

- variable:

  The variable for which you will be supplying population parameters.

- target_params:

  A named vector containing target parameters you wish your data to be
  weighted to. If the variable has a "MISSING" level, you do not need to
  include it in the target_params, it will be calculated automatically
  (essentially set to weight those respondents as 1, the mean).

## Value

A tibble with population parameters to the environment

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
```
