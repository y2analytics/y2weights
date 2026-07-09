# Rake weights using target parameter tables

Use rake_y2() in conjunction with define_target_y2() to rake weights
cleanly and with easier to understand errors. rake_y2() will
automatically pull target population parameter tables from your
environment that have matching names to the variables provided.

## Usage

``` r
rake_y2(dataset, ...)
```

## Arguments

- dataset:

  A dataframe to be used in creating the survey design.

- ...:

  List of variables

## Value

A complex, raked, survey.design object. This output will be used in the
weights_schema argument of trim_weights_y2().

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
#> <environment: R_GlobalEnv>
weights_schema <- municipal_data %>%
  rake_y2(
    s_sex
  )
```
