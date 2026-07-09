# Create a trimmed weights vector

Use trim_weights_y2() to create a vector of trimmed weights by taking
the survey design output from rake_y2() and passing it to this function.

## Usage

``` r
trim_weights_y2(
  weights_schema,
  limit_method = c("standard deviations", "percentile", "decimal"),
  upper_limit = 3,
  lower_limit = -3,
  strict = TRUE
)
```

## Arguments

- weights_schema:

  A complex, raked, survey.design object (can be created using the
  rake_y2() function).

- limit_method:

  (default = 'standard deviations') The type of limits that will be used
  for the upper_limit/lower_limit arguments. Currently accepts 'standard
  deviations', 'percentile', or 'decimal'.

- upper_limit:

  (default = 3) Set an upper trim limit for your weights (the default of
  3 SDs would trim only .3% of the weights). If limit_method is set to
  'percentile', limit will be the upper percentile at which the weight
  will be trimmed. If limit_method is set to 'decimal', limit will be
  the upper numerical limit of the weight.

- lower_limit:

  (default = -3) Set a lower trim limit for your weights (the default of
  3 SDs would trim only .3% of the weights). If limit_method is set to
  'percentile', limit will be the lower percentile at which the weight
  will be trimmed. If limit_method is set to 'decimal', limit will be
  the lower numerical limit of the weight.

- strict:

  (default = TRUE) The reapportionment of the ‘trimmings’ from the
  weights may sometimes push other weights over the limits. If strict =
  TRUE the function repeats the trimming iteratively to prevent this.

## Value

A vector of trimmed weights constructed using the weights_schema, a
survey.design object

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

# Default method (Standard deviations)
municipal_data$trimmed_weights <- trim_weights_y2(weights_schema)
# OR
municipal_data %>%
  dplyr::mutate(
    trimmed_weights = trim_weights_y2(
      weights_schema
    )
  )
#> # A tibble: 100 × 60
#>    StartDate           EndDate             Status          IPAddress Progress
#>    <dttm>              <dttm>              <dbl+lbl>       <chr>        <dbl>
#>  1 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  2 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  3 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  4 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  5 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  6 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  7 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  8 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  9 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#> 10 2021-03-24 12:51:41 2021-03-24 12:51:41 2 [Survey Test] ""             100
#> # ℹ 90 more rows
#> # ℹ 55 more variables: Duration__in_seconds_ <dbl>, Finished <dbl+lbl>,
#> #   RecordedDate <dttm>, ResponseId <chr>, RecipientLastName <chr>,
#> #   RecipientFirstName <chr>, RecipientEmail <chr>, ExternalReference <chr>,
#> #   LocationLatitude <chr>, LocationLongitude <chr>, DistributionChannel <chr>,
#> #   UserLanguage <chr>, s_qualify <dbl+lbl>, s_direction <dbl+lbl>,
#> #   n_overall_1 <dbl>, s_5year <dbl+lbl>, oe_likemost <chr>, …

# Percentile method
municipal_data %>%
  dplyr::mutate(
    trimmed_weights = trim_weights_y2(
      weights_schema,
      limit_method = 'percentile',
      upper_limit = .975,
      lower_limit = .025
    )
  )
#> # A tibble: 100 × 60
#>    StartDate           EndDate             Status          IPAddress Progress
#>    <dttm>              <dttm>              <dbl+lbl>       <chr>        <dbl>
#>  1 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  2 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  3 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  4 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  5 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  6 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  7 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  8 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  9 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#> 10 2021-03-24 12:51:41 2021-03-24 12:51:41 2 [Survey Test] ""             100
#> # ℹ 90 more rows
#> # ℹ 55 more variables: Duration__in_seconds_ <dbl>, Finished <dbl+lbl>,
#> #   RecordedDate <dttm>, ResponseId <chr>, RecipientLastName <chr>,
#> #   RecipientFirstName <chr>, RecipientEmail <chr>, ExternalReference <chr>,
#> #   LocationLatitude <chr>, LocationLongitude <chr>, DistributionChannel <chr>,
#> #   UserLanguage <chr>, s_qualify <dbl+lbl>, s_direction <dbl+lbl>,
#> #   n_overall_1 <dbl>, s_5year <dbl+lbl>, oe_likemost <chr>, …

# Decimal method
municipal_data %>%
  dplyr::mutate(
    trimmed_weights = trim_weights_y2(
      weights_schema,
      limit_method = 'decimal',
      upper_limit = 5,
      lower_limit = 0.5
    )
  )
#> # A tibble: 100 × 60
#>    StartDate           EndDate             Status          IPAddress Progress
#>    <dttm>              <dttm>              <dbl+lbl>       <chr>        <dbl>
#>  1 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  2 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  3 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  4 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  5 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  6 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  7 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  8 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#>  9 2021-03-24 12:51:40 2021-03-24 12:51:40 2 [Survey Test] ""             100
#> 10 2021-03-24 12:51:41 2021-03-24 12:51:41 2 [Survey Test] ""             100
#> # ℹ 90 more rows
#> # ℹ 55 more variables: Duration__in_seconds_ <dbl>, Finished <dbl+lbl>,
#> #   RecordedDate <dttm>, ResponseId <chr>, RecipientLastName <chr>,
#> #   RecipientFirstName <chr>, RecipientEmail <chr>, ExternalReference <chr>,
#> #   LocationLatitude <chr>, LocationLongitude <chr>, DistributionChannel <chr>,
#> #   UserLanguage <chr>, s_qualify <dbl+lbl>, s_direction <dbl+lbl>,
#> #   n_overall_1 <dbl>, s_5year <dbl+lbl>, oe_likemost <chr>, …
```
