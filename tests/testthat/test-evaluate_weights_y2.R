# Test Data -----------------------------------------------------------

set.seed(1)

test_df <- data.frame(
  w_sex = c(
    rep('Male', 375), # 50%
    rep('Female', 368), # 49%
    rep('MISSING', 7) # 1%
  ) %>% 
    sample(),
  w_age = c(
    rep('18 - 34', 75), # 10%
    rep('35 - 44', 150), # 20%
    rep('45 - 54', 150), # 20%
    rep('55 - 64', 150), # 20% 
    rep('65+', 225) # 30 %
  ) %>% 
    sample(),
  w_race = c(
    rep('White', 660), # 88%
    rep('Non-white', 75), # 10%
    rep('MISSING', 15) # 2%
  ) %>% 
    sample()
)


# Errors and Warnings -----------------------------------------------------

test_that('Error: Missing weight_var', {
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  
  set.seed(1)
  svy_design <- rake_y2(
    test_df,
    w_sex
  )
  test_df$weights <- weights(svy_design)
  
  expect_error(
    weights_eval <- test_df %>% 
      evaluate_weights_y2(
        w_sex
      ),
    'Argument "weight_var" is missing',
    fixed = TRUE
  )
})


test_that('Error: no table for target population', {
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )

  set.seed(1)
  svy_design <- rake_y2(
    test_df,
    w_sex
  )
  test_df$weights <- weights(svy_design)

  test_df_new_var <- test_df %>% 
    dplyr::mutate(
      w_party = c(
        rep('Democrat', 113),
        rep('Republican', 375),
        rep('Unaffiliated', 262)
      )
    )
  
  expect_error(
    weights_eval <- evaluate_weights_y2(
      test_df_new_var,
      w_sex,
      w_party,
      weight_var = weights
    ),
    'object \'target_w_party\' not found',
    fixed = TRUE
  )
})


# Arguments ---------------------------------------------------------------

test_that('Evaluation Keeps Missing Values', {
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )

  set.seed(1)
  svy_design <- rake_y2(
    test_df,
    w_sex
  )
  test_df$weights <- weights(svy_design)
  
  weights_eval <- evaluate_weights_y2(
    test_df,
    w_sex,
    weight_var = weights
  )
  var_levels_present <- weights_eval %>% dplyr::pull(label)
  
  weights_eval <- evaluate_weights_y2(
    test_df,
    w_sex,
    weight_var = weights,
    remove_missing = TRUE
  )
  var_levels_missing <- weights_eval %>% dplyr::pull(label)
  
  expect_true('MISSING' %in% var_levels_present)
  expect_true(!'MISSING' %in% var_levels_missing)
})

