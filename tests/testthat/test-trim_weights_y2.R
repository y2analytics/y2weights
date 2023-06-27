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



# Overall -----------------------------------------------------------------

test_that('Right output', {
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  define_target_y2(
    test_df,
    w_age,
    c(
      '18 - 34' = 0.4,
      '35 - 44' = 0.2,
      '45 - 54' = 0.2,
      '55 - 64' = 0.1,
      '65+' = 0.1
    )
  )
  define_target_y2(
    test_df,
    w_race,
    c(
      'Non-white' = 0.15,
      'White' = 0.85
    )
  )
  
  set.seed(1)
  svy_design <- rake_y2(
    test_df,
    w_sex,
    w_age,
    w_race
  )
  
  test_df <- test_df %>% 
    dplyr::mutate(
      weights = weights(svy_design),
      trimmed_weights = trim_weights_y2(svy_design)
    )
  upper_expected <- quantile(test_df$weights, 0.95)
  lower_expected <- quantile(test_df$weights, 0.05)
  upper_actual <- max(test_df$trimmed_weights) 
  lower_actual <- min(test_df$trimmed_weights) 
  
  expect_equal(
    class(test_df$trimmed_weights),
    'numeric'
  )
  expect_false(any(is.na(test_df$trimmed_weights)))
  expect_true(upper_expected <= upper_actual)
  expect_true(lower_expected >= lower_actual)
})


# Arguments ---------------------------------------------------------------



