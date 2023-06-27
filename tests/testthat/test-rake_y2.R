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
    sample(),
  w_district = c(
    rep('District 1', 250),
    rep('District 2', 250),
    rep('District 3', 250)
  ) %>% 
    sample()
)


# Errors and Warnings -----------------------------------------

test_that('Error: Multiple non-existent prop_tables', {
  define_target_y2(
    test_df,
    w_age,
    c(
      '18 - 34' = 0.6,
      '35 - 44' = 0.1,
      '45 - 54' = 0.12,
      '55 - 64' = 0.09,
      '65+' = 0.09
    )
  )
  
  expect_error(
    svy_design <- test_df %>% 
      rake_y2(
        w_age,
        w_district,
        w_race
      ),
    'Expected target(s) "target_w_district, target_w_race" not found',
    fixed = TRUE
  )
})


test_that('Error: Single non-existent prop_table', {
  define_target_y2(
    test_df,
    w_age,
    c(
      '18 - 34' = 0.6,
      '35 - 44' = 0.1,
      '45 - 54' = 0.12,
      '55 - 64' = 0.09,
      '65+' = 0.09
    )
  )
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.55,
      'Male' = 0.45
    )
  )
  
  expect_error(
    svy_design <- test_df %>% 
      rake_y2(
        w_age,
        w_sex,
        w_race
      ),
    'Expected target(s) "target_w_race" not found',
    fixed = TRUE
  )
})



# Overall -----------------------------------------------------------------

test_that('Output structure', {
  define_target_y2(
    test_df,
    w_age,
    c(
      '18 - 34' = 0.6,
      '35 - 44' = 0.1,
      '45 - 54' = 0.12,
      '55 - 64' = 0.09,
      '65+' = 0.09
    )
  )
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.55,
      'Male' = 0.45
    )
  )
  define_target_y2(
    test_df,
    w_race,
    c(
      'White' = 0.55,
      'Non-white' = 0.45
    )
  )
  svy_design <- test_df %>% 
    rake_y2(
      w_age,
      w_sex,
      w_race
    )
  
  expect_equal(
    class(svy_design),
    c('survey.design2', 'survey.design')
  )
  expect_equal(
    length(svy_design[["prob"]]),
    750
  )
  expect_equal(
    class(svy_design[["prob"]]),
    'numeric'
  )
})

