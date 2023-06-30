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

test_that('Error: MISSING should not be in population_params', {
  expect_error(
    define_target_y2(
      test_df,
      w_sex,
      c(
        'Female' = 0.55,
        'Male' = 0.44,
        'MISSING' = .01
      )
    ),
    'Do not supply population_params for "MISSING" values. These are calculated automatically and weighted to 1 on this parameter',
    fixed = TRUE
  )
})


test_that('Error: Unnamed Population Params', {
  expect_error(
    define_target_y2(
      test_df,
      w_age,
      c(
        '18 - 34',
        '35 - 44',
        '45 - 54',
        '55 - 64',
        '65+'
      )
    ),
    'population_params vector is unnamed; please add levels and proportions',
    fixed = TRUE
  )
})


test_that('Error: Stops if NA Values', {
  test_df_with_nas <- test_df %>% 
    dplyr::mutate(
      w_sex = ifelse(
        w_sex == 'MISSING',
        NA,
        w_sex
      )
    )
  
  expect_error(
    define_target_y2(
      test_df_with_nas,
      w_sex,
      c(
        'Female' = 0.55,
        'Male' = 0.45
      )
    ),
    'variable "w_sex" in dataset "test_df_with_nas" contains NA values; please set NAs to "MISSING"',
    fixed = TRUE
  )
})


test_that('Error: levels not in dataset', {
  expect_error(
    define_target_y2(
      test_df,
      w_sex,
      c(
        'Female' = 0.54,
        'Male' = 0.44,
        'Other' = 0.02
      )
    ),
    'Supplied population_params [Other] are not found in the dataset',
    fixed = TRUE
  )
})


test_that('Error: levels not in population_params', {
  expect_error(
    define_target_y2(
      test_df,
      w_age,
      c(
        '18 - 34' = 0.6,
        '35 - 44' = 0.1,
        '45 - 54' = 0.12,
        '55 - 64' = 0.18
      )
    ),
    'dataset levels of [65+] are not found in population_params',
    fixed = TRUE
  )
})


test_that('Error: population_params sum', {
  expect_warning(
    define_target_y2(
      test_df,
      w_sex,
      c(
        'Female' = 0.55,
        'Male' = 0.46
      )
    ),
    'Provided total weighting proportions is not 1. The sum of the supplied proportions is 1.01',
    fixed = TRUE
  )
})



# Overall -----------------------------------------------------------------

test_that('Column Names', {
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  
  names_actual <- target_w_sex %>% 
    names()
  names_expected <- c(
    'w_sex',
    'prop'
  )
  
  expect_equal(names_actual, names_expected)
})


test_that('Row Names', {
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
  
  levels_actual <- target_w_age %>% 
    dplyr::pull(w_age)
  levels_expected <- c(
    '18 - 34',
    '35 - 44',
    '45 - 54',
    '55 - 64',
    '65+'
  )
  
  expect_equal(levels_actual, levels_expected)
})


test_that('Extracts "MISSING" Values', {
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.55,
      'Male' = 0.45
    )
  )
  
  levels <- target_w_sex %>% 
    dplyr::pull(w_sex)
  
  expect_true('MISSING' %in% levels)
})


test_that('Correct Number of "MISSING" Values', {
  define_target_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.55,
      'Male' = 0.45
    )
  )
  
  n_size <- target_w_sex %>% 
    dplyr::pull(prop) %>% 
    dplyr::nth(3)
  
  expect_true(n_size == 7)
})


test_that('Table Sent to Environment', {
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
  
  expect_true(
    exists(
      'target_w_age', 
      envir = .GlobalEnv
    )
  )
})

