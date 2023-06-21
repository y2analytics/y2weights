##### y2weights Function Tests

#### Test Data -----------------------------------------------------------

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

### create_prop_table_y2 -------------------------------------------------
# Unnamed Population Params ----------------------------------------------

test_that('Unnamed Population Params', {
  
  expect_error(
    create_prop_table_y2(
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
    'Population params vector is unnamed; please add levels and proportions',
    fixed = TRUE
  )

})

# Column Names -----------------------------------------------------------

test_that('Column Names', {
  
  create_prop_table_y2(
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
  
  names_actual <- w_age_prop_table %>% 
    names()
  
  names_expected <- c(
    'w_age',
    'prop'
  )
  
  expect_equal(names_actual, names_expected)
  
})

# Row Names --------------------------------------------------------------

test_that('Row Names', {
  
  create_prop_table_y2(
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
  
  levels_actual <- w_age_prop_table %>% 
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

# 'MISSING' Values -------------------------------------------------------

test_that('Extracts "MISSING" Values', {
  
  create_prop_table_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.55,
      'Male' = 0.45
    )
  )
  
  levels <- w_sex_prop_table %>% 
    dplyr::pull(w_sex)
  
  expect_true('MISSING' %in% levels)
  
})

# Number of 'MISSING' Values ---------------------------------------------

test_that('Correct Number of "MISSING" Values', {
  
  create_prop_table_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.55,
      'Male' = 0.45
    )
  )
  
  n_size <- w_sex_prop_table %>% 
    dplyr::pull(prop) %>% 
    dplyr::nth(3)
  
  expect_true(n_size == 7)
  
})

# NA Values --------------------------------------------------------------

test_that('Stops if NA Values', {
  
  test_df_with_nas <- test_df %>% 
    dplyr::mutate(
      w_sex = ifelse(
        w_sex == 'MISSING',
        NA,
        w_sex
      )
    )
  
  expect_error(
    create_prop_table_y2(
      test_df_with_nas,
      w_sex,
      c(
        'Female' = 0.55,
        'Male' = 0.45
      )
    ),
    'variable w_sex contains NA values; please set to "MISSING"',
    fixed = TRUE
  )
  
})

# Level Only in Supplied -------------------------------------------------

test_that('Only in Supplied Error', {
  
  expect_error(
    create_prop_table_y2(
      test_df,
      w_sex,
      c(
        'Female' = 0.54,
        'Male' = 0.44,
        'Other' = 0.02
      )
    ),
    'Supplied inputs [Other] are not found in expected inputs',
    fixed = TRUE
  )
  
})

# Level Only in Expected -------------------------------------------------

test_that('Only in Expected Error', {
  
  expect_error(
    create_prop_table_y2(
      test_df,
      w_age,
      c(
        '18 - 34' = 0.6,
        '35 - 44' = 0.1,
        '45 - 54' = 0.12,
        '55 - 64' = 0.18
      )
    ),
    'Expected inputs [65+] are not found in supplied inputs',
    fixed = TRUE
  )
  
})

# Population params don't add to 1 ---------------------------------------

test_that('Population Params Sum', {
  
  expect_warning(
    create_prop_table_y2(
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

# Sent to Environment ----------------------------------------------------

test_that('Table Sent to Environment', {
  
  create_prop_table_y2(
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
      'w_age_prop_table', 
      envir = .GlobalEnv
    )
  )
  
})

### rake_y2 --------------------------------------------------------------
# Single non-existent prop_table -----------------------------------------

test_that('Single non-existent prop_table error', {
  
  create_prop_table_y2(
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
  
  create_prop_table_y2(
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
    'Expected prop_table(s) "w_race_prop_table" not found',
    fixed = TRUE
  )
  
})

# Multiple non-existent prop_tables --------------------------------------

test_that('Multiple non-existent prop_tables error', {
  
  create_prop_table_y2(
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
        w_sex,
        w_race
      ),
    'Expected prop_table(s) "w_sex_prop_table, w_race_prop_table" not found',
    fixed = TRUE
  )
  
})

### create_trimmed_weights_y2 --------------------------------------------
# Test Less Than Upper Limit for strict = TRUE ---------------------------

test_that('Less Than Upper Limit', {
  
  create_prop_table_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  
  create_prop_table_y2(
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
  
  create_prop_table_y2(
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
      weights = create_weights_y2(svy_design),
      trimmed_weights = create_trimmed_weights_y2(svy_design)
    )
  
  upper_expected <- stats::quantile(test_df$weights, 0.95)
  
  upper_actual <- summary(test_df$trimmed_weights) %>% 
    dplyr::nth(6)
  
  expect_true(upper_expected <= upper_actual)
  
})

### evaluate_weights_y2 --------------------------------------------------
# Error if weight_var is missing -----------------------------------------

test_that('Missing weight_var Error', {
  
  create_prop_table_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  
  create_prop_table_y2(
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
  
  create_prop_table_y2(
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
      weights = create_weights_y2(svy_design)
    )
  
  expect_error(
    weights_eval <- evaluate_weights_y2(
      test_df,
      w_sex,
      w_age,
      w_race,
      remove_missing = TRUE
    ),
    'Argument "weight_var" is missing',
    fixed = TRUE
  )
  
})

# Keeps Missing Values ---------------------------------------------------

test_that('Evaluation Keeps Missing Values', {
  
  create_prop_table_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  
  create_prop_table_y2(
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
  
  create_prop_table_y2(
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
      weights = create_weights_y2(svy_design)
    )
  
  weights_eval <- evaluate_weights_y2(
    test_df,
    w_sex,
    w_age,
    w_race,
    weight_var = weights
  )
  
  var_levels <- weights_eval %>% 
    dplyr::pull(label)
  
  expect_true('MISSING' %in% var_levels)
  
})

# Removes Missing Values -------------------------------------------------

test_that('Evaluation Removes Missing Values', {
  
  create_prop_table_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  
  create_prop_table_y2(
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
  
  create_prop_table_y2(
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
    mutate(
      weights = create_weights_y2(svy_design)
    )
  
  weights_eval <- evaluate_weights_y2(
    test_df,
    w_sex,
    w_age,
    w_race,
    weight_var = weights,
    remove_missing = TRUE
  )
  
  var_levels <- weights_eval %>% 
    dplyr::pull(label)
  
  expect_true(!'MISSING' %in% var_levels)
  
})

# Supplied var with no prop_table ----------------------------------------

test_that('Supplied Var with No prop_table', {
  
  create_prop_table_y2(
    test_df,
    w_sex,
    c(
      'Female' = 0.52,
      'Male' = 0.48
    )
  )
  
  create_prop_table_y2(
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
  
  create_prop_table_y2(
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
    mutate(
      weights = create_weights_y2(svy_design)
    )
  
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
      w_age,
      w_race,
      w_party,
      weight_var = weights,
      remove_missing = TRUE
    ),
    'object \'w_party_prop_table\' not found',
    fixed = TRUE
  )
  
})

### End ------------------------------------------------------------------


