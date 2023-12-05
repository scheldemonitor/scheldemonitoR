## importAbioticData()
test_that("importAbioticData() returns a tibble", {
  # test parameter: 9694
  df <- importAbioticData(9694, 2010, 2014)
  expect_s3_class(df, "data.frame")
})

test_that("importAbioticData() returns unique row FID", {
  df <- importAbioticData(9694, 2010, 2014)
  expect_equal(nrow(df), nrow(df %>% dplyr::distinct(FID)))
})

## importBioticData()
test_that("importBioticData() returns a tibble", {
  # for aphia
  df_aphia <- importBioticData(129938, 2006, 2008, "aphia")
  #for imis
  df_imis <- importBioticData(1073, 2015, 2020, 'imis')

  expect_s3_class(df_aphia, "data.frame")
  expect_s3_class(df_imis, "data.frame")
})

test_that("importBioticData() returns unique row FID", {
  # for aphia
  df_aphia <- importBioticData(129938, 2006, 2008, "aphia")
  #for imis
  df_imis <- importBioticData(1073, 2015, 2020, 'imis')

  expect_equal(nrow(df_aphia), nrow(df_aphia %>% dplyr::distinct(FID)))
  expect_equal(nrow(df_imis), nrow(df_imis %>% dplyr::distinct(FID)))
})
