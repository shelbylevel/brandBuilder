test_that("contrast extremes are correct", {
  expect_equal(brandBuilder:::contrast("#000000", "#FFFFFF"), 21)
  expect_equal(brandBuilder:::contrast("#FFFFFF", "#FFFFFF"), 1)
})
