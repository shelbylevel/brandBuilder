test_that("contrast extremes are correct",{expect_equal(brandbuilder:::contrast("#000000","#FFFFFF"),21);expect_equal(brandbuilder:::contrast("#FFFFFF","#FFFFFF"),1)})
