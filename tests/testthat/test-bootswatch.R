test_that("first_font takes the first family from a font stack", {
  expect_equal(
    brandBuilder:::first_font('"Lato", -apple-system, sans-serif'),
    "Lato"
  )
  expect_equal(
    brandBuilder:::first_font("'Source Sans 3', sans-serif"),
    "Source Sans 3"
  )
  expect_equal(brandBuilder:::first_font("Inter"), "Inter")
})

test_that("first_font falls back for unusable values", {
  expect_equal(brandBuilder:::first_font("null", "x"), "x")
  expect_equal(brandBuilder:::first_font("inherit", "x"), "x")
  expect_equal(brandBuilder:::first_font(NA_character_, "x"), "x")
  expect_equal(brandBuilder:::first_font("var(--bs-font-sans-serif)", "x"), "x")
})

test_that("css_hex returns #RRGGBB or the fallback", {
  expect_equal(brandBuilder:::css_hex("white", "#000000"), "#FFFFFF")
  expect_equal(brandBuilder:::css_hex("not-a-color", "#123456"), "#123456")
  expect_equal(brandBuilder:::css_hex(NA_character_, "#123456"), "#123456")
})

test_that("system fonts are not treated as Google fonts", {
  expect_equal(brandBuilder:::font_source("Arial"), "system")
  expect_equal(brandBuilder:::font_source("monospace"), "system")
  expect_equal(brandBuilder:::font_source("Figtree"), "google")
})
