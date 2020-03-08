test_that("Kelvin tests", {
  expect_equal(convert.temp(from.temp=0, from.unit="Kelvin", to.unit="Celsius"), -273.15)
  expect_equal(convert.temp(from.temp=-273.15, from.unit="Celsius", to.unit="Kelvin"), 0)
})
