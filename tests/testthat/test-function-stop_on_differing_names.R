#library(testthat)

test_that("stop_on_differing_names() works", {

  f <- kwb.umberto:::stop_on_differing_names
  
  expect_error(f())

  dfs <- list(
    df1 = data.frame(a = 1, b = 2),
    df2 = data.frame(a = 1, b = 2, c = 3),
    data.frame(a = 2, b = 3)
  )
  
  expect_error(f(dfs))
  expect_error(f(dfs[-1L]), "<unnamed_2>")
  
  expect_silent(f(dfs[-2L]))
})
