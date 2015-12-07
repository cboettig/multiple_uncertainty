testthat::context("Basic execution and unit tests")


testthat::test_that("multipleuncertainty() function runs without error using default settings", {
  escapement <- multiple_uncertainty()
  testthat::expect_is(escapement, "numeric")
})

testthat::context("Reed results")

  
testthat::test_that("We recover the classic Reed result when implementation and measurement error are zero",{
    
  escapement <- multiple_uncertainty()
  ## linear increase: no harvest at low stock sizes (interval is specific to grid and not comprehensive)
  testthat::expect_equal(escapement[1:20], 0:19)
  ## Constant escapement (interval is specific to grid and not comprehensive)
  testthat::expect_true(all(escapement[80:140] == escapement[80]))
})





testthat::context("Sethi tests")


testthat::test_that("multipleuncertainty() function runs with different grids, different noise levels", {
  
  escapement <- multiple_uncertainty(x_grid = seq(0, 150, length = 151), 
                                     h_grid = seq(0, 150, length = 152), 
                                     y_grid = seq(0, 150, length = 153), 
                                     q_grid = seq(0, 150, length = 154),
                                     sigma_g = 0.5, 
                                     sigma_m = 0.1, 
                                     sigma_i = 0.1)
  testthat::expect_is(escapement, "numeric")
  
  
  ## Expect deviation from constant escapement (interval is specific to grid and not comprehensive)
  testthat::expect_false(all(escapement[80:140] == escapement[80]))

  })



testthat::context("Unit tests for internal functions")

## Probability functions here