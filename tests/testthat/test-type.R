test_that("Type assignment works", {
  x <- y <- c(letters[1:4])
  attr(x, "type") <- "x"
  type(y) <- "x"
  expect_identical(x, y)
})
