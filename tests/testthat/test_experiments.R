context("Create experiments")

test_that("Simple experiments", {
  e1 <- nl_experiment(
    model_file = "no-file.nlogo"
  )
  expect_is(e1, "nl_experiment")
})

test_that("Simple experiments 2", {
  e1 <- nl_experiment(
    model_file = "no-file.nlogo"
  )
  e1 <- nl_set_param_values(
    e1,
    param_values = list(
      a = c(1, 2),
      b = c(3, 4)
    )
  )

  e2 <- nl_experiment(
    model_file = "no-file.nlogo",
    param_values = data.frame(
      a = c(1, 2, 1, 2),
      b = c(3, 3, 4, 4)
    )
  )

  expect_equal(unlist(e1) , unlist(e2))
})

