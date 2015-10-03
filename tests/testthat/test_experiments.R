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


test_that("Parameter values generation - lhs, mc", {

  param_list <- list(


    nl_param_random(n = 5,
                    list(foo = c(1, 2), bar = c(100, 200), baz = 4),
                    FUN = function(n, k) matrix(runif(n*k), ncol = k) ),

    nl_param_lhs(n = 10, foo = c(1, 2), bar = c(100, 200), baz = 4),
    nl_param_lhs(n = 10, foo = 1, bar = c(100, 200), baz = c(1, 2, 3, 4)),
    nl_param_lhs( n = 10, data.frame(foo = c(1,0), bar = c(1,0), baz = 1)),
    nl_param_lhs( n = 10, data.frame(kaka = 1, foo = c(1,0))),

    nl_param_mc(n = 10, foo = c(1, 2), bar = c(100, 200), baz = 4),
    nl_param_mc(n = 10, foo = 1, bar = c(100, 200), baz = c(1, 2, 3, 4)),
    nl_param_mc( n = 10, data.frame(foo = c(1,0), bar = c(1,0), baz = 1)),
    nl_param_mc( n = 10, data.frame(bar = 1, foo = c(1,0))),
    nl_param_mc( n = 10, foo = c(1,0)),
    nl_param_mc( n = 10, list(foo = c(1,0)))
  )

  lapply(param_list, function(x) testthat::expect_is(x, "data.frame"))

})
