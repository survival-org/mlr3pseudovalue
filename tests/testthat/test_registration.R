test_that("mlr3proba comes along with the package", {
  expect_true("package:mlr3proba" %in% search())
  expect_true(inherits(tsk("rats"), "TaskSurv"))
})

test_that("the pipeop dictionary picks up what the package registers", {
  register_pipeop("pv_dummy", function(...) NULL)
  on.exit({
    rm("pv_dummy", envir = mlr3pseudovalue_pipeops)
    mlr3pipelines::mlr_pipeops$remove("pv_dummy")
  })
  register_mlr3pipelines()
  expect_true("pv_dummy" %in% mlr3pipelines::mlr_pipeops$keys())
})
