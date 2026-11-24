test_that("the app is installed with the package", {
  app_dir <- system.file("shiny", "acwr_app", package = "ACWR")
  expect_true(file.exists(file.path(app_dir, "app.R")))
})

test_that("run_app() names the missing packages", {
  local_mocked_bindings(is_installed = function(pkg) pkg != "plotly")
  expect_error(run_app(), "plotly")
  expect_error(run_app(), "install.packages\\(c\\(\"plotly\"\\)\\)")
})
