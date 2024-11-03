test_that("plot by year renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by year",
    plot_aggregated_by_year(input_for_testing)
  )
})

test_that("plot by month renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by month",
    plot_aggregated_by_month(input_for_testing)
  )
})

test_that("plot by hour renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by hour",
    plot_aggregated_by_hour(input_for_testing)
  )
})

test_that("plot by hour and month renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by hour and month",
    plot_by_hour_and_month(input_for_testing)
  )
})

test_that("heatmap plot renders as before", {
  vdiffr::expect_doppelganger(
    "A heatmap plot",
    plot_heatmap(input_for_testing)
  )
})

test_that("ridgeline plot renders as before", {
  vdiffr::expect_doppelganger(
    "A ridgeline plot",
    plot_ridgeline(input_for_testing)
  )
})

test_that("top 10 plot renders as before", {
  vdiffr::expect_doppelganger(
    "A top 10 plot",
    plot_top_days(input_for_testing)
  )
})
