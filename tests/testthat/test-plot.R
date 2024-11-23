larger_input_for_testing <- read_power_data(testthat::test_path("testdata2"))

test_that("plot by year renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by year",
    plot_aggregated_by_year(larger_input_for_testing)
  )
})

test_that("plot by month renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by month",
    plot_aggregated_by_month(larger_input_for_testing)
  )
})

test_that("plot by hour renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by hour",
    plot_aggregated_by_hour(larger_input_for_testing)
  )
})

test_that("plot by hour and month renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by hour and month",
    plot_by_hour_and_month(larger_input_for_testing)
  )
})

test_that("heatmap plot renders as before", {
  vdiffr::expect_doppelganger(
    "A heatmap plot",
    plot_heatmap(larger_input_for_testing)
  )
})

test_that("ridgeline plot renders as before", {
  vdiffr::expect_doppelganger(
    "A ridgeline plot",
    plot_ridgeline(larger_input_for_testing)
  )
})

test_that("top 10 plot renders as before", {
  vdiffr::expect_doppelganger(
    "A top 10 plot",
    plot_top_days(larger_input_for_testing)
  )
})

test_that("line chart plot renders as before", {
  vdiffr::expect_doppelganger(
    "A line chart plot",
    plot_line_chart(larger_input_for_testing)
  )
})

test_that("stacked area chart plot renders as before", {
  vdiffr::expect_doppelganger(
    "A stacked area plot",
    plot_stacked_area(larger_input_for_testing)
  )
})

test_that("First plot from plot_map renders as before", {
  vdiffr::expect_doppelganger(
    "First Plot in plot_map",
    plot(
      power_data = larger_input_for_testing,
      plot_type = "by year"
    )
  )
})

test_that("calendar heatmap plot renders as before", {
  vdiffr::expect_doppelganger(
    "A calendar heatmap plot",
    plot_calendar_heatmap(larger_input_for_testing)
  )
})
