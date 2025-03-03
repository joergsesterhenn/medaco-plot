larger_input_for_testing <- read_power_data(testthat::test_path("testdata2"))

test_that("plot by year renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by year",
    plot_by_year_bars(larger_input_for_testing)
  )
})

test_that("plot by month renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by month",
    plot_by_month_bars(larger_input_for_testing)
  )
})

test_that("plot by hour renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by hour",
    plot_by_hour_per_year_bars(larger_input_for_testing)
  )
})

test_that("plot by hour and month renders as before", {
  vdiffr::expect_doppelganger(
    "A plot aggregated by hour and month",
    plot_by_hour_per_month_bars(larger_input_for_testing)
  )
})

test_that("heatmap plot renders as before", {
  vdiffr::expect_doppelganger(
    "A heatmap plot",
    plot_by_hour_per_month_heatmap(larger_input_for_testing)
  )
})

test_that("ridgeline plot renders as before", {
  vdiffr::expect_doppelganger(
    "A ridgeline plot",
    plot_by_hour_per_month_ridgelines(larger_input_for_testing)
  )
})

test_that("top 10 plot renders as before", {
  vdiffr::expect_doppelganger(
    "A top 10 plot",
    plot_by_day_per_year_top_10_bars(larger_input_for_testing)
  )
})

test_that("line chart plot renders as before", {
  vdiffr::expect_doppelganger(
    "A line chart plot",
    plot_by_hour_per_month_lines(larger_input_for_testing)
  )
})

test_that("stacked area chart plot renders as before", {
  vdiffr::expect_doppelganger(
    "A stacked area plot",
    plot_by_hour_per_month_stacked_areas(larger_input_for_testing)
  )
})

test_that("First plot from plot_map renders as before", {
  vdiffr::expect_doppelganger(
    "First Plot in plot_map",
    plot(
      power_data = larger_input_for_testing,
      plot_type = "by year",
      display_mode = "light"
    )
  )
})

#' @importFrom rlang .data
test_that("calendar heatmap plot renders as before", {
  vdiffr::expect_doppelganger(
    "A calendar heatmap plot",
    withr::with_locale(
      new = c("LC_TIME" = "en_GB"),
      plot_by_day_per_year_calendar_heatmap(
        larger_input_for_testing,
        display_mode = "light"
      )
    )
  )
})
