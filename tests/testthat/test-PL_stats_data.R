context("Check stats function")


test_that("Check PL_stats_data function",{
  test_stats_data <-
    PL_stats_data(team_data = pl_data,
                 date_from = "2016-08-09",
                 date_to = "2016-11-09",
                 x_stat = "goals_conc",
                 y_stat = "goals",
                 teams = NA,
                 x_stat_by = "no_div",
                 y_stat_by = "no_div")


  expect_equal(test_stats_data$main_label, "between 09 Aug '16 and 09 Nov '16")


  expect_equal(test_stats_data$stat1,
               c(11, 16, 15, 9, 19, 13, 24, 18, 14, 10, 13, 12, 12, 18, 21, 21, 6, 19, 15, 20))

  expect_equal(test_stats_data$stat2,
               c(24, 13, 11, 26, 16, 15, 10, 13, 30, 25, 16, 10, 12, 13, 9, 10, 15, 15, 12, 11))
})
