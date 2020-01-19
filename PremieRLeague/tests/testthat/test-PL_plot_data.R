context("Check plot function")


test_that("Check test_plot_data function",{
  test_plot_data <-
    PL_plot_data(team_data = pl_data,
                 date_from = "2018-08-09",
                 date_to = "2018-11-09",
                 x_stat = "goals_conc",
                 y_stat = "goals",
                 teams = NA,
                 x_stat_by = "no_div",
                 y_stat_by = "no_div",
                 custom_boundary = FALSE)


  expect_equal(test_plot_data$xlabels, "Goals conceded")


  expect_equal(test_plot_data$x,
               c(14, 14, 16, 25, 24, 8, 16, 15, 29, 21, 16, 5, 4, 18, 14, 20, 10, 13, 17, 12))

  expect_equal(test_plot_data$y,
               c(25, 20, 12, 12, 9, 27, 8, 19, 11, 5, 17, 21, 33, 19, 7, 7, 19, 16, 13, 11))
})
