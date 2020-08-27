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
               c(14, 14, 16, 25, 24, 8, 16, 15, 29, 21, 16, 5, 4, 18, 14, 20, 10, 13, 17, 12)
  )

  expect_equal(test_plot_data$y,
               c(25, 20, 12, 12, 9, 27, 8, 19, 11, 5, 17, 21, 33, 19, 7, 7, 19, 16, 13, 11)
  )



  test_plot_data2 <-
    PL_plot_data(team_data = pl_data,
                 date_from = "2018-08-09",
                 date_to = "2018-11-09",
                 x_stat = "goals_conc",
                 y_stat = "goals",
                 teams = NA,
                 x_stat_by = "p_shot_t",
                 y_stat_by = "p_corner",
                 custom_boundary = FALSE)

  expect_equal(test_plot_data2$x,
               c(0.2414, 0.2745, 0.5517, 0.8621, 0.7500, 0.1194, 0.4000, 0.2830, 0.6905, 0.7000,
                 0.3019, 0.0820, 0.0449, 0.2535, 0.4118, 0.4000, 0.1754, 0.2826, 0.3864, 0.2264)
  )

  expect_equal(test_plot_data2$y,
               c(0.4630, 0.3077, 0.3158, 0.3636, 0.1957, 0.4219, 0.1455, 0.2754, 0.2500, 0.1136,
                 0.2982, 0.3559, 0.3793, 0.2923, 0.1207, 0.1228, 0.3800, 0.2540, 0.2500, 0.1642)
  )
})
