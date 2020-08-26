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
               c(7, 7, 8, 12.5, 12, 4, 8, 7.5, 14.5, 10.5, 8, 2.5, 2, 9, 7, 10, 5, 6.5, 8.5, 6)
               # c(14, 14, 16, 25, 24, 8, 16, 15, 29, 21, 16, 5, 4, 18, 14, 20, 10, 13, 17, 12)
  )

  expect_equal(test_plot_data$y,
               c(12.5, 10, 6, 6, 4.5, 13.5, 4, 9.5, 5.5, 2.5, 8.5, 10.5, 16.5, 9.5, 3.5, 3.5, 9.5, 8, 6.5, 5.5)
               # c(25, 20, 12, 12, 9, 27, 8, 19, 11, 5, 17, 21, 33, 19, 7, 7, 19, 16, 13, 11)
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
               c(0.1207, 0.1373,  0.2759, 0.4310, 0.3750, 0.0597, 0.2000, 0.1415,
                 0.3452, 0.3500, 0.1509, 0.0410, 0.0225, 0.1268, 0.2059, 0.2000, 0.0877, 0.1413, 0.1932, 0.1132)
  )

  expect_equal(test_plot_data2$y,
               c(0.2315, 0.1538, 0.1579, 0.1818, 0.0978, 0.2109, 0.0727, 0.1377, 0.1250,
                 0.0568, 0.1491, 0.1780, 0.1897, 0.1462, 0.0603, 0.0614, 0.1900, 0.1270, 0.1250, 0.0821)
  )
})
