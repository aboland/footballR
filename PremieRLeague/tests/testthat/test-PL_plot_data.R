context("Check plot function")


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
