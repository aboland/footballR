
library(PremieRLeague)
setwd("PremieRLeague/")
here::dr_here()

year_dates <- data.frame(years = c("0001","0102","0203","0304",
                                   "0405","0506","0607","0708",
                                   "0809","0910","1011","1112",
                                   "1213","1314","1415","1516",
                                   "1617","1718", "1819", "1920"),
                         start_date = c("2000-08-19", "2001-08-18", "2002-08-17", "2003-08-16",
                                        "2004-08-14", "2005-08-13", "2006-08-19", "2007-08-11",
                                        "2008-08-16", "2009-08-15", "2010-08-14", "2011-08-13",
                                        "2012-08-18", "2013-08-17", "2014-08-16", "2015-08-08",
                                        "2016-08-13", "2017-08-11", "2018-08-10", "2019-08-09"),
                         end_date = c("2001-05-19", "2002-05-11", "2003-05-11", "2004-05-15",
                                      "2005-05-15", "2006-05-07","2007-05-13", "2008-05-11",
                                      "2009-05-24", "2010-05-09","2011-05-22", "2012-05-13",
                                      "2013-05-19", "2014-05-11","2015-05-24","2016-05-17",
                                      "2017-05-21", "2018-05-13", "2019-05-12", "2020-05-17"),
                         #start_date=c(seq(as.Date("31-07-00", "%d-%m-%y"),as.Date("31-07-17", "%d-%m-%y"),by="year")),
                         #end_date=c(seq(as.Date("01-05-01", "%d-%m-%y"),as.Date("01-05-18", "%d-%m-%y"),by="year")),
                         stringsAsFactors = F)

usethis::use_data(year_dates, overwrite = TRUE)
# save(year_dates, file = "data/year_dates.rda", compress = "xz")

team_cols <- data.frame(

  teams = c("Arsenal", "Aston Villa", "Birmingham", "Blackburn",
    "Blackpool", "Bolton", "Bournemouth", "Brighton",
    "Burnley", "Cardiff", "Charlton", "Chelsea",
    "Crystal Palace", "Derby", "Everton", "Fulham",
    "Huddersfield", "Hull", "Ipswich", "Leeds",
    "Leicester", "Liverpool", "Man City", "Man United",
    "Middlesbrough", "Newcastle", "Norwich",
    "Portsmouth", "QPR", "Reading", "Sheffield United",
    "Southampton", "Stoke", "Sunderland", "Swansea",
    "Tottenham", "Watford", "West Brom", "West Ham",
    "Wigan", "Wolves"),

  col1 = c("firebrick", "maroon4", "blue3", "white",
          "orange", "white", "black", "white",
          "maroon4", "blue3", "black", "royalblue4",
          "red", "white", "mediumblue", "white",
          "white", "orange", "white", "white",
          "blue3", "red", "lightskyblue", "red",
          "red", "white", "yellow",
          "white", "white", "white", "white",
          "white", "white", "white", "white",
          "white", "goldenrod2", "white", "maroon4",
          "white", "orange"),

  col2 = c("firebrick", "lightskyblue", "blue3", "blue3",
           "orange", "darkblue", "red", "blue",
           "lightskyblue", "blue3", "red", "royalblue4",
           "lightskyblue", "black", "mediumblue", "black",
           "blue", "black", "blue3", "yellow",
           "blue3", "red", "lightskyblue", "black",
           "red", "black", "green",
           "blue3", "blue3", "blue", "red",
           "red", "black", "red", "black",
           "navy", "black", "steelblue4", "lightskyblue",
           "blue3", "black"),


  col1_rgb = rgb(t(col2rgb(c("firebrick", "maroon4", "blue3", "white",
                            "orange", "white", "black", "white",
                            "maroon4", "blue3", "black", "royalblue4",
                            "red", "white", "mediumblue", "white",
                            "white", "orange", "white", "white",
                            "blue3", "red", "lightskyblue", "red",
                            "red", "white", "yellow",
                            "white", "white", "white", "white",
                            "white", "white", "white", "white",
                            "white", "goldenrod2", "white", "maroon4",
                            "white", "orange"))), maxColorValue = 255),

  col2_rgb = rgb(t(col2rgb(c("firebrick", "lightskyblue", "blue3", "blue3",
                             "orange", "darkblue", "red", "blue",
                             "lightskyblue", "blue3", "red", "royalblue4",
                             "lightskyblue", "black", "mediumblue", "black",
                             "lightskyblue", "black", "blue3", "yellow",
                             "blue3", "red", "lightskyblue", "black",
                             "red", "black", "green",
                             "blue3", "blue3", "blue", "red",
                             "red", "black", "red", "red",
                             "navy", "black", "steelblue4", "lightskyblue",
                             "blue3", "black"))), maxColorValue = 255),
  stringsAsFactors = F)

row.names(team_cols) <- c("Arsenal", "Aston Villa", "Birmingham", "Blackburn",
                          "Blackpool", "Bolton", "Bournemouth", "Brighton",
                          "Burnley", "Cardiff", "Charlton", "Chelsea",
                          "Crystal Palace", "Derby", "Everton", "Fulham",
                          "Huddersfield", "Hull", "Ipswich", "Leeds",
                          "Leicester", "Liverpool", "Man City", "Man United",
                          "Middlesbrough", "Newcastle", "Norwich",
                          "Portsmouth", "QPR", "Reading", "Sheffield United",
                          "Southampton", "Stoke", "Sunderland", "Swansea",
                          "Tottenham", "Watford", "West Brom", "West Ham",
                          "Wigan", "Wolves")

usethis::use_data(team_cols, overwrite = TRUE)


pl_data <- PL_read_data(from = "1999-12-01", to = "2019-05-12")

pl_data <- pl_data[,c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "FTR", "HTHG", "HTAG", "HTR",
                      "Attendance", "Referee", "HS", "AS", "HST", "AST", "HHW", "AHW", "HC", "AC", "HF", "AF",
                      "HO", "AO", "HY", "AY", "HR", "AR", "HBP", "ABP")]



usethis::use_data(pl_data, overwrite = TRUE)

# save(pl_data, file = "data/pl_data.rda", compress = "xz")
# write.csv(pl_data, file = "PL_Hist_00-19.csv", row.names = F)

