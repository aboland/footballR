#' Function to read in Premier League data
#'
#' This function retrieves data for Premier League matches inbetween the inputed dates.
#'
#' @param from Start date
#' @param to End date
#' @importFrom utils read.csv
#' @importFrom lubridate as_date dmy
#' @importFrom rlang .data
#'
#' @export

PL_read_data <-
  function(from, to){

  ########################
  #  Check the arguments
  ########################

    from <- as_date(from)
    if (is.na(from))
      stop("Incorrect date format 'from'")

    to <- as_date(to)
    if (is.na(to))
      stop("Incorrect date format 'to'")

  if (from > to)
    stop("End date must be after starting date")  # Check that the start date is before the end date

  #########################
  #  Choose which seasons
  #########################

  # Get the start season
  if (length(which(PremieRLeague::year_dates$start_date >= from)) > 0) {
    first_season <- min(which(PremieRLeague::year_dates$start_date >= from))
  }else{
    first_season <- 1
  }

  # Get the end season
  if (length(which(PremieRLeague::year_dates$end_date >= to)) > 0) {
    last_season <- min(which(PremieRLeague::year_dates$end_date >= to))
  }else{
    last_season <- nrow(PremieRLeague::year_dates)
  }

  years_to_get <- PremieRLeague::year_dates$years[first_season:last_season]  # Years to read in

  ########################
  #  Read in the seasons
  ########################

  output_data <- list()  # Blank list for data
  #Read in data
  for (i in 1:length(years_to_get)) {
    output_data[[i]] <-  tryCatch({
      data <-
        read.csv(
        paste0("http://www.football-data.co.uk/mmz4281/",years_to_get[i],"/E0.csv"),
        stringsAsFactors = F)
      print(paste0("Season: ", years_to_get[i], " read ok"))
      data
    },
    error = function(x)print(paste0("Failed to read season: ",years_to_get[i])),  # if error print out which season
    warning = function(x)print(paste0("Failed to read season: ",years_to_get[i]))  # if warning print out which season
    )
  }
  names(output_data) <- years_to_get  # change the names of the list




  ##################################
  #  Tidy up the dates
  ##################################

  # browser()
  # # The 02-03 season has a different date structure
  # if ("0203" %in% years_to_get) {
  #   # season0203 <- which(years_to_get == "0203")
  #   output_data[["0203"]]$Date <- as.Date(output_data[["0203"]]$Date, "%d/%m/%Y")
  # }
  #
  # # for(my_dmy in c("0203", "1718", "1819", "1920"))
  # if ("1718" %in% years_to_get)
  #   output_data[["1718"]]$Date <- as.Date(output_data[["1718"]]$Date, "%d/%m/%Y")
  #
  # if ("1819" %in% years_to_get)
  #   output_data[["1819"]]$Date <- as.Date(output_data[["1819"]]$Date, "%d/%m/%Y")


  for (i in 1:length(output_data)) {
    output_data[[i]]$Date <- dmy(output_data[[i]]$Date)
    output_data[[i]]$Referee <- gsub(pattern = "\xa0", "", output_data[[i]]$Referee)
  }

  ##########################
  #  Combine
  ##########################
  combined_data <-
    dplyr::bind_rows(output_data)

  # combined_data$Date <- as.Date(combined_data$Date, "%d/%m/%y")


  ##########################
  #  output
  ##########################

  combined_data$HomeTeam[which(combined_data$HomeTeam == "Middlesboro")] <- "Middlesbrough"
  combined_data$AwayTeam[which(combined_data$AwayTeam == "Middlesboro")] <- "Middlesbrough"

  combined_data <- combined_data[which(combined_data$Div != ""),]
  return(combined_data)

}
