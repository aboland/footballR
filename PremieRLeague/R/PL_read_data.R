#' Function to read in Premier League data
#'
#' This function retrieves data for Premier League matches inbetween the inputed dates.
#'
#' @param from Start date
#' @param to End date
#' @importFrom utils read.csv
#' @export

PL_read_data <-
  function(from, to){

  ########################
  #  Check the arguments
  ########################

  tryCatch({
    as.Date(from)
  },error=function(x)stop("Incorrect date format 'from'"))

  tryCatch({
    as.Date(to)
  },error=function(x)stop("Incorrect date format 'to'"))

  if(from>to)
    stop("End date must be after starting date")  # Check that the start date is before the end date

  #########################
  #  Choose which seasons
  #########################

  # Get the start season
  if(length(which(year_dates$start_date>=from))>0)
    first_season <- min(which(year_dates$start_date>=from))
  else
    first_season <- 1

  # Get the end season
  if(length(which(year_dates$end_date>=to))>0)
    last_season <- min(which(year_dates$end_date>=to))
  else
    last_season <- nrow(year_dates)

  years_to_get <- year_dates$years[first_season:last_season]  # Years to read in

  ########################
  #  Read in the seasons
  ########################

  output_data <- list()  # Blank list for data
  #Read in data
  for(i in 1:length(years_to_get)){
    output_data[[i]] <-  tryCatch({
      data <- read.csv(paste0("http://www.football-data.co.uk/mmz4281/",years_to_get[i],"/E0.csv"), stringsAsFactors = F)
      print(paste0("Season: ",years_to_get[i]," read ok"))
      data
    }, error = function(x)print(paste0("Failed to read season: ",years_to_get[i])),  # if error print out which season
    warning = function(x)print(paste0("Failed to read season: ",years_to_get[i]))  # if warning print out which season
    )
  }
  names(output_data) <- years_to_get  # change the names of the list

  ##################################
  #  Tidy up the dates and columns
  ##################################

  #common_columns <- lapply(output_data, names)
  common_columns <- names(output_data[[1]])
  if(length(output_data)>1){
    for(i in 2:length(output_data)){
      common_columns <- unique(common_columns, names(output_data[[i]]))
      #common_columns <- common_columns[which(common_columns %in% names(output_data[[i]]))]
    }
  }

  # The 02-03 season has a different date structure
  if("0203" %in% years_to_get){
    season0203 <- which(years_to_get=="0203")
    output_data[[season0203]]$Date<-as.Date(output_data[[season0203]]$Date,"%d/%m/%Y")
  }

  # Change dates to date format and add any missing columns
  output_data <- lapply(output_data, function(x){
    x$Date <- as.Date(x$Date,"%d/%m/%y")

    # If there are missing columns add them in with NA's
    if(sum(common_columns %in% names(x)) < length(common_columns)){
      missing_columns <- common_columns[which(!common_columns %in% names(x))]
      for(j in 1:length(missing_columns)){
        x[missing_columns[j]] <- NA
      }
    }
    return(x[,common_columns])
  })

  ##########################
  #  Combine and output
  ##########################

  combined_data <- do.call(rbind, output_data)  # Combine data

  combined_data$HomeTeam[which(combined_data$HomeTeam == "Middlesboro")] <- "Middlesbrough"
  combined_data$AwayTeam[which(combined_data$AwayTeam == "Middlesboro")] <- "Middlesbrough"

  combined_data <- combined_data[which(combined_data$Div!=""),]
  return(combined_data)

}
