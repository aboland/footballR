events <- c(as.POSIXct("2016-01-27 19:45:00", "GMT"), 
			as.POSIXct("2016-01-27 16:30:00", "GMT"))
event_labels <- c("Start", 
				"Goal test")


#setwd("~/Git/R/Betfair/BetfairEx")
load("~/Odds/ManCity_Everton_27Jan.RData")
plot.ts(as.numeric(odds_data[,1]))

plot(as.numeric(odds_data[,7]), as.numeric(odds_data[,1]), type="l")


times <- as.POSIXct(as.numeric(odds_data[,7]) - 3600, origin = "1970-01-01")
#times <- format(times,"%X") 


#pdf("timeseries_MC-Ev_27Jan.pdf")
par(mfrow=c(3,1))
plot(times, as.numeric(odds_data[,1]), 
		type="l", 
		main="Man City", 
		ylab="Odds",
		col="cornflowerblue", 
		ylim = range(as.numeric(odds_data[,1:2])))
lines(times, as.numeric(odds_data[,2]), col = "lightpink1")
legend("topright", c("Back", "Lay"), 
		col=c("cornflowerblue", "lightpink1"),
		lty=1)
abline(v = events, lty=2)
text(x = events, 
	y = range(as.numeric(odds_data[,1:2]))[1], 
	pos = 4,
	labels=event_labels)

plot(times, as.numeric(odds_data[,3]), 
		type="l", 
		main="Everton",
		ylab="Odds", 
		col="cornflowerblue", 
		ylim = range(as.numeric(odds_data[,3:4])))
lines(times, as.numeric(odds_data[,4]), col = "lightpink1")
legend("topright", c("Back", "Lay"), 
		col=c("cornflowerblue", "lightpink1"),
		lty=1)
abline(v = events, lty=2)
text(x = events, 
	y = range(as.numeric(odds_data[,3:4]))[1], 
	pos = 4,
	labels=event_labels)

plot(times, as.numeric(odds_data[,5]), 
		type="l", 
		main="Draw",
		ylab="Odds", 
		col="cornflowerblue", 
		ylim = range(as.numeric(odds_data[,c(5,6)])))
lines(times, as.numeric(odds_data[,6]), col = "lightpink1")
legend("topright", c("Back", "Lay"), 
		col=c("cornflowerblue", "lightpink1"),
		lty=1)
		abline(v = events, lty=2)
text(x = events, 
	y = range(as.numeric(odds_data[,1:2]))[1], 
	pos = 4,
	labels=event_labels)
#dev.off()

