# Function to calculate appropriate BP values
# x = BP value, pp = pulse pressure, type = type of blood pressure: sys, dia, mean
convertBP <- function (x, pp = 40, type = "sys") {
	if (x <= 10 | x > 300) stop('Not a valid blood pressure variable')
	if (pp <= 10 | x > 300) stop('Not a valid blood pressure variable')
	y <- c(0,0,0)
	names(y) <- c("sys", "mean","dia")
	if (type == "sys") {
		y[1] <- x
		y[3] <- x - pp
		y[2] <- x - pp/3*2
		return(y)
	} else if (type == "dia") {
		y[3] <- x
		y[1] <- x + pp
		y[2] <- x + pp/3
		return(y)
	} else if (type == "mean") {
		y[2] <- x
		y[3] <- x - pp/3
		y[1] <- x + pp/3*2
		return(y)
	} else {
		stop('not a BP type')
	}
}


