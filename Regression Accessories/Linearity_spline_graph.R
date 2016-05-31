# ******************************************************************************************************************************
# Functie: spline.plot
# Function for (using Hmisc, rms) with percentual deviation 
# Original function by Y.Vergouwe. Further updates by T.Kappen
# Version: 1.3, May 4 2016
# ******************************************************************************************************************************

require(Hmisc)
require(rms)
require(ggplot2)

spline.plot <-function (x, y, data, knots, xlab = "x", ylab = "lp.spline", na.action = na.delete)
{	
    	m <- match.call(expand.dots = FALSE)
    	mc <- match(c("x", "y", "data", "na.action"), 
        names(m), 0)
     	m <- m[c(1, mc)]
   	m$na.action <- na.action
	
	d <- data.frame(x = eval(m$x, data),y = eval(m$y,data))
	d <- na.action(d)

	xvar	<- d$x	
	if (length(unique(d$y))> 2) 
		stop("y is not a binary variable")	

	fit1 <- lrm(y ~ rcs(x, knots), x=T, y=T, se.fit=T, data=d)
	fit2 <- lrm(y ~ x, x=T, y=T, se.fit=T, data=d)

	# Sort the predictors
	dat <-cbind(xvar,fit1$linear.predictors,fit2$linear.predictors)
	dat<-dat[order(xvar),]

	xx <- rcspline.eval(dat[,1], inclx=TRUE, nk=knots)
	xx.knots <- attr(xx, "knots")
	coef.1 <- fit1$coefficients
	coef.2 <- fit2$coefficients
	coef.1[1:length(coef.2)] <- coef.1[1:length(coef.2)] - coef.2
	options(digits=4)
	w <- rcspline.restate(xx.knots, coef.1[-1])
	xtrans <- eval(attr(w, "function"))	

	y.max <- max(dat[,2])
	y.min <- min(dat[,2])
	y.mean <- (y.max + y.min)/2	
	y.delta <- y.max - y.mean
	y.transl <- c(-1,-0.5,0,0.5,1)*y.delta + y.mean

	# Create graph
	plot(x=dat[,1],y=dat[,2],type='l',xlab=xlab,ylab=ylab, frame.plot=F)
	lines(dat[,1],dat[,3],lty=2)

	# translate axis in to percentages
	axis(4, y.transl,c("-100%","-50%","0%","50%","100%"), col= "blue",col.axis = "blue")
	# plot difference between the fits
	lines(x=dat[,1], plogis(coef.1[1] + xtrans(dat[,1]))-0.5+y.mean, type="l",col="blue")
	# line of no difference
	abline(h=y.mean,lty=3,col="blue")
	
	# add min/max lines with values
	abline(h=max(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),lty=3,col="blue")
	abline(h=min(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),lty=3,col="blue")
	text(max(xvar), max(plogis(coef.1[1] + xtrans(dat[,1]))-0.5)+0.075*(max(dat[,2])-min(dat[,2])), paste("max:",signif(max(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),2)*100,"%"), cex = 1, col="blue", pos = 2, offset = 0)
	text(max(xvar), min(plogis(coef.1[1] + xtrans(dat[,1]))-0.5)-0.075*(max(dat[,2])-min(dat[,2])), paste("min",signif(min(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),2)*100,"%"), cex = 1, col="blue", pos = 2, offset = 0)

}

decile.plot <- function (x, y, data, brkinclude = NULL, se = FALSE, 
	g = c(.01, .025, .05, .1, .25, .5, .75, .9, .95, .975, .99), 
	xlab = "Quantiles", ylab = "Proportion", na.action = na.delete, ...)
{
    	m <- match.call(expand.dots = FALSE)
    	mc <- match(c("x", "y", "data", "na.action"), 
        names(m), 0)
     	m <- m[c(1, mc)]
   	m$na.action <- na.action
	
	d <- data.frame(x = eval(m$x, data),y = eval(m$y,data))
	d <- na.action(d)

	if (length(unique(d$y))> 2) 
		stop("y is not a binary variable")	

	if (is.numeric(brkinclude)) {
		x.br <- d[d$x != brkinclude,]$x 
		brks <- c(brkinclude, quantile(x.br, g))

	} else { 
		brks <- quantile(d$x, g)
	}

	d$x_decile <- cut(d$x, brks, include.lowest = TRUE)

	deciles <- group_by(d, x_decile) %>%
    		summarize(n=n(),
			x_mean = mean(x),
        		y_sum = sum(y)) %>%
    		mutate(freq = y_sum / n )

	y.max <- ceiling(10*max(deciles$freq))/10	

	ggplot(deciles, aes(x=x_mean, y=freq)) + 
   		geom_smooth(method = "loess", se = se) + 
		geom_point(size = 1.5, shape = 23,
			 colour ="#777777", fill = "#AAAAAA",
			 stroke = 1.3) +
   		scale_x_continuous(name=xlab) +
   		scale_y_continuous(name=ylab, limits = c(0, y.max))

}
