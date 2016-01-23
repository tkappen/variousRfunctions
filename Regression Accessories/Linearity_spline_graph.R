require(Hmisc)
require(rms)


#functie voor spline.plot.lrm met percentuele afwijking (door Y.Vergouwe/T.Kappen)
spline.plot.lrm <-function (xvar,fit1,fit2,knots,xlab,ylab)
{
#om een mooi plaatje te krijgen moet predictor gesorteerd worden
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

# oorspronkelijke plaatje
plot(x=dat[,1],y=dat[,2],type='l',xlab=xlab,ylab=ylab, frame.plot=F)
lines(dat[,1],dat[,3],lty=2)

# voeg een percentuele as toe (is eigenlijk geen nieuwe as, maar alleen vertaald in procenten)
axis(4, c(-1,-0.5,0,0.5,1),c("-100%","-50%","0%","50%","100%"), col= "blue",col.axis = "blue")
# plot verschil tussen de twee fits
lines(x=dat[,1], plogis(coef.1[1] + xtrans(dat[,1]))-0.5, type="l",col="blue")
# voeg lijn van geen verschil toe 
abline(h=0,lty=3,col="blue")
# voeg lijnen min/max verschil toe en waarden erbij
abline(h=max(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),lty=3,col="blue")
abline(h=min(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),lty=3,col="blue")

text(max(xvar), max(plogis(coef.1[1] + xtrans(dat[,1]))-0.5)+0.075*(max(dat[,2])-min(dat[,2])), paste("max:",signif(max(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),2)*100,"%"), cex = 1, col="blue", pos = 2, offset = 0)
text(max(xvar), min(plogis(coef.1[1] + xtrans(dat[,1]))-0.5)-0.075*(max(dat[,2])-min(dat[,2])), paste("min",signif(min(plogis(coef.1[1] + xtrans(dat[,1]))-0.5),2)*100,"%"), cex = 1, col="blue", pos = 2, offset = 0)
}

