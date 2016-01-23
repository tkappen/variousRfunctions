require(ggplot2)


lrm

x <- antiem.count~tiva
t <- terms(x, data=d)


test <- function (formula, data, var.penalty = c("simple", "sandwich")) 
{
	# Get the line with which the function was called
	call <- match.call()
	print(call)
	# Match argument in list of arguments in function
	# Returns error if not in list	
	# Looks not for exact pattern, parts (e.g. "sand") will also work
	var.penalty <- match.arg(var.penalty) 

	m <- match.call(expand.dots = FALSE)
	print (names(m))

	mc <- match(c("formula", "data"), 
		names(m), 0)
	m <- m[c(1, mc)]

	# as.name is required to get rid of the quotes
	m[[1]] <- as.name("model.frame")
	return(m)
}

tt <- test(x,d)
