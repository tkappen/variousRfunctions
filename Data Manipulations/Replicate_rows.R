library(data.table)

d.f <- data.frame(var1=1:2e3, var2=1:2e3, freq=1:2e3)
d.t <- as.data.table(d.f)

# a function that replicates rows according to 
# a frequency variable. Either a a column name (between "") can be passed
# or a numeric vector. When a column name is passed, this column is dropped
# from the dataset.
# A data.frame and data.table can be used. If l.dt = TRUE it will try to
# use data.table for faster calculation even when a data.frame is used
rep.rows <- function(data, freq, l.dt = TRUE) {
  if (!is.data.frame(data)) stop("Object is not an appropriate data object")

  if (l.dt) l.dt <- require(data.table)
  if (l.dt) data <- as.data.table(data)  

  f <- freq
  vars <- c(1:ncol(data))

  if(is.character(f)) {
    vars <- which(names(data) != f)
    f <- data[[f]]
  }

  if (l.dt) {
    data[rep(seq(nrow(data)), f), vars, with=FALSE]
  } else {
    data[rep(seq(nrow(data)), f), vars]
  }
}

# Create replicated ("weighted") dataset
d.weighted <- rep.rows(d.f, "freq", l.dt=F)

# Check performance
system.time(x <- rep.rows(d.f, "freq"))
system.time(x <- rep.rows(d.t, "freq"))

# And now when data.table is not available/being used
system.time(x <- rep.rows(d.f, "freq", l.dt=F))
