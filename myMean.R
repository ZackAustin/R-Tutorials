# Create our own user-defined mean function while trying to match functionality.

myMean <- function(x, trim = 0, na.rm = false, ...)
{
	# If x is not logical (coerced to numeric), numeric (including integer) or complex, NA_real_ is returned, with a warning.
	
	if ( class(x) != 'logical' && class(x) != 'numeric' && class(x) != 'integer' && class(x) != 'complex' )
	{
		# EX: [1] NA
		# 	Warning message:
		# 	In mean.default("hi") : argument is not numeric or logical: returning NA	
		# foo <- deparse(substitute(myMean))
		
		cat('Warning message:\nIn "', deparse(match.call()), '" : argument is not numeric or logical: returning NA\n', sep = '')
		return (NA_real_)
	}
	
	# na.rm means to strip values that aren't the correct class types in x.
	
	# perform mean
	
	results <- 0
	count = length(x)
	for (i in x)
	{
		#summation
		results = results + i
	}
	# mean
	results = results / count
	
	return (as.numeric(results))
}

x <- myMean

print (x)

# NOT RUN {
x <- c(0:10, 50)
cat ('\n', x, '\n')
xm <- myMean(x)
cat ('\n', xm, '\n')

xJUNK <- c("This is junk!!!!")
xFAIL <- myMean(xJUNK)
cat ('\n', xFAIL, '\n')

# xc <- c(xm, mean(x, trim = 0.10))
# print (xc)
# }
