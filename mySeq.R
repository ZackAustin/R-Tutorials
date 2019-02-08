# Create our own user-defined seq function while trying to match exact functionality.

options(scipen = 999, digits = 8)

mySeq <- function(from = 1, to = 1, by = ((to - from) / (length.out - 1)),
	length.out = NULL, along.with = NULL, ...)
{
	# Random ass case inputs
	if (length(match.call()) - 1 == 0)
	{
		return (c(1))
	}
	else if ( (from < to && by < 0) || (from > to && by > 0) )
	{
		return (paste ('Error in mySeq.default(', from, ', ', to, ', by = ',
			by, ', length.out = ', length.out, ') : wrong sign in "by" argument\n',
			sep = ''))
	}
	else if (!missing(length.out) && !missing(by))
	{
		return (paste ('Error in mySeq.default(', from, ', ', to, ', by = ',
			by, ', length.out = ', length.out, ') : too many arguments\n',
			sep = ''))
	}
	else if (length(match.call()) - 1 == 1)
	{
		# 17 would be a vector of 1:17 etc for seq. Type Check? Needs to also handle vector inputs. How do we differentiate this call from ...?
	}
	else if (!missing(length.out))
	{
		# update stepwise
		# stepwise_evaluations <- round(as.numeric(abs((to - from)) / (length.out - 1)), digits = 6 )
		stepwise_evaluations <- (to - from / (length.out - 1))
		if (stepwise_evaluations != 1)
			by = stepwise_evaluations
	}
	else if (!missing(by))
	{
		# currently fine
	}
	
	results <- c()
	stepwise_evaluations <- 1
	start = 1
	end = 1
	
	if ( (from < to || isTRUE(all.equal(from, to))) && by > 0)
	{
		start = from
		end = to
	}
	else
	{
		start = to
		end = from
	}

	if (start < end || isTRUE(all.equal(start, end)))
	{
		nextValue <- from
		results <- c(results, nextValue)
		while (start < end || isTRUE(all.equal(start, end)))
		{	
			if (by > 0)
			{
				start <- start + by
				nextValue <- start	
			}
			else
			{
				end <- end + by
				nextValue <- end
			}
			
			if (start < end || isTRUE(all.equal(start, end)))
				results <- c(results, nextValue)
		}
	}
		
	return (as.numeric(results) )
}

x <- mySeq

print (x)

cat ('test 1: ', mySeq(), '\n\n')

cat ('test 2: ', mySeq(2, 4, by = 0.05, length.out = 10), '\n\n')

mySeq(0, 1, length.out = 12)

mySeq(1, 0, length.out = 12)

cat ('test 3: ', mySeq(0, 1, length.out = 11), '\n\n')
# mySeq(stats::rnorm(20)) # effectively 'along'
# mySeq(1, 9, by = 2)     # matches 'end'
# mySeq(1, 9, by = pi)    # stays below 'end'
# mySeq(1, 6, by = 3)
# mySeq(1.575, 5.125, by = 0.05)
# mySeq(17) # same as 1:17, or even better seq_len(17)

